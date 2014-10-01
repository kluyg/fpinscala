package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val absN = if (n == Int.MinValue) 0 else math.abs(n)
    (absN, rng2)
  }

  val double = map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng)
    val (d3, rng3) = double(rng)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = if (count <= 0) (acc, rng) else {
      val (i, r) = rng.nextInt
      go(count - 1, r, i :: acc)
    }
    go(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)
    (f(a,b), rngB)
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))(map2(_,_)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    val (b, r2) = g(a)(r1)
    (b, r2)
  }

  def nonNegativeLessThen(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThen(n)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def go(machine: Machine, input: Input): Machine = (machine, input) match {
      case (m, Coin) if m.candies > 0 => Machine(false, m.candies, m.coins + 1)
      case (m, Turn) if !m.locked => Machine(true, m.candies - 1, m.coins)
      case (m, Coin) => Machine(m.locked, m.candies, m.coins + 1)
      case (m, _) => m
    }
    State[Machine, (Int, Int)] { m =>
      val machine = inputs.foldLeft(m) { (m, input) => go(m, input) }
      ((machine.coins, machine.candies), machine)
    }
  }

  def unit[S, A](a: A): State[S,A] = State[S,A](s => (a, s))
  def sequence[S,A](ss: List[State[S,A]]): State[S,List[A]] = ss.foldRight(unit[S,List[A]](Nil))((x, acc) => x.map2(acc)(_ :: _))
}
