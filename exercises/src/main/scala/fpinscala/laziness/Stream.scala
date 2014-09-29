package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = if (n <= 0) Empty else this match {
    case Empty => Empty
    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], n: Int): Stream[A] = if (n <= 0) s else s match {
      case Empty => Empty
      case Cons(h, t) => go(t(), n-1)
    }
    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => if (!(p(a))) false else b)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def take2(n: Int): Stream[A] = if (n <= 0) empty else unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(h(), t().take2(n-1))
  }

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, t) => if (p(h())) Some((h(), t().takeWhile3(p))) else None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
  }

  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s).filter { case (x1, x2) => x1 != x2 }.headOption match {
    case None => true
    case Some((_, None)) => true
    case _ => false
  }

  def tails: Stream[Stream[A]] = unfold((this, true)) {
    case (_, false) => None
    case (Empty, _) => Some((Empty, (Empty, false)))
    case (s@Cons(h, t), _) => Some((s, (t(), true)))
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def go(x1: Int, x2: Int): Stream[Int] = Stream.cons(x1, go(x2, x1 + x2))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  val ones2: Stream[Int] = unfold(1)(a => Some(a, a))
  def from2(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))
  def constant2[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))
  def fibs2: Stream[Int] = unfold((0,1)) { case (x1, x2) => Some(x1, (x2, x1+x2)) }
}
