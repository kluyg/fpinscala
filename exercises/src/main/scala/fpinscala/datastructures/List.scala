package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("tail of empty list")
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n-1)

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(tail(l), f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("init of empty list")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, x) => x+1) 

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x+1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, acc) => Cons(x, acc))

  def appendAll[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])((x, acc) => append(x, acc))

  def add1(xs: List[Int]): List[Int] = foldRight(xs, Nil: List[Int])((x, acc) => Cons(x+1, acc))

  def d2s(xs: List[Double]): List[String] = foldRight(xs, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((x, acc) => append(f(x), acc))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) Cons(x, Nil) else Nil)

  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumLists(t1, t2))
    case (Nil, Nil) => Nil
    case _ => throw new Exception("sumLists was called with lists that have different length")
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1,l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    case (Nil, Nil) => Nil
    case _ => throw new Exception("zipWith was called with lists that have different length")
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith(l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(ph, pt)) if h == ph => startsWith(t, pt)
      case _ => false
    }

    if (sup == Nil) false else
    if (startsWith(sup, sub)) true else
    hasSubsequence(tail(sup), sub)
  }
}
