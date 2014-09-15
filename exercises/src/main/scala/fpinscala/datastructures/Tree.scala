package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(tree: Tree[A], acc: Int): Int = tree match {
      case Leaf(_) => 1 + acc
      case Branch(l, r) => go(l, acc + 1) max go(r, acc + 1)
    }

    go(tree, 0)
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A,B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = tree match {
    case Leaf(v) => lf(v)
    case Branch(l,r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + l + r)
  
  def maximum2(tree: Tree[Int]): Int = fold(tree)(x => x)((l, r) => l max r)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + (l max r))

  def map2[A,B](tree: Tree[A])(f: A =>B): Tree[B] = fold(tree)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
}
