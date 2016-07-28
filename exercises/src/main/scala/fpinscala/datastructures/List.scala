package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("List l is Empty")
      case Cons(h, t) => t
    }
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("List l is Empty, could run setHead")
      case Cons(h0, t) => Cons(h, t)
    }
  }

  // 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n-1)
    }
  }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t, f)
      case _ => l
    }
  }

  // 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("List l is Empty")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("List l is Empty")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  // 3.9
  def length[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(h, t) => 1 + length(t)
    }
  }

  def length2[A](l: List[A]): Int =
    foldRight[A, Int](l, 0)((_, acc) => acc + 1)

  def length3[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(h, t) => 1 + length3(t)
    }
  }

  // 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z,h))(f)
    }
  }

  // 3.11
  def sumViaFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def productViaFoldLeft(l: List[Double]): Double = {
    foldLeft(l, 1.0)(_ * _)
  }

  // 3.12
  def reverseList[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buff = new ListBuffer[A]
    def go(cur: List[A]): List[A] = cur match {
      case Nil => List(buff.toList: _*)
      case Cons(h, t) => buff.append(h); go(t)
    }
    go(l)
  }
  def reverseList2[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h: A) => Cons(h, acc))

    /**
     * l match {
     * case Nil => List[A]()
     * case Cons(h, t) => foldLeft(t, f(List[A](), h)(f)
     * }
     * f = (b: B, a: A) => a :: b
     * f2 = (b: B, a: A) => Cons(a, b)
     *
     */
  }

  // 3.13
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f:(A, B) => B): B = {
    foldLeft(reverseList(l), z)((b: B, a: A) => f(a, b))
  }

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverseList(l), z)((b, a) => f(a, b))
  }

  // 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)((h, acc) => Cons(h, acc))
  }

  // 3.15
  def concat[A](l: List[List[A]]) : List[A] = {
    foldRight(l, List[A]())(appendViaFoldRight)
  }

  // 3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((h, t) => Cons(h + 1, t))

  // 3.17
  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, List[String]())((d, t) => Cons(d.toString, t))
  }

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((h, t) => Cons(f(h), t))
  }
}
