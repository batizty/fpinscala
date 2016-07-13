package fpinscala.laziness

import java.awt.peer.SystemTrayPeer

import Stream._

import scala.collection.mutable.ListBuffer

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

  /**
    * 5.1
    * 在大规模数据条件下会发生stack over flow
    * @return
    */
  def toList: List[A] = {
    this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }

  def toList1: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }
    go(this, List.empty).reverse // 这里一定要有reverse
  }

  def toListFast: List[A] = {
    val buf = new ListBuffer[A]()
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buf += (h()); go(t())
      case Empty => buf.toList
    }
    go(this)
  }

  /**
    * 5.2
    * @param n
    * @return
    */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => t().take(n - 1)
      case Cons(h, t) if n == 1 => t()
      case _ => Empty
    }
  }


  def drop2(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => t().take(n - 1)
      case _ => this
    }
  }

  /**
    * 5.3
    * @param p
    * @return
    */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) =>
        val hh = h()
        if (p(hh))
          cons(hh, t().takeWhile(p))
        else
          cons(hh, empty)
      case _ => Empty
    }
  }

  /**
    * 5.4
    * @param p
    * @return
    */
  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) || t().forAll(p)
      case _ => true
    }
  }

  /**
    * 5.5
    * takeWhile through foldRight
    * @return
    */
  def takeWhileThroughFoldRight(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((hh, t) => if (p(hh)) cons(hh, t) else empty)
  }


  /**
    * 5.6
    * @return
    */
  def headOption: Option[A] = {
    foldRight[Option[A]](None)((hh, t) => Some(hh))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((hh, t) => cons(f(hh), t))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((hh, t) => if (p(hh)) cons(hh, t) else t)
  }

//  def append[B >: A](e: B): Stream[B] = {
//    foldRight(cons(e, empty))((hh, t) => cons(hh, t))
//  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }



  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}