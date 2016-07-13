package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = {
   this match {
     case Left(l) => Left(l)
     case Right(r) => Right(f(r))
   }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(l) => Left(l)
     case Right(r) => f(r)
   }
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(l) => b
     case Right(r) => Right(r)
   }
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   this flatMap { aa => b map { bb => f(aa, bb) } }
 }

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil:List[B])
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
  }

  /**
   * 注意这里
   * Line 61中的             f(h).map2(t)(_ :: _)
   * 对比 traverse中的变化     (f(h) map2 traverse(t)(f))(_ :: _)
   * foldRight 实际上就是自动调用了 traverse(t)(f)
   * @param es
   * @param f
   * @tparam E
   * @tparam A
   * @tparam B
   * @return
   */
  def traverse_2[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight[Either[E, List[B]]](Right(Nil:List[B]))((h, t) => f(h).map2(t)(_ :: _))
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    traverse(es)(x => x)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }


}
