package fpinscala.state

import scala.collection.mutable.ListBuffer


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

  /**
    * 6.1
    * @param rng
    * @return
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
//    val r = rng.nextInt
//    if (r._1 >= 0) r
//    else (0 - r._1, r._2)

    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
    * 6.2
    * @param rng
    * @return
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ( if (i == Int.MaxValue) 0.0 else (i.toDouble / Int.MaxValue), r)
  }

  /**
    * 6.3
    * @param rng
    * @return
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((d, i), r) = intDouble(rng)
    ((i, d), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * 6.4
    * @param count
    * @param rng
    * @return
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var r1 = rng
    val buffer = new ListBuffer[Int]
    for { i <- 0.until(count) } yield {
      val (i, r) = r1.nextInt
      buffer += i
      r1 = r
    }
    (buffer.toList, r1)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    val buffer = new ListBuffer[Int]
    def loop(n: Int, r: RNG): (List[Int], RNG) = {
      if (n > 0) {
        val (i, r0) = r.nextInt
        buffer += i;
        loop(n - 1, r0)
      } else (buffer.toList, r)
    }
    loop(count, rng)
  }

  def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty, rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints3(count - 1)(r1)
      (x :: xs, r2)
    }
  }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
