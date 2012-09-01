package org.everpeace.start_scalaz

import scalaz._
import Scalaz._

object _2_MonoidExercise {

  def main(args: Array[String]): Unit = {
    CodesInSlides
    Exercise
  }

  object CodesInSlides {

    case class Rational(n: Int, d: Int) {
      def +(r: Rational) = Rational(n * r.d + r.n * d, d * r.d)
    }

    object Rational {

      // Monoid instance.
      // Monoid = Semigroup with unit element.
      implicit object RationalMonoid extends Monoid[Rational] {
        def zero = Rational(0, 1)

        def append(r1: Rational, r2: => Rational) = r1 + r2
      }

      // required for assert_===
      implicit object RationalEqual extends Equal[Rational] {
        def equal(a1: Rational, a2: Rational) = a1.n / a1.d.asInstanceOf[Double] === a2.n / a2.d.asInstanceOf[Double]
      }

      implicit val RationalShow = Show.showA[Rational]

    }

    mzero[Int] assert_=== 0
    mzero[Option[String]] assert_=== None
    mzero[Rational] assert_=== Rational(0, 1)
    Rational(0, 1) |+| Rational(1, 2) assert_=== Rational(1, 2)
    Rational(1, 3) |+| Rational(1, 3) assert_=== Rational(2, 3)

    // Monoid Law
    val a = 1
    mzero[Int] |+| a assert_=== a
    a |+| mzero[Int] assert_=== a

    // Useful Monoid functions
    // replicate
    Monoid.replicate[List, Int](0)(3, 1 |+| _) assert_=== List(0, 1, 2)
    // unfold
    Monoid.unfold[List, List[Int], Int](List(1, 2, 3)) {
      case Nil => None
      case x :: xs => Some(x * 2 -> xs)
    } assert_=== List(2, 4, 6)
  }

  object Exercise {
    // 1.Define a function extracting n even numbers from even number sequence.
    def evens(n: Int): List[Int] = Monoid.replicate[List, Int](0)(n, 2 |+| _)

    evens(5) assert_=== List(0, 2, 4, 6, 8)

    // 2.Define a function encoding the given number from decimal to binary.
    def encode(n: Int): List[Int] = Monoid.unfold[List, Int, Int](n) {
      case a: Int if a > 0 => (a % 2 -> a / 2).some
      case a: Int => none
    }

    encode(13) assert_=== List(1, 0, 1, 1)

  }

}
