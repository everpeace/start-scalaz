package org.everpeace.start_scalaz
import scalaz._
import Scalaz._
import org.everpeace.start_scalaz._2_MonoidExercise.CodesInSlides.Rational

object _3_GroupExample extends App{

  // Group = Monoid with ∀e has an inverse element.
  implicit object RationalGroup extends Group[Rational] {
    def zero = implicitly[Monoid[Rational]].zero
    def append(r1: Rational, r2: => Rational) = implicitly[Monoid[Rational]].append(r1, r2)
    def inverse(r: Rational) = Rational(-r.n, r.d)
  }

  1.inverse assert_=== -1
  Rational(1, 2).inverse assert_=== Rational(-1, 2)


  // Plus: universally quantified Semigroup.
  // PlusEmpty: universally quantified Monoid.
  // <+> is operator for Plus.
  // ex. List[_] can be `plus`ed by concatenation.
  List(1, 2) |+| List(3, 4) assert_=== List(1, 2, 3, 4)
  List(1, 2) <+> List(3, 4) assert_=== List(1, 2, 3, 4)
  Option(1) |+| Option(1) assert_=== Option(2)
  Option(1) <+> Option(1) assert_=== Option(1)

  object vector {
    implicit object VectorInstance extends PlusEmpty[Vector] {
      def empty[A] = Vector.empty[A]
      def plus[A](v1: Vector[A], v2: => Vector[A]) = v1 ++ v2
    }
  }

  import vector._
  assert(Vector(1, 2) <+> Vector(3, 4) == Vector(1, 2, 3, 4))

}
