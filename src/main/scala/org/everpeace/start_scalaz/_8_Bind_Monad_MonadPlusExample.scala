package org.everpeace.start_scalaz

import scalaz._
import Scalaz._
import language.higherKinds

object _8_Bind_Monad_MonadPlusExample extends App {

  implicit def VectorEaual[A] = Equal.equalA[Vector[A]]
  implicit def VectorShow[A] = Show.showA[Vector[A]]

  {
    // Bind: F[A] => (A=>F[B]) => F[B]
    implicit object VectorInstance extends Bind[Vector] {
      def map[A, B](v: Vector[A])(f: A => B) = v map f
      def bind[A, B](v: Vector[A])(f: A => Vector[B]) = v flatMap f
    }
    // we can for comprehension for Bind.
    def append3[F[_] : Bind, A: Semigroup](fa: F[A], fb: F[A], fc: F[A]) =
      for {
        a <- fa
        b <- fb
        c <- fc
      } yield a |+| b |+| c
    append3(Vector(1), Vector(2), Vector(3)) assert_=== Vector(6)

    // for comprehension is just only syntax suger.
    // for is expanded to flatmap, filter, map.
    (for (a <- List(1, 2)) yield a + 1) assert_=== List(1, 2).map(a => a + 1)
    (for (a <- Option(1); b <- Option(2)) yield a + b) assert_=== Option(1).flatMap(a => Option(2).map(b => a + b))
    (for (a <- List(1, 2) if a % 2 == 0) yield a) assert_=== List(1, 2).filter(a => a % 2 == 0)
  }

  {
    // Monad at last.
    // Monad is Applicative Bind.
    object vector {
      implicit object VectorInstance extends Monad[Vector] {
        def point[A](a: => A) = Vector(a)
        def bind[A, B](v: Vector[A])(f: A => Vector[B]) = v flatMap f
      }
    }
    // Monad must be satisfied Monad Law
    // fa >>= point(_) = fa
    // point(a) >>= f  = f(a)
    // fa >>= f >== g  = fa >>= (a => f(a) >>= g)
    import scala.util.control.Exception._
    val a = 1
    val fa = Option(a)
    lazy val f: Int => Option[String] = _.toString |> Option.apply
    lazy val g: String => Option[Int] = allCatch opt _.toInt
    (fa >>= (_.point[Option])) assert_=== fa
    (a.point[Option] >>= f) assert_=== f(a)
    (fa >>= f >>= g) assert_=== (fa >>= (a => f(a) >>= g))
  }

  {
    // ApplicativePlus is Applicative and PlusEmpty
    object vector {
      implicit object VectorInstance extends ApplicativePlus[Vector] {
        def empty[A] = Vector.empty[A]
        def plus[A](v1: Vector[A], v2: => Vector[A]) = v1 ++ v2
        def point[A](a: => A) = Vector(a)
        def ap[A, B](va: => Vector[A])(vab: => Vector[A => B]) = vab flatMap (va map _)
      }
    }

    {
      // MonadPlus is Monad and PlusEmpty.
      object vector {
        implicit object VectorInstance extends MonadPlus[Vector] {
          def empty[A] = Vector.empty[A]
          def plus[A](v1: Vector[A], v2: => Vector[A]) = v1 ++ v2
          def point[A](a: => A) = Vector(a)
          def bind[A, B](v: Vector[A])(f: A => Vector[B]) = v flatMap f
        }
      }

      // filter is provided to MonadPlus
      def evens[F[_]: MonadPlus](f: F[Int]) = f filter (_ % 2 === 0)
      evens(List(1, 2, 3)) assert_=== List(2)
      evens(Option(1)) assert_=== None
      import vector._
      evens(Vector(1, 2, 3)) assert_=== Vector(2)
    }
  }
}
