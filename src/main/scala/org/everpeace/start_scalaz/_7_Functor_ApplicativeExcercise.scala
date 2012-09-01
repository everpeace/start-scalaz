package org.everpeace.start_scalaz

import scalaz._
import Scalaz._
import language.higherKinds

object _7_Functor_ApplicativeExcercise{

  def main(args: Array[String]): Unit = {
    CodesInSlides
    Exercise
  }

  implicit def VectorEaual[A] = Equal.equalA[Vector[A]]
  implicit def VectorShow[A] = Show.showA[Vector[A]]

  object CodesInSlides {
    {
      // Functor: what can be `map`ped.
      object vector{
        implicit object VectorFunctor extends Functor[Vector] {
          def map[A, B](v: Vector[A])(f: A => B) = v map f
        }
      }
      // fdouble is a function which double contents inside in F.
      def fdouble[F[_] : Functor, A: Semigroup](fa: F[A]) = fa.map((a:A) => a |+| a)
      fdouble(List(1, 2, 3)) assert_=== List(2, 4, 6)
      fdouble("geso".some) assert_=== Some("gesogeso")
      import vector._
      fdouble(Vector(1.2, 2.1)) assert_=== Vector(2.4, 4.2)

      // functor must be satisfied functor law
      // map(fa)(id) = fa
      // map(map(fa)(f))(g) = map(fa)(g compose f)
      val fa = List(1, 2)
      lazy val f: Int => Int = _ + 2
      lazy val g: Int => Int = _ * 2
      fa map (x => x) assert_=== fa
      fa map f map g assert_=== (fa map g <<< f)
    }

    {
      // Point: Functor which can be lifted from any object.
      object vector {
        implicit object VectorPointed extends Pointed[Vector] {
          def map[A, B](v: Vector[A])(f: A => B) = v map f

          def point[A](a: => A) = Vector(a)
        }
      }

      // sytax for point.
      1.point[List] assert_=== List(1)
      1.point[Option] assert_=== Some(1)
      import vector._
      1.point[Vector] assert_=== Vector(1)
    }

    {
      // Apply: Functor with ap
      // ap: which takes two inputs(lifted object(va) and lifted function(vab))
      //     and apply the object inside va to the function inside vab and return lifted object.
      object vector {
        implicit object VectorApply extends Apply[Vector] {
          def map[A, B](v: Vector[A])(f: A => B) = v map f
          def ap[A, B](va: => Vector[A])(vab: => Vector[A => B]) = vab flatMap (va map _)
        }
      }

      // syntax for Apply
      Option(0) <*> Option(Enum[Int].succ _) assert_=== Option(1)
      List(1, 2, 3) <*> PlusEmpty[List].empty[Int => Int] assert_=== Nil
      import vector._
      Vector(1, 2) <*> Vector(Enum[Int].succ _, Enum[Int].pred _) assert_=== Vector(2, 3, 0, 1)
    }

    {
      // Applicative = Pointed Apply
      object vector {
        implicit object VectorApplicative extends Applicative[Vector] {
          def point[A](a: => A) = Vector(a)
          def ap[A, B](va: => Vector[A])(vab: => Vector[A => B]) = vab flatMap (va map _)
        }
      }

      // Applicative must be Applicative Law.
      // ap(fa)(point((a: A) => a)) == fa
      // ap(ap(fa)(fab))(fbc) == ap(fa)(ap(fab)(ap(fbc)(point((bc: B => C) => (ab: A => B) => bc compose ab))))
      // ap(point(a))(point(ab)) == point(ab(a))
      // ap(point(a))(fab) == ap(fab)(point((f: A => B) => f(a)))
      val a = 0
      val fa = Option(a)
      val ab: Int => String  = _.toString
      lazy val fab: Option[Int => String] = Option(_.toString)
      lazy val fbc: Option[String => Int] = Option(_.size)
      fa <*> ((a: Int) => a).point[Option] assert_=== fa
      fa <*> fab <*> fbc assert_=== fa <*> (fab <*> (fbc <*> (((bc: String => Int) => (ab: Int => String) => bc compose ab).point[Option])))
      a.point[Option] <*> ab.point[Option] assert_=== ab(a).point[Option]
      a.point[Option] <*> fab assert_=== fab <*> ((f: Int => String) => f(a)).point[Option]

      // Applicative Builder
      // can generate A=>B=>C to F[A]=>F[B]=>F[C]
      def append3[F[_] : Apply, A: Semigroup](fa: F[A], fb: F[A], fc: F[A]) =
        (fa |@| fb |@| fc)(_ |+| _ |+| _)

      append3(Option(1), Option(2), Option(3)) assert_=== Option(6)
      append3(Option(1), None, Option(3)) assert_=== None
      append3(List(1), List(1, 2), List(1, 2, 3)) assert_=== List(3, 4, 5, 4, 5, 6)
    }
  }

  object Exercise {

    case class User(id: String, pass: String)
    implicit val UserEqual = Equal.equalA[User]
    implicit val UserShow = Show.showA[User]

    // Define a function 'user'
    //   which takes a map m as an input
    //   and construct User by extracting 'id' and 'pass'.
    def user(m: Map[String, String]): Option[User]
    = (m.get("id") |@| m.get("pass"))(User)

    user(Map("id" -> "halcat0x15a", "pass" -> "gesogeso")) assert_=== Some(User("halcat0x15a", "gesogeso"))
    user(Map.empty) assert_=== None
  }

}
