package org.everpeace.start_scalaz

import java.util.Date
import scalaz._
import Scalaz._
import org.everpeace.start_scalaz._1_SemiGroupExercise.CodesInSlides.Point
import org.everpeace.start_scalaz._2_MonoidExercise.CodesInSlides.Rational

object _5_OrderExercise {

  def main(args: Array[String]): Unit = {
    CodesInSlides
    Exercise
  }

  object  CodesInSlides {

    // Equal Instance
    implicit object PointInstance extends Equal[Point] {
      def equal(p1: Point, p2: Point) = p1 == p2
    }

    // systax for Equal
    assert(Point(2, 3) === Point(2, 3))
    assert(Point(2, 3) =/= Point(3, 5))

    // plain scala expression can be compiled.
    // but scalaz operator is type safe.
    1 == "geso"
    /* 1 === "geso" */ // compile error
    1 + 1.5
    /* 1 |+| 1.5 */ // compile error


    // Order Instance
    // It defines relations between rationals.
    implicit object RationalInstance extends Order[Rational] {
      // order returns 'Ordering'
      def order(r1: Rational, r2: Rational) = r1.n * r2.d -> r2.n * r1.d match {
        case (m, n) if m == n => Ordering.EQ
        case (m, n) if m < n => Ordering.LT
        case (m, n) if m > n => Ordering.GT
      }
    }

    // sytax for Order.
    assert(Rational(1, 2) === Rational(1, 2))
    assert(Rational(1, 2) < Rational(3, 4))
    assert(Rational(5, 2) >= Rational(5, 3))
    assert(Rational(5,2) ?|? Rational(5,3) === Ordering.GT)

    // Ordering is Monoid.
    mzero[Ordering] assert_=== Ordering.EQ
    (Ordering.EQ: Ordering) |+| Ordering.LT assert_=== Ordering.LT
    (Ordering.GT: Ordering) |+| Ordering.LT assert_=== Ordering.GT
    (Ordering.GT: Ordering) |+| Ordering.EQ assert_=== Ordering.GT


    case class Person(name: String, age: Int, height: Int)
    object Person {
      // Order Instance for Person
      // we can easily create lexicographical order because append of Ordering is dominated by the first ordering.
      implicit object PersonInstance extends Show[Person] with Order[Person] {
        def show(p: Person) = p.toString.toList
        def order(p1: Person, p2: Person) =
          p1.age ?|? p2.age |+| p1.height ?|? p2.height
      }
    }

    val miku = Person("miku", 16, 158)
    val rin = Person("rin", 14, 152)
    val len = Person("len", 14, 156)
    List(miku, rin, len) sorted Order[Person].toScalaOrdering assert_=== List(rin, len, miku)
  }

  object Exercise {
    // 1.Define Order Instance for java.util.Date
    implicit object DateOrder extends Order[Date]{
      def order(x: Date, y: Date) = Ordering.fromInt(x.compareTo(y))
    }

    // 2.Define Order Instance for Student.
    //   Note that the order is lexicographical order of grade and birthday
    case class Student(name: String, grade: Int, birthday: Date)
    implicit object StudentOrder extends Order[Student]{
      def order(x: Student, y: Student) = x.grade ?|? y.grade |+| x.birthday ?|? y.birthday
    }
    implicit val StudentShow = Show.showA[Student]

    val format = new java.text.SimpleDateFormat("yyyy MM dd")
    val akari = Student("akari", 1, format.parse("1995 07 24"))
    val kyoko = Student("kyoko", 2, format.parse("1995 03 28"))
    val yui = Student("yui", 2, format.parse("1994 04 22"))
    val chinatsu = Student("chinatsu", 1, format.parse("1995 11 06"))
    List(akari, kyoko, yui, chinatsu) sorted Order[Student].toScalaOrdering assert_=== List(akari, chinatsu, yui, kyoko)
  }
}
