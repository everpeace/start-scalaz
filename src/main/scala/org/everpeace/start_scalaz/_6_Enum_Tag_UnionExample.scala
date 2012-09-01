package org.everpeace.start_scalaz

import scalaz._
import Scalaz._

import org.everpeace.start_scalaz._2_MonoidExercise.CodesInSlides.Rational

object _6_Enum_Tag_UnionExample extends App{

  // Enum is Order with pred and succ.
  // Enum Instance for Rational.
  implicit object RationalInstance extends Enum[Rational] {
    def order(r1: Rational, r2: Rational) = r1.n * r2.d -> r2.n * r1.d match {
      case (m, n) if m == n => Ordering.EQ
      case (m, n) if m < n => Ordering.LT
      case (m, n) if m > n => Ordering.GT
    }
    def succ(r: Rational) = r.copy(n = r.n + r.d)
    def pred(r: Rational) = r.copy(n = r.n - r.d)
  }

  // Sytax for Enum.
  // .succ and .pred is provided to Enum.
  1.succ assert_=== 2
  Rational(1, 2).succ assert_=== Rational(3, 2)
  'b'.pred assert_=== 'a'
  Rational(1, 2).pred assert_=== Rational(-1, 2)


  // Tagged Type.
  sealed trait Author
  sealed trait Title
  case class Book(title: String @@ Title, author: String @@ Author)

  val book = Book(Tag("Programming in Scala"), Tag("Martin Odersky"))
  /* book.copy(title = book.author) */ // compile error

  import scalaz.Tags._
  3 |+| 3 assert_=== 6
  (Multiplication(3) |+| Multiplication(3): Int) assert_=== 9
  (Conjunction(true) |+| Conjunction(false): Boolean) assert_=== false
  (Disjunction(true) |+| Disjunction(false): Boolean) assert_=== true
  import scalaz.Dual._
  (Dual("hello") |+| Dual("world"): String) assert_=== "worldhello"


  import scalaz.UnionTypes._
  // Union Types.
  def size[A](a: A)(implicit ev: A Contains t[Int]#t[String]#t[List[_]]) = a match {
    case i: Int => i
    case s: String => s.length
    case l: List[_] => l.size
  }
  size(1) assert_=== 1
  size("geso") assert_=== 4
  size(List(1, 2, 3)) assert_=== 3
  /* size(1L) */ // compile error
}
