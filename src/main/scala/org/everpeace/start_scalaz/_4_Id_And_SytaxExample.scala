package org.everpeace.start_scalaz

import scalaz._
import Scalaz._

object _4_Id_And_SytaxExample extends App{
  // Id Monad.
  ("geso": Id[String]) assert_=== "geso"

  // IdOps: syntax for all types.
  1.some assert_=== Some(1)
  none[Int] assert_=== None
  assert(1.right[String] === Right(1))
  assert("geso".left[Int] === Left("geso"))

  // argument applications to function.
  1 |> (1 + _) assert_=== 2
  "geso" |> (_.size) assert_=== 4
  0 |> Show[Int].shows assert_=== "0"
}
