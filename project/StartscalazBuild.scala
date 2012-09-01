import sbt._
import sbt.Keys._

object StartscalazBuild extends Build {

  lazy val startscalaz = Project(
    id = "start-scalaz",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "start-scalaz",
      organization := "org.everpeace",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0-M6"
      // add other settings here
    )
  )
}
