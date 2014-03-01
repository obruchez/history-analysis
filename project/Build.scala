import sbt._
import sbt.Keys._

object ProjectBuild extends Build {
  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "history-analysis",
      organization := "org.bruchez",
      version := "0.1",
      scalaVersion := "2.10.3",
      libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.9"
      // add other settings here
    )
  )
}
