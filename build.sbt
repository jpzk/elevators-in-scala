cancelable in Global := true

lazy val commonSettings = Seq(
  organization := "mwt.hc",
  version := "1.0.0",
  scalaVersion := "2.12.1",
  description := "hc",
  organizationHomepage := Some(url("http://www.madewithtea.com"))
    )

// Testing
lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += scalaTest 
