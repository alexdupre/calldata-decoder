ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "calldata-decoder",
    libraryDependencies ++= List(
      "com.eed3si9n" %% "gigahorse-okhttp" % "0.6.0",
      "com.typesafe.play" %% "play-json" % "2.9.2",
      "org.web3j" % "core" % "4.9.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    )
  )
