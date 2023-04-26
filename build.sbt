organization := "io.github.mkatrenik"
name := "arrow-scala"
version := "0.1-SNAPSHOT"

scalaVersion := "3.3.0-RC4"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.0.5",
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

scalacOptions := Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-deprecation",
  "-Yexplicit-nulls"
)

fork := true

javaOptions ++= Seq(
  "--enable-preview",
  "--add-modules",
  "jdk.incubator.concurrent"
)
