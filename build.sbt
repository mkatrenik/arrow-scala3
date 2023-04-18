//ThisBuild / scalaVersion := "3.3.0-RC3"
scalaVersion := "3.3.1-RC1-bin-20230304-c610f18-NIGHTLY"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.0.5",
  "ch.qos.logback" % "logback-classic" % "1.4.6",
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

scalacOptions := Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-deprecation",
  "-Yexplicit-nulls"
)

javaOptions ++= Seq(
  "--enable-preview",
  "--add-modules",
  "jdk.incubator.concurrent"
)
