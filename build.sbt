import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

This / logo := "Probabilistic Playground (v" + version.value + ")"
This / usefulTasks := Seq(
  UsefulTask("theta", "project theta", "use the Theta subproject"),
  UsefulTask("tuple", "project tuple", "use the Tuple subproject"),
  UsefulTask("", "code", "Launch VSCode")
)

val commonSettings = List(
  version      := "0.0.1",
  scalaVersion := "3.2.2",
  organization := "net.andimiller",
  libraryDependencies ++= Seq(
    "io.indigoengine" %%% "tyrian-io"   % "0.5.1",
    "org.scodec"      %%% "scodec-core" % "2.2.1",
    "org.scodec"      %%% "scodec-bits" % "1.1.37",
    "org.scalameta"   %%% "munit"       % "0.7.29" % Test
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  scalaJSLinkerConfig ~= {
    _.withModuleKind(ModuleKind.CommonJSModule)
  },
  scalafixOnCompile := true,
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  autoAPIMappings   := true,
  code := {
    val command = Seq("code", ".")
    val run = sys.props("os.name").toLowerCase match {
      case x if x contains "windows" => Seq("cmd", "/C") ++ command
      case _                         => command
    }
    run.!
  },
  logo := "Probabilistic Playground (v" + version.value + ")",
  usefulTasks := Seq(
    UsefulTask("fast", "fastOptJS", "Rebuild the JS (use during development)"),
    UsefulTask(
      "full",
      "fullOptJS",
      "Rebuild the JS and optimise (use in production)"
    ),
    UsefulTask("", "code", "Launch VSCode")
  ),
  logoColor        := scala.Console.MAGENTA,
  aliasColor       := scala.Console.BLUE,
  commandColor     := scala.Console.CYAN,
  descriptionColor := scala.Console.WHITE
)

lazy val probabilisticplayground =
  aggregateProjects(theta)

lazy val theta = (project in file("module/theta"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "theta"
  )
  .settings(commonSettings: _*)

lazy val tuple = (project in file("module/tuple"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "tuple"
  )
  .settings(commonSettings: _*)

lazy val code =
  taskKey[Unit]("Launch VSCode in the current directory")
