import org.ensime.sbt.Plugin.Settings.ensimeConfig

import org.ensime.sbt.util.SExp._

name := "scatur"

version := "0.1"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Ydependent-method-types")

ensimeConfig := sexp(key(":compiler-args"), sexp("-Ydependent-method-types"), key(":formatting-prefs"), sexp(key(":alignParameters"), true ) )
