name := "ScalaLab4"

version := "0.1"

scalaVersion := "3.0.2"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"