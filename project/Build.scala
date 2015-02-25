import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization  := "unstable.build",
    version       := "0.1-SNAPSHOT",
    scalaVersion  := "2.11.5",
    scalacOptions ++= Seq(
      "-Xlog-free-terms",
      "-Xexperimental",
      "-unchecked",
      "-deprecation",
      "-Xlint",
      "-Ywarn-dead-code",
      "-target:jvm-1.7",
      "-encoding", "UTF-8"
    ),
    licenses      := ("Apache2", new java.net.URL("http://www.apache.org/licenses/LICENSE-2.0.txt")) :: Nil
  )
}

object MacrosBuild extends Build {
  import BuildSettings._

  lazy val macros: Project = Project(
    "macros",
    file("."),
    settings = buildSettings
  ) aggregate(macrosImpl, examples)

  lazy val macrosImpl: Project = Project(
    "macrosImpl",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <++= (scalaVersion) { scalaV: String =>
        Seq(
          "org.scala-lang" % "scala-compiler" % scalaV,
          "com.novus" %% "salat" % "1.9.9"
        )})
  )

  lazy val examples: Project = Project(
    "examples",
    file("examples"),
    settings = buildSettings
  ) dependsOn(macrosImpl)
}
