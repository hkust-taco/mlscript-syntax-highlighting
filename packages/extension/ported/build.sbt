import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "ported",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("ported")))
    },
    
    Compile / fullLinkJS / crossTarget := baseDirectory.value / ".." / ".." / "ported",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.1",
  )
