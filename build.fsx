#!tools/FAKE/tools/FAKE.exe
#r @"tools/FAKE/tools/FakeLib.dll"
open Fake

RestorePackages()

let buildDir = "./build/"
let guiDir = "./gui/"
let testDir  = "./test/"

Target "Clean" (fun _ ->
  CleanDirs [buildDir; guiDir; testDir]
)

Target "BuildApp" (fun _ ->
  !! "src/app/**/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)

Target "BuildGUI" (fun _ ->
  !! "src/gui/**/*.fsproj"
    |> MSBuildRelease guiDir "Build"
    |> Log "BuildGui-Output: "
)

Target "BuildTest" (fun _ ->
  !! "src/test/**/*.fsproj"
    |> MSBuildDebug testDir "Build"
    |> Log "BuildTest-Output: "
)

Target "Test" (fun _ ->
  !! (testDir + "/*.dll")
    |> NUnit (fun p ->
        {p with
           DisableShadowCopy = true;
           OutputFile = testDir + "TestResults.xml" })
)

Target "Default" (fun _ ->
  trace "Default Target."
)

"Clean"
  ==> "BuildApp"
  ==> "BuildGui"
  ==> "BuildTest"
  ==> "Test"
  ==> "Default"

RunTargetOrDefault "Default"
