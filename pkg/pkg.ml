#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ofx" @@ fun c ->
  Ok [ Pkg.mllib "src/ofx.mllib";
       Pkg.test ~dir:"test" "test/test" ]
