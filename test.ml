open OUnit2

let test_open_file test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  match file with
  | f -> OUnit2.assert_bool "Data file opend and pares fine" true
  | _ -> OUnit2.assert_bool "Data file failed to open and parse" false

let test_parse_file text_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  let _ofx = Ofx.parse_ofx file in
  match _ofx with  
  | Some _ -> OUnit2.assert_bool "parse_ofx parsed the file" true
  | None -> OUnit2.assert_bool "parse_ofx failed to parse the file" false


let suite = 
  "suite" >::: ["Test to see if the file is there" >:: test_open_file;
	       "Test to see if the file can be parsed" >:: test_parse_file]

let () =
  run_test_tt_main suite
