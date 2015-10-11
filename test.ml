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

let test_sonrs test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  let ofx = Ofx.parse_ofx file in 
  match ofx with
  | Some o -> OUnit2.assert_equal 0 o.signonmsgsrsv1.sonrs.status_sonrs.code ~msg:"SONRS: Status code does not match" ~printer:string_of_int
	    ; OUnit2.assert_equal "INFO" o.signonmsgsrsv1.sonrs.status_sonrs.severity ~msg:"SONRS: Status severity does not match"
	    ; OUnit2.assert_equal "ENG" o.signonmsgsrsv1.sonrs.language  ~msg:"SONRS: Language does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal "20150215114612.074[+0]" o.signonmsgsrsv1.sonrs.dtserver ~msg:"SONRS: Dtserver does not match"
  | None -> OUnit2.assert_bool "parse_ofx failed to parse the file" false

let test_stmttrnrs test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  let ofx = Ofx.parse_ofx file in 
  match ofx with
  | Some o -> OUnit2.assert_equal "A" o.bankmsgsrsv1.stmttrnrs.trnuid ~msg:"STMTTRNRS: Tranuid code does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal 0 o.bankmsgsrsv1.stmttrnrs.status.code  ~msg:"STMTTRNRS:Status: Code does not match" ~printer:string_of_int
	    ; OUnit2.assert_equal "INFO" o.bankmsgsrsv1.stmttrnrs.status.severity ~msg:"STMTTRNRS:Status: Severity does not match" ~printer:(fun x -> x)
  | None -> OUnit2.assert_bool "parse_ofx failed to parse the file" false

let test_stmtrs test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  let ofx = Ofx.parse_ofx file in 
  match ofx with
  | Some o -> OUnit2.assert_equal "GBP" o.bankmsgsrsv1.stmttrnrs.stmtrs.curdef ~msg:"STMTRS:Curdef code does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal "0" o.bankmsgsrsv1.stmttrnrs.stmtrs.bankacctfrom.bankid  ~msg:"BANKACCTFROM: Bankid does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal "****00185" o.bankmsgsrsv1.stmttrnrs.stmtrs.bankacctfrom.acctid  ~msg:"BANKACCTFROM: Acctid does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal "CHECKING" o.bankmsgsrsv1.stmttrnrs.stmtrs.bankacctfrom.accttype  ~msg:"BANKACCTFROM: Accttype does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal 2.46 o.bankmsgsrsv1.stmttrnrs.stmtrs.ledgerbal.balamt  ~msg:"LEDGERBAL: Balamt does not match" ~printer:string_of_float
	    ; OUnit2.assert_equal "20150117120000.000[+0]" o.bankmsgsrsv1.stmttrnrs.stmtrs.ledgerbal.dtasof  ~msg:"LEDGERBAL: Dtasof does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal 2.46 o.bankmsgsrsv1.stmttrnrs.stmtrs.availbal.balamt  ~msg:"AVAILBAL: Balamt does not match" ~printer:string_of_float
	    ; OUnit2.assert_equal "20150117120000.000[+0]" o.bankmsgsrsv1.stmttrnrs.stmtrs.availbal.dtasof  ~msg:"AVAILBAL: Dtasof does not match" ~printer:(fun x -> x)

  | None -> OUnit2.assert_bool "parse_ofx failed to parse the file" false





let suite = 
  "suite" >::: ["Test to see if the file is there" >:: test_open_file;
	        "Test to see if the file can be parsed" >:: test_parse_file;
	        "Test to see if STMTTRNRS has been been parsed correctly" >:: test_stmttrnrs;
	        "Test to see if STMTRS has been been parsed correctly" >:: test_stmtrs;
	        "Test to see if SONRS has been been parsed correctly" >:: test_sonrs]
let () =
  run_test_tt_main suite
