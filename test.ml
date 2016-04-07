open OUnit2
open Ofx

let test_open_file test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  match file with
  | _ -> OUnit2.assert_bool "Data file opend and pares fine" true

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
  | Some o -> OUnit2.assert_equal 0 o.signonmsgsrsv1.sonrs.status.code ~msg:"SONRS: Status code does not match" ~printer:string_of_int
	    ; OUnit2.assert_equal "INFO" o.signonmsgsrsv1.sonrs.status.severity ~msg:"SONRS: Status severity does not match"
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

let test_banktranlist test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  let ofx = Ofx.parse_ofx file in 
  match ofx with
  | Some o -> OUnit2.assert_equal "20150115114544.730[+0]" o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.dtstart ~msg:"BANKTRANLIST: Dtstart does not match" ~printer:(fun x -> x)
	    ; OUnit2.assert_equal "20150215114544.730[+0]" o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.dtend ~msg:"BANKTRANLIST: Dtend code does not match" ~printer:(fun x -> x)
  | None -> OUnit2.assert_bool "parse_ofx failed to parse the file" false


let test_stmttrn test_ctxt = 
  let file = Xml.parse_file "test_data.ofx" in
  let ofx = Ofx.parse_ofx file in
  match ofx with 
  | Some o -> OUnit2.assert_equal 6 (List.length o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn) ~msg:"BANKTRANLIST: Stmttrn length deos not match" ~printer:string_of_int;
              OUnit2.assert_bool "Checking STMTTRN 1" 
				 (List.exists (fun s -> print_string (string_of_float s.trnamt);
(String.compare s.trntype "XFER") == 0  &&
							(String.compare s.dtposted "20150117120000.000[+0]") == 0 &&
							(compare s.trnamt 600.) == 0 && 
							(String.compare s.fitid "00XFER2015011712000000000600007123430609010Credit16January2015") == 0 &&
							(String.compare s.name "071234 30609010 Credit 16 January 2015" == 0))
				 o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn);
	      OUnit2.assert_bool "Checking STMTTRN 2" 
				 (List.exists (fun s -> (String.compare s.trntype "XFER") == 0  &&
							(String.compare s.dtposted "20150117120000.000[+0]") == 0 &&
							(compare s.trnamt (-250.0)) == 0 && 
							(String.compare s.fitid "00XFER201501171200000000-2500JOEBLOGGS") == 0 &&
							(String.compare s.name "JOE BLOGGS" == 0))
				 o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn);
	      OUnit2.assert_bool "Checking STMTTRN 3" 
				 (List.exists (fun s -> (String.compare s.trntype "XFER") == 0  &&
							(String.compare s.dtposted "20150117120000.000[+0]") == 0 &&
							(compare s.trnamt (-370.0)) == 0 &&
							(String.compare s.fitid "00XFER201501171200000000-3700JOEBLOGGS") == 0 &&
							(String.compare s.name "JOE BLOGGS" = 0))
				 o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn);
	      OUnit2.assert_bool "Checking STMTTRN 4" 
				 (List.exists (fun s -> (String.compare s.trntype "CREDIT") == 0  &&
							(String.compare s.dtposted "20150119120000.000[+0]") == 0 &&
							(compare s.trnamt 1.0) == 0 &&
							(String.compare s.fitid "00CREDIT20150119120000000010TransferfromSMITHMJ") == 0 &&
							(String.compare s.name "Transfer from SMITH M J" == 0))
				 o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn);
	      OUnit2.assert_bool "Checking STMTTRN 5" 
				 (List.exists (fun s -> (String.compare s.trntype "XFER") == 0  &&
							(String.compare s.dtposted "20150119120000.000[+0]") == 0 &&
							(compare s.trnamt (-600.0)) == 0 &&
							(String.compare s.fitid "00XFER201501191200000000-6000SMITHSDIAMONDS") == 0 &&
							(String.compare s.name "SMITH S DIAMONDS" == 0))
				 o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn);
	      OUnit2.assert_bool "Checking STMTTRN 6" 
				 (List.exists (fun s -> (String.compare s.trntype "XFER") == 0  &&
							(String.compare s.dtposted "20150216120000.000[+0]") == 0 &&
							(compare s.trnamt 40.0) == 0 &&
							(String.compare s.fitid "00XFER20150216120000000040007024664259927Credit15February2015") == 0 &&
							(String.compare s.name "070246 64259927 Credit 15 February 2015" == 0))
				 o.bankmsgsrsv1.stmttrnrs.stmtrs.banktranlist.stmttrn)

  | None -> OUnit2.assert_bool "parse_ofx failed to parse the file" false




let suite = 
  "suite" >::: ["Test to see if the file is there" >:: test_open_file;
	        "Test to see if the file can be parsed" >:: test_parse_file;
	        "Test to see if STMTTRNRS has been been parsed correctly" >:: test_stmttrnrs;
	        "Test to see if STMTRS has been been parsed correctly" >:: test_stmtrs;
	        "Test to see if BANKTRANLIST has been been parsed correctly" >:: test_banktranlist;
	        "Test to see if SONRS has been been parsed correctly" >:: test_sonrs;
		"Test to see if STMTTRNs have been parsed correctly" >:: test_stmttrn
	       ]
let () =
  run_test_tt_main suite
