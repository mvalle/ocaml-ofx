open Xml
open XmlParser

type status=  {code : int; 
	       severity : string}

type sonrs = {status_sonrs : status; 
	      dtserver : string; 
	      language : string}

type signonmsgsrsv1 = {sonrs : sonrs}

type bankacctfrom = {bankid : string;
		     acctid : string;
		     accttype : string}

type stmttrn = {trntype : string;
		dtposted : string;
		trnamt : float;
		fitid : string;
		name : string}

type banktranlist = {dtstart : string;
		     dtend : string;
		     stmttrn : stmttrn list;}

type balance = {balamt : float;
		 dtasof : string;}

type stmtrs = {curdef : string;
	       bankacctfrom : bankacctfrom;
	       banktranlist : banktranlist;
	       ledgerbal : balance;
	       availbal : balance}

type stmttrnrs = {trnuid : string;
		  status : status;
		  stmtrs : stmtrs}


type bankmsgsrsv1 = {stmttrnrs : stmttrnrs}



type ofx = { signonmsgsrsv1 : signonmsgsrsv1;
	     bankmsgsrsv1 : bankmsgsrsv1;}


let rec find_child_by_tag_helper xml_list child_tag =
  if List.length xml_list = 0 
  then None
  else if (Xml.tag (List.hd xml_list) = child_tag)
  then Some (List.hd xml_list)
  else find_child_by_tag_helper (List.tl xml_list) child_tag 

let find_child_by_tag xml_element child_tag =
  find_child_by_tag_helper (Xml.children xml_element) child_tag

let rec find_children_by_tag_helper xml_list child_tag =
  if List.length xml_list = 0
  then []
  else if (Xml.tag (List.hd xml_list) = child_tag)
  then match find_children_by_tag_helper (List.tl xml_list) child_tag with
       | [] -> [List.hd xml_list]
       | res -> List.append [(List.hd xml_list)] res
  else find_children_by_tag_helper (List.tl xml_list)  child_tag

(*if List.length xml_list = 0
  then None
  else if (Xml.tag (List.hd xml_list) = child_tag)
  then match find_children_by_tag_helper (List.tl xml_list) child_tag with
       | Some res -> Some (List.append [(List.hd xml_list)] res)
       | None -> Some [(List.hd xml_list)]
  else find_children_by_tag_helper (List.tl xml_list) child_tag*)


let find_children_by_tag xml_element child_tag =
  find_children_by_tag_helper (Xml.children xml_element) child_tag

let get_value_by_tag xml_element child_tag = 
    match (find_child_by_tag xml_element child_tag) with
    | Some e -> Xml.pcdata (List.hd (Xml.children e))
    | None -> ""


let parse_STATUS xml = 
  match xml with 
  | Some el -> {code = int_of_string (get_value_by_tag el "CODE");
		severity = get_value_by_tag el "SEVERITY"}
  | None -> {code = -1; severity = ""}


let parse_STMTTRN el = 
  {trntype = get_value_by_tag el "TRNTYPE";
   dtposted = get_value_by_tag el "DTPOSTED";
   trnamt = float_of_string (get_value_by_tag el "TRNAMT");
   fitid = get_value_by_tag el "FITID";
   name = get_value_by_tag el "NAME"}

let parse_STMTTRNs xml =
  match xml with
  | Some el -> List.map parse_STMTTRN (find_children_by_tag el "STMTTRN")
  | None -> []


let parse_BANKTRANLIST xml = 
  match xml with 
  | Some el -> {dtstart = get_value_by_tag el "DTSTART";
		dtend = get_value_by_tag el "DTEND";
		stmttrn = parse_STMTTRNs xml}
  | None -> {dtstart = "";
	     dtend = "";
	     stmttrn = parse_STMTTRNs None}

let parse_BANKACCTFROM xml = 
  match xml with 
  | Some el -> {bankid = get_value_by_tag el "BANKID";
		acctid = get_value_by_tag el "ACCTID";
		accttype = get_value_by_tag el "ACCTTYPE"}
  | None -> {bankid = ""; acctid = ""; accttype = ""}
			       
let parse_balance xml = 
  match xml with 
  | Some el -> {balamt = float_of_string (get_value_by_tag el "BALAMT");
		dtasof = (get_value_by_tag el "DTASOF")}
  | None -> {balamt = 0.; dtasof = ""}



let parse_STMTRS xml = 
  match xml with
  | Some el -> {curdef = get_value_by_tag el "CURDEF";
		bankacctfrom = parse_BANKACCTFROM (find_child_by_tag el "BANKACCTFROM");
		banktranlist = parse_BANKTRANLIST (find_child_by_tag el "BANKTRANLIST");
		ledgerbal = parse_balance (find_child_by_tag el "LEDGERBAL");
		availbal = parse_balance (find_child_by_tag el "AVAILBAL")}
  | None -> {curdef = "";
	     bankacctfrom = parse_BANKACCTFROM None;
	     banktranlist = parse_BANKTRANLIST None;
	     ledgerbal = parse_balance None;
	     availbal = parse_balance None}

let parse_STMTTRNRS xml =
  match xml with 
  | Some el -> {trnuid = get_value_by_tag el "TRNUID";
		status = parse_STATUS (find_child_by_tag el "STATUS");
		stmtrs = parse_STMTRS (find_child_by_tag el "STMTRS")}
  | None -> {trnuid = "";
	     status = parse_STATUS None;
	     stmtrs = parse_STMTRS None}

let parse_BANKMSGSRSV1 xml =
  match xml with
  | Some el -> {stmttrnrs = parse_STMTTRNRS (find_child_by_tag el "STMTTRNRS")}
  | None -> {stmttrnrs = parse_STMTTRNRS None}

let parse_SONRS xml = 
  match xml with 
  | Some el -> {status_sonrs = parse_STATUS (find_child_by_tag el "STATUS");
		dtserver = get_value_by_tag el "DTSERVER";
		language = get_value_by_tag el "LANGUAGE"}
  | None -> {status_sonrs = parse_STATUS  xml;
		dtserver = "";
		language = ""}

let parse_SIGNONMSGRSRV1 xml = 
  match xml with
  | Some el ->  {sonrs = parse_SONRS (find_child_by_tag el "SONRS")}
  | None ->   {sonrs = parse_SONRS xml}

let parse_ofx xml = 
  match Xml.tag xml with
  | "OFX" ->  Some {signonmsgsrsv1 = parse_SIGNONMSGRSRV1 (find_child_by_tag xml "SIGNONMSGSRSV1");
		bankmsgsrsv1 = parse_BANKMSGSRSV1 (find_child_by_tag xml "BANKMSGSRSV1") }
  | _ -> None
