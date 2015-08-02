open XmlParser
open Xml
 

let ofxextractor ofx = ofx

let extractor filename =
    let file = Xml.parse_file filename in
    match Xml.tag file with
    | "OFX" -> Some (ofxextractor file)
    | _ -> None


    

let rec find_child_by_tag xml_list child_tag =
    if List.length xml_list = 0 
    then None
    else if (Xml.tag (List.hd xml_list) = child_tag)
    then Some (List.hd xml_list)
    else find_child_by_tag (List.tl xml_list) child_tag 
				  

	   

(*OFX->BANKMSGSRSV1->STMTTRNRS->STMTRS->BANKTRANLIST->STMTTRN*)
