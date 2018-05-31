open Rresult

module BC = struct
  open OByteLib

  type secdata =
    | CODE of Code.t
    | DLPT of Dlpt.t
    | DLLS of Dlls.t
    | PRIM of Prim.t
    | DATA of Data.t
    | SYMB of Symb.t
    | CRCS of Crcs.t
    | DBUG of Dbug.t
    | Unknown of {
        name : string ;
        data : string ;
      }

  let from_bytefile
      {Bytefile.
        version; vmpath = _; vmarg = _; index = _;
        extra; data; prim; code; dlpt; dlls; 
        crcs; dbug; symb } =
    let l = [
      CODE code;
      DLPT dlpt; DLLS dlls; 
      PRIM prim; DATA data;
      SYMB symb;
      CRCS crcs; DBUG dbug; 
    ]
    in
    version, extra, l
    

  let write_secdata_raw v oc section : Section.t = match section with
    | CODE x -> Code.write v oc x ; CODE
    | DLPT x -> Dlpt.write oc x ; DLPT
    | DLLS x -> Dlls.write oc x ; DLLS
    | PRIM x -> Prim.write oc x ; PRIM
    | DATA x -> Data.write oc x ; DATA
    | SYMB x -> Symb.write oc x ; SYMB
    | CRCS x -> Crcs.write oc x ; CRCS
    | DBUG x -> Dbug.write oc x ; DBUG
    | Unknown { data ; name } -> output_string oc data ; Unknown name

  let write_secdata v oc secdata =
    let offset = pos_out oc in
    let section = write_secdata_raw v oc secdata in
    let length = pos_out oc - offset in
    Index.({ section; offset; length })

  let write_secdatas v oc secdatas =
    let rec aux = function
      | [] -> []
      | sec :: l ->
        let i = write_secdata v oc sec in
        i :: aux l
    in
    aux secdatas

  let writeshebang oc s = Printf.fprintf oc "#!%s\n" s

  let write_oc ~oc ~version ?shebang ?(extra=Extra.empty) secdatas = 
    begin match shebang with
      | Some s -> writeshebang oc s
      | None -> ()
    end;
    Extra.write oc extra;
    let indices = write_secdatas version oc secdatas in
    Index.write oc indices ;
    Version.write oc version ;
    flush oc

  let write ~filename ~version ?shebang ?(extra=Extra.empty) secdatas =
    let oflags = [ Open_wronly; Open_creat; Open_trunc; Open_binary ] in
    let oc =
      try open_out_gen oflags 0o751 filename
      with _ -> failwith @@ Printf.sprintf "fail to open file %S for writting" filename
    in
    try
      write_oc ~oc ~version ?shebang ~extra secdatas ;
      close_out oc ;
    with
    | Failure msg ->
      close_out oc;
      failwith @@ Printf.sprintf  "fail to write bytecode file %S (%s)" filename msg
    | exn ->
      close_out oc;
      failwith @@ Printf.sprintf  "fail to write bytecode file %S (internal error: %s)" filename (Printexc.to_string exn)

end

module PdfAnnot = struct

  let dict l = Pdf.Dictionary l
  let stream d s =
    let b = Pdfio.bytes_of_string s in
    let l = Pdfio.bytes_size b in    
    Pdf.Stream ( ref ( dict (("/Length", Pdf.Integer l) :: d), Pdf.Got b) )

  (* let get_first_page pdf = 
   *   let document_catalog =
   *     try Pdf.lookup_obj pdf pdf.Pdf.root with
   *       Not_found -> raise (Pdf.PDFError "/Root entry is incorrect")
   *   in 
   *   let pages =
   *     Pdf.lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
   *   in
   *   match Pdf.lookup_direct pdf "/Type" pages with
   *   | Some (Pdf.Name "/Pages") ->
   *     begin match Pdf.lookup_fail "No /Kids in page tree" pdf "/Kids" pages with
   *       | Pdf.Array (p :: _) -> p
   *       | _ -> raise (Pdf.PDFError "Malformed /Kids in page tree node")
   *     end
   *   | _ -> raise (Pdf.PDFError "find_pages: Not a page tree node or page object") *)

  let add_annotation pdf page obj =
    let rest = page.Pdfpage.rest in
    let new_annots =
      match Pdf.lookup_direct pdf "/Annots" rest with
      | Some (Pdf.Array annotations) ->
        Pdf.Array (obj :: annotations)
    | Some _ -> raise (Pdf.PDFError "Bad annotation dictionary")
    | None -> Pdf.Array [obj]
    in
    {page with rest = Pdf.add_dict_entry rest "/Annots" new_annots}
  
  let attach ~pdf ~filename ~content =
    let embeddedfile = stream [("/Type", Name "/EmbeddedFile")] content in
    let id_embeddedfile = Pdf.addobj pdf embeddedfile in
    let filespec = dict [
        ("/Type", Name "/Filespec");
        ("/F", String filename);
        ("/EF", dict [("/F", Indirect id_embeddedfile)]);
      ]
    in
    let id_filespec = Pdf.addobj pdf filespec in
    let coord =
      Pdf.(Array [Real 0.; Real 0.; Real 0.; Real 0.])
    in
    let annot = dict [
        ("/Type", Name "/Annot");
        ("/Subtype", Name "/FileAttachment");
        ("/FS", Indirect id_filespec);
        ("/Rect", coord);
        ("/F", Integer 2); (* Hidden annotation *)
      ]
    in
    let i = Pdf.addobj pdf annot in
    let annotobj = Pdf.Indirect i in
    (* TODO : Add the annotation in the array of /Annots *)
    let pages = Pdfpage.pages_of_pagetree pdf in
    match pages with
    | [] -> failwith "This PDF has no pages. Impossible to add the annotation."
    | p :: _pages ->
      let newpage = add_annotation pdf p annotobj in
      let pdf = Pdfpage.change_pages ~changes:[1,1] true pdf [newpage] in
      Pdf.remove_unreferenced pdf;
      pdf

end

(** Borrowed from containers *)
module IO = struct

  let finally_ f x ~h =
    try
      let res = f x in
      h x;
      res
    with e ->
      h x;
      raise e

  let with_in ?(mode=0o644) ?(flags=[Open_text]) filename f =
    let ic = open_in_gen (Open_rdonly::flags) mode filename in
    finally_ f ic ~h:close_in

  let read_all ic =
    let buf = ref (Bytes.create 1024) in
    let len = ref 0 in
    try
      while true do
        (* resize *)
        if !len = Bytes.length !buf then (
          buf := Bytes.extend !buf 0 !len;
        );
        assert (Bytes.length !buf > !len);
        let n = input ic !buf !len (Bytes.length !buf - !len) in
        len := !len + n;
        if n = 0 then raise Exit;  (* exhausted *)
      done;
      assert false (* never reached*)
    with Exit ->
      Bytes.sub_string !buf 0 !len
end

let smash filepdf filebc fileout =
  let version, extra, bc =
    BC.from_bytefile @@ OByteLib.Bytefile.read filebc
  in
  let shebang = "/home/gabriel/.opam/tools/bin/ocamlrun" in

  BC.write ~filename:"bdebug.bc" ~shebang ~extra ~version bc ;
  
  let bc_string =
    R.get_ok @@
    Bos.OS.File.with_tmp_oc "bytepdf%s"
      (fun f oc bc ->
         BC.write_oc ~oc ~shebang ~extra ~version bc;
         R.get_ok @@ Bos.OS.File.read f
      )
      bc
  in
  (* let original_bc_len = String.length bc_string in *)
  
  let pdf = Pdfread.pdf_of_file None None filepdf in
  let pdf = PdfAnnot.attach ~pdf ~filename:filebc ~content:bc_string in
  
  let pdf_string =
    let oc, br = Pdfio.input_output_of_bytes 16 in
    Pdfwrite.pdf_to_output
      ~preserve_objstm:false ~generate_objstm:false ~compress_objstm:false false
      None pdf oc;
    let b = Pdfio.extract_bytes_from_input_output oc br in
    Pdfio.string_of_bytes b
  in
  let pdf_len = String.length pdf_string in

  let search_token = "#!" ^ shebang ^ "\n" in
  let search_len = String.length search_token in
  
  let bc_start =
    let i = CCString.find ~sub:search_token pdf_string in
    assert (i <> -1) ;
    search_len + i
  in
  let bc_end =
    let sub = OByteLib.Version.to_magic version ^ "\nendstream" in
    let i = CCString.rfind ~sub pdf_string in
    assert (i <> -1) ;
    Format.printf "bc_end: %i@.%s@." i (String.sub pdf_string i 20) ;
    i - 8 * List.length bc - 4
  in
  let offset = pdf_len - bc_end in
  
  Format.printf "offset: %i@.%s@." offset
    (String.sub pdf_string bc_end (4 + 8 * List.length bc))
  ;
  
  let extra = String.sub pdf_string 0 bc_start in
  let _bc_string2 = String.sub pdf_string bc_start (bc_end - bc_start) in
  (* assert (CCString.find ~sub:bc_string2 bc_string <> -1) ; *)
  let xpdf = String.sub pdf_string bc_end (pdf_len - bc_end) in
  
  BC.write
    ~filename:fileout
    ~version
    ?shebang:None
    ~extra
    (bc @ [BC.Unknown {name="XPDF"; data=xpdf}]);
  
  (* ignore (extra, xpdf); *)
  (* let oflags = [ Open_wronly; Open_creat; Open_trunc; Open_binary ] in
   * let oc =
   *   try open_out_gen oflags 0o751 fileout
   *   with _ -> failwith @@ Printf.sprintf "fail to open file %S for writing" fileout
   * in
   * output_string oc pdf_string; *)

  ()
  
let term =
  let open Cmdliner in
  let bytecode =
    let doc =
      Arg.info ~docv:"BC" ~doc:"The OCaml bytecode file to be included in the resulting polyglot file." ["bytecode";"bc"]
    in
    Arg.(required & opt (some non_dir_file) None doc)
  in
  let pdf = 
    let doc =
      Arg.info ~docv:"PDF" ~doc:"The PDF file to be included in the resulting polyglot file." ["pdf"]
    in
    Arg.(required & opt (some non_dir_file) None doc)
  in
  let output = 
    let doc =
      Arg.info ~docv:"PDF" ~doc:"Output file" ["o"]
    in
    Arg.(required & opt (some string) None doc)
  in
  let info =
    Term.info "bytepdf"
      ~doc:"Merge an OCaml bytecode and a PDF into a file that is both."
  in
  let t =
    Term.(const smash $ pdf $ bytecode $ output)
  in
  (t, info)

let () = Cmdliner.Term.(exit @@ eval term)
