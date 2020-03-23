(* Post-processing the HTML of the OCaml Manual, Part 1

   based on Lambdasoup

   To execute this file, cd to the directory where the "html" and "docs" dirs
   reside, and [dune exec src/process.exe]

 *)

open Soup
open Printf

let debug = true
let pr = if debug then print_endline else fun _ -> ()
                                                   
(* Set this to the directory where to find the html sources of all versions: *)
let html_maindir = "html"
(* Set this to the destination directory: *)
let docs_maindir = "docs"
let ocamlorg_maindir = "ocamlorg"
(* Header for ocaml.org md files. *)
let md_head = "<!-- ((! set title Manual !)) ((! set documentation !)) ((! set manual !)) ((! set nobreadcrumb !)) -->\n"
let archives = ["refman-html.tar.gz"; "refman.txt"; "refman.pdf"; "refman.info.tar.gz"]

(* Where to get the original html files *)
let html_dir version = Filename.concat version "htmlman"
                       |> Filename.concat html_maindir
let html_file version = Filename.concat (html_dir version)

(* Where to save the modified html files *)
let docs_dir version = Filename.concat docs_maindir version
let docs_file version = Filename.concat (docs_dir version)

(* Where to save the ocamlorg files *)
let ocamlorg_dir version = Filename.concat ocamlorg_maindir version
let ocamlorg_file version = Filename.concat (ocamlorg_dir version)

(* Return next html element. *)
let rec next node =
  match next_element node with
  | Some n -> n
  | None -> match parent node with
    | Some p -> next p
    | None -> raise Not_found

(* Remove number: "Chapter 1  The core language" ==> "The core language" *)
let remove_number s =
  Str.global_replace (Str.regexp ".+  ") "" s

(* Scan the index and return the list of chapters (title, file) *)
let index version part =
  sprintf "Reading part [%s]" part |> pr;
  let html = read_file (html_file version "index.html") in
  let soup = parse html in
  (* Foreword. We do nothing. *)
  (* We select the list of (html files, titles) for Part [part] *)
  let part =
    let a = match select_one ("a[id=\"p:" ^ part ^ "\"]") soup with
      | Some node -> node
      | None -> R.select_one ("a[name=\"p:" ^part ^ "\"]") soup in
    let ul = next a in
    assert (name ul = "ul");
    ul $$ "a"
    |> fold (fun list a ->
        (R.leaf_text a |> remove_number, R.attribute "href" a) :: list) []
    |> List.rev in
  part

(* Some syntax... *)
let do_option f = function
  | None -> ()
  | Some x -> f x

let (|?>) o f = match o with None -> None | Some x -> Some (f x)

let (<<) f g x = f (g x)
    
let onlyif f x = if f x then Some x else None

(* For instance we want to do:
   # "ABC" |> onlyif ((=) 3 << String.length) |?> print_endline |> ok;;
   ABC
   - : unit = ()
   Which is a super complicated way to write
   # let s = "ABC" in if String.length s = 3 then print_endline s
*)

let ok = function | None -> () | Some () -> ();;
                
let tokens_to_string tokens =
  List.map Higlo.token_to_xml tokens
  |> Xtmpl_xml.to_string
  
let highlight code =
  let tokens = Higlo.parse ~lang:"ocaml" code in
  parse (tokens_to_string tokens);;

let copyright () =
  "<div class=\"copyright\">The present documentation is copyright Institut \
   National de Recherche en Informatique et en Automatique (INRIA). A complete \
   version can be obtained from <a \
   href=\"http://caml.inria.fr/pub/docs/manual-ocaml/\">this page</a>.</div>"
  |> parse

let load_html ~version file =
  pr file;
  (* First we perform some direct find/replace in the html string. *)
  (* Warning charset = ascii for 4.05 et utf8 for >=4.09.  But the original html
     is kind of buggy because 4.05 uses &#XA0; for non-breaking space in
     us-ascii encoding, although &#XA0; is latin encoding...  Finally we decide
     to force utf8 encoding.  *)
  let html =
    read_file (html_file version file)
    (* Normalize non-break spaces: *)
    |> Str.global_replace (Str.regexp_string "&#XA0;") " "
    |> Str.global_replace (Str.regexp "Chapter \\([0-9]+\\)")
      "<span>Chapter \\1</span>"
    (* |> Str.global_replace (Str.regexp_string "chapter") "tutorial"
     * |> Str.global_replace (Str.regexp_string "Chapter") "Tutorial" *)
    |> Str.global_replace (Str.regexp ">[0-9]+\\.\\([0-9]+\\) ") ">\\1 "
    |> Str.global_replace (Str.regexp "[0-9]+\\.\\([0-9]+\\.[0-9]+\\) ")
      "\\1 "
    |> Str.global_replace (Str.regexp_string "\"libref/")
      (sprintf "\"../../api/%s/" version)
    |> Str.global_replace (Str.regexp_string "\"compilerlibref/")
      (sprintf "\"../../api/%s/compilerlibref/" version)

  (* |> Str.global_replace (Str.regexp_string file) "" *)
  (* that one was not necessary; it's just cleaner not to link to oneself. *)
  in

  (* Set utf8 encoding directly in the html string *)
  let charset_regexp = Str.regexp "charset=\\([-A-Za-z0-9]+\\)\\(\\b\\|;\\)" in

  match Str.search_forward charset_regexp html 0 with
  | exception Not_found -> pr "Warning, no charset found in html."; html
  | _ -> match (String.lowercase_ascii (Str.matched_group 1 html)) with
    | "utf-8" -> pr "Charset is UTF-8; good."; html
    | "us-ascii" -> pr "Charset is US-ASCII. We change it to UTF-8";
      Str.global_replace charset_regexp "charset=UTF-8\\2" html
    | _ -> pr "Warning, charset not recognized."; html

let save_to_files ~version soup file =
 (* Save new html file *)
  let new_html = to_string soup in
  write_file (docs_file version file) new_html;
  (* Save ocamlorg md file *)
  let md = Filename.basename file
           |> Filename.remove_extension in
  let md = md ^ ".md" in
  (* for ocaml.org, we modify the link on the version number *)
  soup $$ "a#version-select"
  |> iter (set_attribute "href" "/docs");
  soup $ "div.manual" |> to_string
  |> (^) md_head 
  |> write_file (ocamlorg_file version md)

let remove_navigation soup =
  (* Remove first three links "Previous, Up, Next" *)
  do_option delete (soup $? "hr");
  ["Previous"; "Up"; "Next"]
  |> List.iter (fun s ->
      soup $$ ("img[alt=\"" ^ s ^ "\"]")
      |> iter (do_option delete << parent))

(* include external html file at the end of the currently processed [body] *)
let include_file ~version ?(id_tag="h2") body file =
  sprintf "*** appending file %s" file |> pr;
  let xternal = parse (load_html ~version file) in
  remove_navigation xternal;
  do_option delete (xternal $? "hr");
  let xbody = xternal $ "body" in
  let id = xbody $ id_tag |> attribute "id" in
  set_name "external" xbody;
  append_child body xbody;
  body $ "external" |> unwrap;
  id
  
(* Alternative to [include_file]. Here we create a new file by cloning the
   structure of "soup", and inserting the content of new file (hence preserving
   TOC and headers) *)
let clone_structure ~version soup xfile =
  let xternal = parse (load_html ~version xfile) in
  remove_navigation xternal;
  do_option delete (xternal $? "hr");
  let xbody = xternal $ "body" in
  let clone = parse (to_string soup) in
  let header = clone $ "header" in
  insert_after header xbody;
  create_element ~id:"start-section" "a"
  |> insert_after header;
  next_siblings xbody
  |> iter delete;
  insert_after xbody (copyright ());
  set_name "section" xbody;
  set_attribute "id" "section" xbody;
  save_to_files ~version clone xfile


  
(* [convert] has to be run for each "entry" [file] of the manual, making a
   "Chapter".  (the list of [chapters] corresponds to a "Part" of the manual) *)
let convert version chapters (title, file) =
  print_endline
    ((html_file version file) ^ " ==> " ^ (docs_file version file));

  (* We use lambdasoup *)
  let soup = parse (load_html ~version file) in

  (* Change title *)
  let title_tag = soup $ "title" in
  let new_title = create_element "title" ~inner_text:("Ocaml - " ^ title) in
  replace title_tag new_title;

  (* Wrap body. TODO use set_name instead *)
  let body = soup $ "body" in
  wrap body (create_element "body");
  let body = soup $ "body" in
  let dummy = body $ "body" in
  wrap dummy (create_element "div" ~classes:["manual"; "content"]);
  let body = body $ "body" in
  unwrap body;
  let body = soup $ "div.content" in
  (* let dummy = create_element "div" ~attributes:["id", "top"] in
   * prepend_child body dummy; *)

  remove_navigation soup;

  (* Remove "translated from LaTeX" *)
  if file = "index.html"
  then soup $$ "blockquote" |> last |> do_option delete;

  (* Create left sidebar for TOC.  *)
  let toc = match soup $? "ul" with
    | None -> None (* can be None, eg chapters 15,19...*)
    | Some t -> if classes t <> [] (* as in libthreads.html or parsing.html *)
      then (sprintf "We don't promote <UL> to TOC for file %s" file |> pr; None)
      else Some t in 
  let nav = create_element "nav" ~class_:"toc" in
  let () = match toc with
    | None -> prepend_child body nav
    | Some toc -> wrap toc nav in
  let nav = soup $ "nav" in
  wrap nav (create_element "header");
  begin match toc with
    | None -> sprintf "No TOC for %s" file |> pr
    | Some toc -> begin
        (* TOC - Create a "Top" entry in the menu *)
        let a = create_element "a" ~inner_text:"Top"
            ~attributes:["href", "#"] in
        let li = create_element "li" ~class_:"top" in
        append_child li a;
        prepend_child toc li;

        (* index of keywords *)
        if file = "index.html"
        then let keywords =
               body $$ "ul"
               |> fold (fun key ul ->
                   match key with
                   | None -> begin
                       match ul $$ "li" |> last with
                       | None -> None
                       | Some l -> begin match l $ "a" |> leaf_text with
                           | Some text -> sprintf "[%s]" text |> pr;
                             if text = "Index of keywords"
                             then l $ "a" |> attribute "href" else None
                           | None -> None
                         end
                     end
                   | _ -> key) None in
          begin match keywords with
            | None -> pr "Could not find Index of keywords"
            | Some keywords ->
              let a = create_element "a" ~inner_text:"Index of keywords"
                  ~attributes:["href", keywords] in
              let li = create_element "li" ~class_:"top" in
              (append_child li a;
               append_child toc li)
          end else ();
      end
  end;

  (* Add title *)
  let toc_title = create_element "div" ~class_:"toc_title" in
  let a = create_element "a" ~inner_text:title
      ~attributes:["href", "#"] in
  append_child toc_title a;
  prepend_child nav toc_title;

  (* Add version number *)
  let vnum = create_element "div" ~class_:"toc_version" in
  let a = create_element "a" ~inner_text:("Version " ^ version)
      ~attributes:["href", "../index.html"; "id", "version-select"] in
  append_child vnum a;
  prepend_child nav vnum;

  (* Create new menu *)
  let menu = create_element "ul" ~class_:"part_menu" in
  List.iter (fun (title, href) ->
      let a = create_element "a" ~inner_text:title ~attributes:["href", href] in
      let li = if href= file
        then create_element "li" ~class_:"active"
        else create_element "li" in
      append_child li a;
      append_child menu li) chapters;
  (* let body = soup $ "div.content" in *)
  prepend_child body menu;

  (* Add logo *)
  begin match soup $? "header" with
    | None -> sprintf "Warning: no <header> for %s" file |> pr
    | Some header ->
      let logo_html = {|<nav class="toc brand"><a class="brand" href="https://ocaml.org/" ><img src="colour-logo-gray.svg" class="svg" alt="OCaml" /></a></nav>|} in
      prepend_child header (parse logo_html)
  end;

  (* Move authors to the end. Versions >= 4.05 use c009.  4.04 and 4.03 use
     c012, while 4.02 and 4.01: c013; for 4.00, only <i>. *)
  ["span.c009"; "span.c012"; "span.c013"; "i"]
  |> List.iter (fun selector ->
      soup $? selector
      |> do_option (fun authors ->
          match leaf_text authors with
          | None -> ()
          | Some s ->
            match Str.search_forward (Str.regexp "(.+written by.+)") s 0 with
            | exception Not_found -> ()
            | _ ->
              pr "Moving authors";
              delete authors;
              add_class "authors" authors;
              append_child body authors));

  (* Syntax highlighting. Done by LaTeX starting from 4.10 *)
  if float_of_string version <= 4.09 then begin
    pr "Syntax highlighting";
    (* The manual for 4.05 <= version <= 4.09 has some "div" inside a "pre", which
       is forbidden. See (https://github.com/ocaml/ocaml/issues/9109) We replace
       it by code or span. TODO check when this is corrected upstream. *)
    soup $$ "pre div"
    |> iter (set_name "code");
    let camls = soup $$ "pre .caml-input" in
    iter (fun e ->
        match leaf_text e with
        | Some code ->
          let h = highlight code in
          let cs = classes e in
          let div = create_element (name e) ~classes:cs in
          append_child div h;
          insert_after e div;
          delete e
        | _ -> ()) camls
  end;

  (* Only for versions <= 4.00 *)
  if float_of_string version <= 4.0 then begin
    soup $$ "h2.section"
    |> iter (fun e ->
        to_string e |>
        Str.global_replace (Str.regexp "[0-9]+\\.\\([0-9]+\\)") "\\1"
        |> parse
        |> replace e)
  end;

  if false then begin
    (* Wrap content after header (hence not TOC) in a section tag *)
    (* Not necessary here but could be useful later *)
    let section = create_element "section" in
    insert_after (body $ "header") section;
    next_siblings section
    |> iter (append_child section)
  end;

  (* Get the list of external files *)
  (* Ça a du bon d'inclure les fichiers, c'est pratique, mais
     malheureusement on perd les liens d'autres fichiers qui pointeraient
     vers eux. On ne fait donc plus ça. *)
  let xfiles = match toc with
    | None -> []
    | Some toc ->
      toc $$ "li"
      |> fold (fun list li ->
          let ref = li $ "a" |> R.attribute "href" in
          sprintf "TOC reference = %s " ref |> pr;
          if not (String.contains ref '#') &&
             not (String.length ref > 2 && String.sub ref 0 2 = "..")
          then begin
            li $ "a" |> set_attribute "href" (ref ^ "#start-section");
            ref::list
          end else list) []
  in

  (* Add copyright *)
  append_child body (copyright ());

  (* Generate external files *)
  List.iter (clone_structure ~version soup) xfiles;
  
  (* And finally save *)
  save_to_files ~version soup file
  
(* Create "index.html" (for standalone version only) *)
let make_index versions =
  let html = read_file @@ Filename.concat "src" "index.html" in
  let soup = parse html in
  let ul = soup $ "ul.versions" in
  List.iter (fun (version, files) ->
      let file = List.rev files |> List.hd in
      sprintf "Using file %s as entry for Version %s" file version |> pr;
      let v = "Version " ^ version in
      let file = Filename.concat version file in
      let li = create_element "li" in
      let a = create_element ~attributes:["href", file] ~inner_text:v "a" in
      append_child li a;
      append_child ul li) versions;
  write_file (Filename.concat docs_maindir "index.html") (to_string soup)
(* ; soup $ "div.manual" |> to_string |> (^) md_head 
    |> write_file (Filename.concat ocamlorg_maindir "index.md") *)
    

let sys_cp file dst =
  if Sys.command (sprintf "cp %s %s" file dst) <> 0
  then failwith ("Could not copy " ^ file)

let sys_mkdir dir =
  if Sys.command (sprintf "mkdir -p %s" dir) <> 0
  then failwith ("Could not create directory" ^ dir)

(* Download version of the manual if the dir does not exist yet. *)
(* Remark: remove the html_maindir to force downloading everything. *)
let download_version version =
  let pwd = Sys.getcwd () in
  begin try
      let dir = Filename.concat html_maindir version in
      if not (Sys.file_exists dir)
      then begin
        sys_mkdir dir;
        archives
        |> List.iter (fun name ->
            let file = sprintf "ocaml-%s-%s" version name in
            let url = sprintf "http://caml.inria.fr/distrib/ocaml-%s/%s"
                version file in
            let tmp = Filename.temp_file version name in
            if Sys.command (sprintf "wget %s -O %s" url tmp) <> 0
            then failwith ("Could not download manual at " ^ url)
            else begin
              (* Sys.rename won't work between different partitions... *)
              sys_cp tmp (Filename.concat dir file);
              Sys.remove tmp
            end);
        Sys.chdir dir;
        let html =  sprintf "ocaml-%s-refman-html.tar.gz" version in
        if Sys.command (sprintf "tar xvf %s" html) <> 0
        then failwith (sprintf "Could not extract %s." html)
      end
    with
    | e -> Sys.chdir pwd; raise e
  end;
  Sys.chdir pwd

let download_versions = List.iter download_version

(* Completely process the given version of the manual.
   Returns the names of the main html files. *)
let process version =
  print_endline (sprintf "\nProcessing version %s...\n" version);

  pr (sprintf "Current directory is: %s" (Sys.getcwd ()));
  sys_mkdir (docs_dir version);
  sys_mkdir (ocamlorg_dir version);

  pr "* Copying files";
  let css = let css = sprintf "manual-%s.css" version in
    if Sys.file_exists (Filename.concat "src/" css)
    then css, "manual.css" else "manual.css", "manual.css" in
  let to_copy = css::["colour-logo-gray.svg", "colour-logo-gray.svg"] in
  List.iter (fun (file, out) ->
      pr file;
      sys_cp (Filename.concat "src/" file) (docs_file version out)
    ) to_copy;
  
  sys_cp "src/colour-logo-gray.svg" (ocamlorg_file version "colour-logo-gray.svg");

  archives
  |> List.iter (fun name ->
      let file = sprintf "ocaml-%s-%s" version name in
      pr file;
      sys_cp (html_file version ("../" ^ file)) (ocamlorg_file version file));
  if float_of_string version < 4.09
  then begin
    sys_cp "src/libgraph.gif" (ocamlorg_file version "libgraph.gif");
    sys_cp "src/libgraph.gif" (docs_file version "libgraph.gif")
  end;

  (* special case of the "index.html" file *)
  (* TODO: the inline css styling of this file is quite bad, we should propose
     something else. *)
  convert version [] ("The OCaml Manual", "index.html");


  let parts = ["tutorials"; "refman"; "commands"; "library" ] in
  (* TODO "appendix" needs a special treatment *)
  let main_files = List.fold_left (fun list part ->
      pr "* Scanning index";
      let chapters = index version part in

      pr "* Processing chapters";
      List.iter (convert version chapters) chapters;
      (snd (List.hd chapters)) :: list) [] parts in

  main_files

  (* pr "* Create main link";
   * let link = "part1.html" in
   * [docs_file version; ocamlorg_file version]
   * |> List.iter (fun dir ->
   *     if not (Sys.file_exists (dir link))
   *     then let _,entry_file = List.hd chapters in
   *       if Sys.command
   *           (sprintf "ln -s %s %s" entry_file (dir link)) <> 0
   *       then failwith (sprintf "Could not create symlink to %s" (dir link)))
  *)

  
  
  
(*********************************************************************)
        
let () = 
  let all_versions = Array.init 11 (fun i -> sprintf "4.%02u" i)
                     |> Array.to_list in
  let _all_versions = ["4.10"] in
  download_versions all_versions;
  List.iter (fun file ->
      sys_cp (Filename.concat "src" file) (Filename.concat docs_maindir file))
    ["colour-logo-gray.svg"; "index.html"; "manual.css"];

  let versions = List.map (fun v -> v, process v) all_versions in
  pr "* Make index";
  make_index (List.rev versions);
  
  pr "DONE."
