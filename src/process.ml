(* Post-processing the HTML of the OCaml Manual, Part 1

   based on Lambdasoup

   To execute this file, cd to the directory where the "html" and "docs" dirs
   reside, and [dune exec src/process.exe]

 *)

open Soup
open Printf

let debug = false
let pr = if debug then print_endline else fun _ -> ()
                                                   
(* Set this to the directory where to find the html sources of all versions: *)
let html_maindir = "html"
(* Set this to the destination directory: *)
let docs_maindir = "docs"
let ocamlorg_maindir = "ocamlorg"
(* Header for ocaml.org md files. *)
let md_head = "<!-- ((! set title Manual !)) ((! set documentation !)) ((! set manual !)) ((! set nobreadcrumb !)) -->\n"
  
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
let index version =
  let html = open_in (html_file version "index.html")
             |> read_channel in
  let soup = parse html in
  (* Foreword. We do nothing. *)
  (* Tutorials. We select the list of (html files, titles) for Part 1 *)
  let tutorials =
    let a = match select_one "a[id=\"p:tutorials\"]" soup with
      | Some node -> node
      | None -> R.select_one "a[name=\"p:tutorials\"]" soup in
    let ul = next a in
    assert (name ul = "ul");
    ul $$ "a"
    |> fold (fun list a ->
        (R.leaf_text a |> remove_number, R.attribute "href" a) :: list) []
    |> List.rev in
  tutorials   

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
  
let convert version chapters (title, file) =
  print_endline
    ((html_file version file) ^ " ==> " ^ (docs_file version file));

  (* First we perform some direct find/replace in the html string. *)
  (* Warning charset = ascii for 4.05 et utf8 for 4.09.  But the original html
     is kind of buggy because 4.05 uses &#XA0; for non-breaking space in
     us-ascii encoding, although &#XA0; is latin encoding...  Finally we decide
     to will force utf8 encoding.  *)
  let html =
    open_in (html_file version file) |> read_channel
    (* Normalize non-break spaces: *)
    |> Str.global_replace (Str.regexp_string "&#XA0;") " "
    |> Str.global_replace (Str.regexp "Chapter \\([0-9]\\)")
      "<span>Tutorial \\1</span>"
    |> Str.global_replace (Str.regexp_string "chapter") "tutorial"
    |> Str.global_replace (Str.regexp_string "Chapter") "Tutorial"
    |> Str.global_replace (Str.regexp ">[0-9]\\.\\([0-9]+\\) ") ">\\1 "
    |> Str.global_replace (Str.regexp "[0-9]\\.\\([0-9]+\\.[0-9]+\\) ")
      "\\1 "
    |> Str.global_replace (Str.regexp_string file) ""
    (* that one was not necessary; it's just cleaner not to link to oneself. *)
  in

  (* Set utf8 encoding directly in the html string *)
  let charset_regexp = Str.regexp "charset=\\([-A-Za-z0-9]+\\)\\(\\b\\|;\\)" in

  let html = match Str.search_forward charset_regexp html 0 with
    | exception Not_found -> pr "Warning, no charset found in html."; html
    | _ -> match (String.lowercase_ascii (Str.matched_group 1 html)) with
      | "utf-8" -> pr "Charset is UTF-8; good."; html
      | "us-ascii" -> pr "Charset is US-ASCII. We change it to UTF-8";
        Str.global_replace charset_regexp "charset=UTF-8\\2" html
      | _ -> pr "Warning, charset not recognized."; html in

  (* Now we use lambdasoup *)
  let soup = parse html in

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
  (* let dummy = create_element "div" ~attributes:["id", "top"] in
   * prepend_child body dummy; *)

  (* Remove first three links "Previous, Up, Next" *)
  soup $ "hr" |> delete;
  ["Previous"; "Up"; "Next"]
  |> List.iter (fun s ->
      soup $$ ("img[alt=\"" ^ s ^ "\"]")
      |> iter (do_option delete << parent));

  (* Create TOC *)
  let toc = soup $ "ul" in
  let nav = create_element "nav" ~class_:"toc" in
  wrap toc nav;
  let nav = soup $ "nav" in
  wrap nav (create_element "header");
  (* TOC - Create a "Top" entry in the menu *)
  let a = create_element "a" ~inner_text:"Top"
      ~attributes:["href", "#"] in
  let li = create_element "li" ~class_:"top" in
  append_child li a;
  prepend_child toc li;
  let toc_title = create_element "div" ~class_:"toc_title" in
  let a = create_element "a" ~inner_text:("Version " ^ version)
      ~attributes:["href", "../index.html"; "id", "version-select"] in
  append_child toc_title a;
  prepend_child toc toc_title;

  (* Create new menu *)
  let menu = create_element "ul" ~class_:"tutos_menu" in
  List.iter (fun (title, href) ->
      let a = create_element "a" ~inner_text:title ~attributes:["href", href] in
      let li = if href= file
        then create_element "li" ~class_:"active"
        else create_element "li" in
      append_child li a;
      append_child menu li) chapters;
  let body = soup $ "div.content" in
  prepend_child body menu;

  (* Add logo *)
  let logo_html = "<nav class=\"toc brand\"><a class=\"brand\" href=\"https://ocaml.org/\" ><img src=\"colour-logo-gray.svg\" class=\"svg\" alt=\"OCaml\" /></a></nav>" in
  let header = soup $ "header" in
  prepend_child header (parse logo_html);

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

  (* Syntax highlighting *)
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
      | _ -> ()) camls;

  (* Only for versions <= 4.00 *)
  if float_of_string version <= 4.0 then begin
    soup $$ "h2.section"
    |> iter (fun e ->
        to_string e |>
        Str.global_replace (Str.regexp "[0-9]\\.\\([0-9]+\\)") "\\1"
        |> parse
        |> replace e)
  end;

  (* Add copyright *)
  append_child body (copyright ());

  (* Save new html file *)
  let new_html= to_string soup in
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

(* Create "index.html" (for standalone version) 
      and "index.md"   (for ocaml.org) *)
let make_index versions =
  let html = read_channel @@ open_in @@ Filename.concat "src" "index.html" in
  let soup = parse html in
  let ul = soup $ "ul.versions" in
  List.iter (fun (version, file) ->
      let v = "Version " ^ version in
      let file = Filename.concat version file in
      let li = create_element "li" in
      let a = create_element ~attributes:["href", file] ~inner_text:v "a" in
      append_child li a;
      append_child ul li) versions;
  write_file (Filename.concat docs_maindir "index.html") (to_string soup);
  soup $ "div.manual" |> to_string |> (^) md_head 
  |> write_file (Filename.concat ocamlorg_maindir "index.md")
  
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
        let url = sprintf
            "http://caml.inria.fr/distrib/ocaml-%s/ocaml-%s-refman-html.tar.gz"
            version version in
        let tmp = Filename.temp_file version ".tar.gz" in
        let name = Filename.basename tmp in
        if Sys.command (sprintf "wget %s -O %s" url tmp) <> 0
        then failwith ("Could not download manual at " ^ url)
        else begin
          (* Sys.rename won't work between different partitions... *)
          sys_cp tmp (Filename.concat dir name);
          Sys.remove tmp;
          Sys.chdir dir;
          if Sys.command (sprintf "tar xvf %s" name) <> 0
          then failwith (sprintf "Could not extract %s." name)
          else Sys.remove name
        end
      end
    with
    | e -> Sys.chdir pwd; raise e
  end;
  Sys.chdir pwd

let download_versions = List.iter download_version

(* Completely process the given version of the manual.
   Returns the name of the main html file. *)
let process version =
  print_endline (sprintf "\nProcessing version %s...\n" version);

  pr (sprintf "Current directory is: %s" (Sys.getcwd ()));
  sys_mkdir (docs_dir version);
  sys_mkdir (ocamlorg_dir version);

  pr "* Copying files";
  let to_copy = ["colour-logo-gray.svg"; "manual.css"] in
  List.iter (fun file ->
      pr file;
      sys_cp (Filename.concat "src/" file) (docs_file version file)
    ) to_copy;

  pr "* Scanning index";
  let chapters = index version in

  pr "* Processing chapters";
  List.iter (convert version chapters) chapters;

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

  snd (List.hd chapters)
  
  
(*********************************************************************)
        
let () = 
  let all_versions = Array.init 10 (fun i -> sprintf "4.%02u" i)
                     |> Array.to_list in
  download_versions all_versions;
  List.iter (fun file ->
      sys_cp (Filename.concat "src" file) (Filename.concat docs_maindir file))
    ["colour-logo-gray.svg"; "index.html"; "manual.css"];

  let versions = List.map (fun v -> v, process v) all_versions in
  pr "* Make index";
  make_index (List.rev versions);
  
  pr "DONE."
