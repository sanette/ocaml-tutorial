(* Cf https://github.com/aantron/lambdasoup/blob/master/docs/postprocess.ml *)

open Soup
open Printf
    
let convert chapters (title, file, outfile) =

  print_endline (file ^ " ==> " ^ outfile);

  (* First we perform some direct find/replace in the html string. *)  
  let html =
    open_in ("html/" ^ file) |> read_channel
    |> Str.global_replace (Str.regexp_string "chapter") "tutorial"
    |> Str.global_replace (Str.regexp "Chapter \\([0-9]\\)") "<span>Tutorial \\1</span>"
    |> Str.global_replace (Str.regexp_string "Chapter") "Tutorial"
    |> Str.global_replace (Str.regexp "[0-9]\\.\\([0-9]+\\) ") "\\1 "
  in
  let html = List.fold_left
      (fun s (_, file, outfile) ->
         Str.global_replace (Str.regexp_string file) outfile s)
      html
      chapters in

  (* Now we use lambdasoup *)
  let soup = parse html in

  (* Change title *)
  let title_tag = soup $ "title" in
  let new_title = create_element "title" ~inner_text:("Ocaml - " ^ title) in
  replace title_tag new_title;

  (* Wrap body *)
  let body = soup $ "body" in
  wrap body (create_element "body");
  let body = soup $ "body" in
  let dummy = body $ "body" in
  wrap dummy (create_element "div" ~class_:"content");
  let body = body $ "body" in
  unwrap body;

  (* Create TOC *)
  let toc = soup $ "ul" in
  let nav = create_element "nav" ~class_:"toc" in
  wrap toc nav;
  let nav = soup $ "nav" in
  wrap nav (create_element "header");

  (* Remove first three links "Previous, Up, Next" *)
  soup $ "a"
  |> delete;
  soup $ "a"
  |> delete;
  soup $ "a"
  |> delete;
  soup $ "hr"
  |> delete;

  (* Remove the other "Previous, Up, Next" links at the end of the file *)
  let links_to_remove =
    List.map (fun (_,_,file) -> file) chapters
    |> List.append ["index.html"; "foreword.html"; "language.html"] in
  soup $$ "a"
  |> iter (fun e ->
      match attribute "href" e with
      | Some f when List.mem f links_to_remove ->
        delete e
      | _ -> ());

  (* Create new menu *)
  let menu = create_element "ul" ~class_:"tutos_menu" in
  List.iter (fun (title, _ , file) ->
      let a = create_element "a" ~inner_text:title ~attributes:["href", file] in
      let li = if file = outfile
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
  
  (* Save new html file *)
  let new_html= to_string soup in
  write_file ("docs/" ^ outfile) new_html

let () =
  print_endline (sprintf "Current directory is: %s" (Sys.getcwd ()));
  print_endline "* Copying files";
  let to_copy = ["colour-logo-gray.svg"; "manual.css"] in
  List.iter (fun file ->
      print_endline file;
      if Sys.command (sprintf "cp src/%s docs/%s" file file) <> 0
      then failwith ("Could not copy " ^ file)) to_copy;

  print_endline "* Processing chapters";
  let chapters =
    ["core", "The Core Language";
     "module", "The Module System";
     "object", "Objects in OCaml";
     "labl", "Labels and Variants";
     "polym", "Polymorphism and its Limitations";
     "adv", "Advanced Examples with Classes and Modules" ]
    |> List.map (fun (chap, title) ->
        (title, chap ^ "examples.html", chap ^ "_tutorial.html")) in

  (* We correct this particular case: *)
  let chapters = List.map (fun (title, file, file2) ->
      if file2 = "polym_tutorial.html"
      then (title, "polymorphism.html", file2)
      else (title, file, file2)) chapters in
  
  List.iter (convert chapters) chapters

