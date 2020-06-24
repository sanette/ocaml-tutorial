# Ocaml-tutorial

Project for revamping the OCaml manual

The original OCaml manual can be found
[here](https://ocaml.org/releases/).

All contents generated, and displayed for demonstration only on the
page

https://sanette.github.io/ocaml-tutorial/index.html

are copyright Institut National de Recherche en Informatique et en
Automatique (INRIA).


## Running the script

```
dune exec src/process_standalone.exe

```

This will downwload (to the `html` dir) and process (to the `docs` dir)
all versions from 4.00 to 4.10.

## TODO

Write a global index page. Currently we only offer direct links to
Part 1 (Tutorials) but other parts exist and can be accessed via
internal links.

## OCaml API

The "API" part of the manual is **not** part of this project, but
instead moved to a separate one: see

https://github.com/sanette/ocaml-api
