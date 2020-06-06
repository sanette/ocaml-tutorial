css: 
	sass src/manual_ocamlorg.scss > ocamlorg/manual_ocamlorg.css
	sass src/manual.scss > src/manual.css

local: css
	dune exec src/process_standalone.exe

ocamlorg: css
	cp src/main.mpp ocamlorg/main.mpp
	cp src/index.md ocamlorg/
	./ocamlorg.sh

all: local ocamlorg

check:
	linkchecker docs/index.html

clean:
	dune clean
	rm -rf docs/*
	rm -rf ocamlorg/*

