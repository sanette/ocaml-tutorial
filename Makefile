local:
	dune exec src/process.exe

css: 
	sass src/manual.scss > ocamlorg/manual_ocamlorg.css

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

