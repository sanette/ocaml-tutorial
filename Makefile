local:
	dune exec src/process.exe
	cp src/main.mpp ocamlorg/main.mpp

css: 
	sass src/manual.scss > ocamlorg/manual_ocamlorg.css

ocamlorg: css 
	./ocamlorg.sh

all: local ocamlorg

check:
	linkchecker docs/index.html

clean:
	dune clean
	rm -rf docs/*
	rm -rf ocamlorg/*

