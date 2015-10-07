all:
	ocamlfind ocamlc ofx.ml -package xml-light,oUnit -linkpkg 

test:
	ocamlfind ocamlc ofx.ml test.ml -package xml-light,oUnit -linkpkg -g
