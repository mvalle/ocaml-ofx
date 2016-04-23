
ifdef PREFIX
 DESTDIR = $(PREFIX)
else
 DESTDIR = `ocamlc -where`
endif

all: ofx.cmx


test: ofx.cmx
	ocamlfind ocamlopt -g ofx.cmx test.ml -o test -package xml-light,oUnit -linkpkg

ofx.cmx:
	ocamlfind ocamlopt -package xml-light,oUnit -c ofx.ml

install: ofx.cmx
	echo $(DESTDIR)
	cp ofx.cmx $(DESTDIR)/bin 
