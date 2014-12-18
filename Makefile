
OCAMLC = ocamlc -g
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc -v
OCAMLMKTOP = ocamlmktop
MENHIR = menhir
OCAMLDEP = ocamldep
OCAMLDSORT = ocamldsort
INSTALL = install
PREFIX = /usr/local

COBJ = 

# Source plus generated files.
OCAMLSRC := cmm.ml parser.ml lexer.ml cmmparse.ml

OCAMLOBJ := $(shell < .depend $(OCAMLDSORT) -byte $(OCAMLSRC))

TARGET = cmmparse

all:	$(TARGET)

clean:
	rm -f *.cmo *.cmi $(TARGET) parser.ml lexer.ml parser.mli

cleaner: clean
	rm -f .depend

install: all
	$(INSTALL) $(TARGET) $(PREFIX)/bin

ML_ERROR:
	@echo Some sort of Ocaml dependency error occurred.
	@false

# core compiler
$(TARGET): $(OCAMLOBJ)
	$(OCAMLC) $(OCAMLOBJ) -o $@

# Also include (lex, yacc) generated files here.
.depend:	$(OCAMLSRC)
	$(OCAMLDEP) $(OCAMLSRC) > .depend

%.cmo: %.ml
	$(OCAMLC) $< -c -o $@

%.cmi: %.mli
	$(OCAMLC) $< -c -o $@

%.ml: %.mly
	$(MENHIR) --infer $<

%.ml: %.mll
	$(OCAMLLEX) $<

parser.ml: cmm.cmo
lexer.ml: cmm.cmo parser.cmo
parser.cmo: cmm.cmo parser.cmi
lexer.cmo: cmm.cmo parser.cmo

parser.cmi: parser.mli

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleaner)
include .depend
endif
endif
