PATH_TO_CIL             = $(PWD)/../cil
PATH_TO_CIL_LIBS	= $(PATH_TO_CIL)/_build/src
PATH_TO_FRONTC_LIBS	= $(PATH_TO_CIL)/_build/src/frontc
PATH_TO_OCAMLUTIL_LIBS	= $(PATH_TO_CIL)/_build/src/ocamlutil
PATH_TO_EXT_LIBS	= $(PATH_TO_CIL)/_build/src/ext
PATH_TO_EXT_CALLGRAPH   = $(PATH_TO_EXT_LIBS)/callgraph

OCAMLOPT  = ocamlopt -g -I $(PATH_TO_CIL_LIBS) -I $(PATH_TO_FRONTC_LIBS) -I $(PATH_TO_OCAMLUTIL_LIBS) -I $(PATH_TO_EXT_LIBS) -I $(PATH_TO_EXT_CALLGRAPH)
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

POINTER_OBJS = \
	utils.cmx \
	pointer.cmx 

MY_LIBS = \
	str.cmxa \
	unix.cmxa \
        nums.cmxa \
	cil.cmxa 

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

pointer: $(POINTER_OBJS)
	$(OCAMLOPT) -o pointer $(MY_LIBS) $(POINTER_OBJS)

clean:
	$(RM) pointer *.cmi *.cmx *.o
