CC = ocamlfind ocamlc
FLAGS = -package graphics -package unix -package camlimages.png -package camlimages.graphics
LFLAGS = -linkpkg

all: debut dm4 tests

tests: tests.ml debut.cmo dm4.cmo tests.cmo
	$(CC) -o tests debut.cmo dm4.cmo $(FLAGS) $(LFLAGS) $<

dm4: dm4.ml debut.cmo dm4.cmo
	$(CC) -o dm4 debut.cmo $(FLAGS) $(LFLAGS) $<

debut: debut.ml debut.cmo
	$(CC) -o debut $(FLAGS) $(LFLAGS) $<

%.cmo:
	$(CC) -c $(@:.cmo=.ml) $(FLAGS)
	
clean :
	rm -f *.cmo *.cmi *.o *.cmx 

mrpropper : clean
	rm -f test

