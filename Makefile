all: calc

calc: calc.hs
	ghc -O3 -Wall $<

.PHONY: test clean distclean

test: calc
	(cd test; make test)
	

clean:
	(cd test; make clean)
	rm -rf *.hi *.o

distclean: clean
	rm -rf calc
