all: lc

lc: Core.hs lc.hs
	ghc --make lc.hs

clean: 
	rm -f *.hi *.o lc

