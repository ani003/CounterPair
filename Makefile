all:
	dune build counter_pair.exe

clean:
	dune clean
	rm -f *~
