default: all

all:
	dune build
test:
	dune runtest

clean:
	dune clean

.PHONY: all test clean
