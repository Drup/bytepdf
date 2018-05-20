default: all

all:
	jbuilder build @install --dev

test:
	jbuilder runtest

clean:
	jbuilder clean

doc:
	jbuilder build @doc

.PHONY: all test clean doc
