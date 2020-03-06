EMACS ?= emacs

.PHONY: compile test checkdoc clean

all: compile test checkdoc

compile:
	$(EMACS) -Q -batch --eval '(mapc #'"'"'byte-compile-file '"'"'("ppcompile.el"))'

test:
	$(EMACS) -Q -batch -l ppcompile.el -l test/ppcompile-test.el -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q -batch --eval '(checkdoc-file "ppcompile.el")'

clean:
	rm -f *.elc
