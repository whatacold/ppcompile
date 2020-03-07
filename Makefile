EMACS ?= emacs

.PHONY: compile test checkdoc clean

all: compile test checkdoc

compile:
	$(EMACS) -Q -batch --eval '(mapc #'"'"'byte-compile-file '"'"'("ppcompile.el"))'

test:
	$(EMACS) -Q -batch -l ppcompile.el -l test/ppcompile-test.el --eval "(ert-run-tests-batch-and-exit '(not (tag sshd)))"

test-with-sshd:
	$(EMACS) -Q -batch -l ppcompile.el -l test/ppcompile-test.el --eval "(ert-run-tests-batch-and-exit '(tag sshd))"

checkdoc:
	$(EMACS) -Q -batch --eval '(checkdoc-file "ppcompile.el")'

clean:
	rm -f *.elc
