EMACS ?= emacs

.PHONY: compile checkdoc clean test test-with-sshd

all: compile checkdoc test

compile:
	$(EMACS) -Q -batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile ppcompile.el

# TODO How to make it error out if something wrong?
checkdoc:
	$(EMACS) -Q -batch --eval '(checkdoc-file "ppcompile.el")'

test:
	$(EMACS) -Q -batch -l ppcompile.el -l test/ppcompile-test.el --eval "(ert-run-tests-batch-and-exit '(not (tag sshd)))"

test-with-sshd:
	$(EMACS) -Q -batch -l ppcompile.el -l test/ppcompile-test.el --eval "(ert-run-tests-batch-and-exit '(tag sshd))"

clean:
	rm -f *.elc
