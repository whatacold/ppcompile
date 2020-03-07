;;; ppcompile-test.el --- Tests for ppcompile

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Guangwang Huang

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'ppcompile)

(ert-deftest test-authsource ()
  (let ((auth-sources '("./test/authinfo.txt"))
        (ppcompile-ssh-host "example.com")
        (ppcompile-ssh-port 1234)
        (ppcompile-ssh-user "foobar"))
    (should
     (string= "foo:BAR123!" (ppcompile-get-ssh-password)))))

(ert-deftest test-replace-path ()
  (let ((content (concat "/home/foo/Lorem ipsum dolor"
                         "/home/foo sit amet, consectetur adipiscing elit."
                         " /home/bar/Pellentesque sem mauris")))
    (with-temp-buffer
      (insert content)
      (let ((ppcompile--current-buffer (current-buffer)))
        (setq ppcompile-path-mapping-alist '(("/home/foo" . "/home/barr")
                                             ("/home/bar/" . "/home/baz")))
        (ppcompile--replace-path (current-buffer) "finish-msg"))
      (should
       (string= (concat "/home/barr/Lorem ipsum dolor"
                        "/home/foo sit amet, consectetur adipiscing elit."
                        " /home/baz/Pellentesque sem mauris")
                (buffer-string)))

      (erase-buffer)
      (insert content)
      (let ((ppcompile--current-buffer (current-buffer)))
        ;; replaced in 2 rounds
        (setq ppcompile-path-mapping-alist '(("/home/foo" . "/home/bar")
                                             ("/home/bar/" . "/home/baz")))
        (ppcompile--replace-path (current-buffer) "finish-msg"))
      (should
       (string= (concat "/home/baz/Lorem ipsum dolor"
                        "/home/foo sit amet, consectetur adipiscing elit."
                        " /home/baz/Pellentesque sem mauris")
                (buffer-string))))))

(ert-deftest test-with-sshd ()
  "Test the ping-pong compile function with a ad-hoc sshd server,
which listens at localhost:22000 via `/usr/sbin/sshd -p 22000'.

This test should be run manually due to extra work to start up sshd.

Identity files should be generated and copied to the server at first:
ssh-keygen -t rsa -b 4096 -f ./test/id_ppcompile_test
ssh-copy-id -p 22000 -i ./test/id_ppcompile_test localhost
"
  :tags '(sshd)
  (delete-directory "/tmp/hello-world-project/" 'recursive)

  (let ((enable-local-variables :all)
        (project-find-functions (lambda (dir) (cons 'test dir))))
    (find-file "./test/hello-world-project/hello-world.c")
    (ppcompile)
    (sleep-for 2) ; wait for finishing compiling, kinda silly
    (kill-buffer "hello-world.c")
    (switch-to-buffer "*compilation*")
    (next-error)
    (switch-to-buffer "hello-world.c") ; the source file should be targeted now!
    (should
     (= 6 (line-number-at-pos)))))
