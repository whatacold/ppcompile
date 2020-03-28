;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (eval setq project-find-functions #'(lambda (dir) (cons 'test dir)))
  (ppcompile-remote-compile-command . "make -C /tmp/hello-world-project")
  (eval setq ppcompile-path-mapping-alist
        `(("/tmp/" . ,(file-name-directory (directory-file-name default-directory)))))
  (ppcompile-rsync-dst-dir . "/tmp/")
  (ppcompile-ssh-port . 22000)
  (ppcompile-ssh-host . "localhost")
  (eval setq ppcompile-ssh-additional-args (format "-i %sid_ppcompile_test" (file-name-directory (directory-file-name default-directory))))))
