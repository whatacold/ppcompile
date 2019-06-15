;;; ppcompile.el --- Ping-pong compile projects on remote machines -*- lexical-binding: t -*-

;; Author: Guangwang Huang
;; Maintainer: Guangwang Huang
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;

;;; Code:

; depends on rsync ssh expect(Tcl in general)
; host port user passwd srcdir dstdir compile-command

(require 'compile)

(defgroup ppcompile nil
  "Run a ping pong compilation to build remotely and fix errors locally."
  :group 'tools)

(defcustom ppcompile-ssh-host nil
  "Host of remote machine, used in rsync'ing and compiling."
  ; TODO add :type
  :group 'ppcompile)

(defcustom ppcompile-ssh-port 22
  "Port of remote machine."
  :group 'ppcompile)

(defcustom ppcompile-ssh-user nil
  "User for remote compilations."
  :group 'ppcompile)

;;; XXX rsync depends on ssh
(defcustom ppcompile-rsync-args '("--exclude=*.o"
 "--exclude=*.log"
 "--exclude=*.so"
 "--exclude=unittest_main"
 "--exclude=.git*"
 "--exclude=.ccls-cache*"
 "--exclude=.cquery_cached_index*"
 "--exclude=.vscode*"
 "--exclude=.svn*"
 "--exclude=tags"
 "--exclude=cscope.*") ; TODO define a exclude variable
  "Arguments to `rsync' command to sync project to remote host."
  :group 'ppcompile)

;;; TODO could use mapping to deduce
(defcustom ppcompile-rsync-dst-dir nil
  "Destination directory to rsync files into."
  :group 'ppcompile)

(defcustom ppcompile-compile-command nil
  "Compile command to build the project on the remote machine."
  :group 'ppcompile)

(defcustom ppcompile-path-map-list nil
  "A list of cons'es tells how to map remote paths to local paths."
  :group 'ppcompile)

(defconst ppcompile--with-password-script-path (concat (file-name-directory (buffer-file-name))
                                                       "with-password.exp")
  "The path of the helper expect script, with-password.exp")

(defun ppcompile--convert-path (buffer finish-msg)
  "Convert paths matching SRC to DST in current buffer."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (dolist (map ppcompile-path-map-list)
        (goto-char (point-min))
        (while (search-forward (car map) nil t)
          (replace-match (cdr map)))))))

(defun ppcompile--project-root ()
  "Find the root directory of current prject.

If `project-current' finds the root, return it;
or else fallback to the `.git' directory."
  (or (cdr (project-current))
      (locate-dominating-file default-directory ".git")
      default-directory)) ; TODO use lambda to exclude .git file

(defun ppcompile-ping ()
  "Rsync files from local machine to remote one."
  (let* ((default-directory (ppcompile--project-root))
         (process-environment (cons (format "PPCOMPILE_PASSWORD=%s"
                                            ppcompile-ssh-password)
                                    process-environment))
         (process-connection-type nil)
         rsync-args full-dst rc output)
    (message "p e: %s" process-environment)
    (setq rsync-args "-avz") ; TODO add
    (setq full-dst (format "%s@%s:%s"
                           ppcompile-ssh-user
                           ppcompile-ssh-host
                           ppcompile-rsync-dst-dir))
    (with-temp-buffer
      (setq rc (call-process "expect" nil (current-buffer) nil
                             ppcompile--with-password-script-path
                             "rsync"
                             "-e"
                             (format "ssh -p %d" ppcompile-ssh-port)
                             rsync-args
                             (expand-file-name default-directory)
                             full-dst))
      (setq output (buffer-substring-no-properties (point-min) (point-max))))
    (cons rc output)))

(defun ppcompile-pong ()
  "Compile projects remotely and map paths in the output."
  (let* ((default-directory (ppcompile--project-root))
         (compilation-environment (cons (format "PPCOMPILE_PASSWORD=%s"
                                                ppcompile-ssh-password)
                                        compilation-environment))
         compile-command)
    (save-some-buffers)
    (add-to-list 'compilation-finish-functions #'ppcompile--convert-path) ; XXX how to achieve this in an elegant way?
    (setq compile-command (format "expect %s ssh -p %d %s@%s %s"
                                  ppcompile--with-password-script-path
                                  ppcompile-ssh-port
                                  ppcompile-ssh-user
                                  ppcompile-ssh-host
                                  ppcompile-compile-command))
    (compilation-start compile-command)
                                        ;(compilation-start compile-command t) ; password may be needed, so comint mode
    ))

(defun ppcompile (&optional dont't-compile)
  (interactive "P")
  (let* ((rsync-result (ppcompile-ping)))
    (if (not (eq 0 (car rsync-result)))
        (message "Failed to rsync current project to the remote machine")
      (unless don't-compile
        (ppcompile-pong)))))

(provide 'ppcompile)

;;; ppcompile.el ends here