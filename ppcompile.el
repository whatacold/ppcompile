;;; ppcompile.el --- Ping-pong compile projects on remote machines -*- lexical-binding: t -*-

;; Author: Guangwang Huang <whatacold@gmail.com>
;; Maintainer: Guangwang Huang
;; URL: https://github.com/whatacold/ppcompile
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
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

;; This package tries to ease the workflow of development locally
;; and compiling remotely, it depends on built-in packages: auth-source,
;; project and compile, and external programs: rsync, ssh, expect.

;;; Code:

(require 'compile)
(require 'auth-source)
(require 'project)

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

  "User for remote compilations."
(defcustom ppcompile-ssh-user (user-login-name)
  :group 'ppcompile)

(defcustom ppcompile-rsync-exclude-list '("*.o"
                                          ".git*"
                                          ".svn*")
  "Files the `rsync' should exclude."
  :group 'ppcompile)

(defcustom ppcompile-rsync-additional-args "-az"
  "Arguments for `rsync', in addition to `ppcompile-rsync-exclude-list'."
  :group 'ppcompile)

;;; TODO use mapping to deduce it?
(defcustom ppcompile-rsync-dst-dir nil
  "Destination directory to rsync files into."
  :group 'ppcompile)

(defcustom ppcompile-remote-compile-command nil
  "Compile command to build the project on the remote machine."
  :group 'ppcompile)

(defcustom ppcompile-path-mapping-alist nil
  "A list of cons'es tells how to map remote paths to local paths.
All paths should be in absolute path."
  :group 'ppcompile)

(defconst ppcompile--with-password-script-path (concat (file-name-directory (buffer-file-name))
                                                       "with-password.exp")
  "The path of the helper expect script, with-password.exp")

(defvar ppcompile--current-buffer nil
  "Internal variable to keep current buffer, in order to fetch buffer-local variables.")

(defun ppcompile--convert-path (buffer finish-msg)
  "Convert paths matching SRC to DST in current `BUFFER'."
  (let ((path-mapping-list (with-current-buffer ppcompile--current-buffer
                             ppcompile-path-mapping-alist)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (dolist (map path-mapping-list)
          (goto-char (point-min))
          (while (search-forward (car map) nil t)
            (replace-match (cdr map))))))))

(defun ppcompile--project-root ()
  "Find the root directory of current prject.

If `project-current' finds the root, return it;
or else fallback to the `.git' directory."
  (or (cdr (project-current))
      (locate-dominating-file default-directory #'(lambda (dir)
                                                    (file-directory-p (expand-file-name ".git" dir))))
      default-directory))

(defun ppcompile--get-ssh-password ()
  "Get ssh password using auth-source."
  (let ((secret
         (plist-get
          (nth 0
               (auth-source-search :max 1
                                   :host ppcompile-ssh-host
                                   :user ppcompile-ssh-user
                                   ;; secrets.el wouldn’t accept a number
                                   :port (if (numberp ppcompile-ssh-port)
                                             (number-to-string ppcompile-ssh-port)
                                           ppcompile-ssh-port)
                                   :require '(:secret)))
          :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun ppcompile--ping ()
  "Rsync files from local machine to remote one."
  (let* ((default-directory (ppcompile--project-root))
         (process-environment (cons (format "PPCOMPILE_PASSWORD=%s"
                                            (ppcompile--get-ssh-password))
                                    process-environment))
         (rsync-args (mapcar #'(lambda (pattern) (format "--exclude=%s" pattern))
                             ppcompile-rsync-exclude-list))
         (project-path (expand-file-name default-directory))
         (rsync-status 1)
         rsync-output)
    (push (format "--rsh=ssh -p %d" ppcompile-ssh-port) rsync-args)
    (when ppcompile-rsync-additional-args
      (push ppcompile-rsync-additional-args rsync-args))

    ;; trailing slash makes a difference for rsync, trim it if any.
    (if (equal (substring project-path
                          (1- (length project-path))) "/" )
        (setq project-path (substring project-path 0 (1- (length project-path)))))
    (push project-path rsync-args)

    (push (format "%s@%s:%s"
                  ppcompile-ssh-user
                  ppcompile-ssh-host
                  ppcompile-rsync-dst-dir)
          rsync-args)
    (with-temp-buffer
      (setq rsync-status (apply #'call-process "expect" nil (current-buffer) nil
                                ppcompile--with-password-script-path
                                "rsync"
                                (nreverse rsync-args)))
      (setq rsync-output (buffer-substring-no-properties (point-min) (point-max))))
    (cons rsync-status rsync-output)))

(defun ppcompile--pong ()
  "Compile projects remotely and map paths in the output."
  (let* ((default-directory (ppcompile--project-root))
         (compilation-environment (cons (format "PPCOMPILE_PASSWORD=%s"
                                                (ppcompile--get-ssh-password))
                                        compilation-environment))
         compile-command)
    (save-some-buffers)
    (setq ppcompile--current-buffer (current-buffer))
    (add-to-list 'compilation-finish-functions #'ppcompile--convert-path) ; XXX how to achieve this in an elegant way?
    (setq compile-command (format "expect %s ssh -p %d %s@%s %s"
                                  ppcompile--with-password-script-path
                                  ppcompile-ssh-port
                                  ppcompile-ssh-user
                                  ppcompile-ssh-host
                                  ppcompile-remote-compile-command))
    (compilation-start compile-command)))

;;;###autoload
(defun ppcompile (&optional dont-pong)
  "Ping-pong compile current project.

Where ping means rsync the project to a remote machine,
and pong means compiling on the remote machine and get the
compilation output, including errors, back.

If `DONT-PONG' is not nil, it will only rsync the project."
  (interactive "P")
  (let* ((rsync-result (ppcompile--ping)))
    (if (not (eq 0 (car rsync-result)))
        (message "Failed to rsync current project to the remote machine")
      (unless dont-pong
        (ppcompile--pong)))))

(provide 'ppcompile)

;;; ppcompile.el ends here