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

;; This package tries to ease the workflow that consists of coding locally,
;; compiling remotely, and fixing errors with `next-error' locally.
;; and compiling remotely, it depends on built-in packages: auth-source,
;; project , files-x and compile, and external programs: rsync, ssh, expect.

;;; Code:

(require 'compile)
(require 'auth-source)
(require 'project)
(require 'files-x)

(defgroup ppcompile nil
  "Run a ping pong compilation to build remotely and fix errors locally."
  :group 'tools)

(defcustom ppcompile-ssh-host nil
  "Host of the remote machine, where remote compilation runs."
  :type 'string
  :group 'ppcompile)

(defcustom ppcompile-ssh-port 22
  "SSH port of the remote machine."
  :type 'integer
  :group 'ppcompile)

(defcustom ppcompile-ssh-user (user-login-name)
  "SSH user to login for remote compilations."
  :type 'string
  :group 'ppcompile)

(defcustom ppcompile-ssh-additional-args ""
  "Additional arguments for `ssh'."
  :type 'string
  :group 'ppcompile)

(defcustom ppcompile-rsync-exclude-list '("*.o"
                                          ".git*"
                                          ".svn*")
  "Files that `rsync' should exclude.
See `--exclude' option of `rsync' for the syntax."
  :type '(repeat string)
  :group 'ppcompile)

(defcustom ppcompile-rsync-additional-args "-az"
  "Arguments for `rsync', in addition to `ppcompile-rsync-exclude-list'.
Do not contain spaces in the value."
  :type 'string
  :group 'ppcompile)

(defcustom ppcompile-rsync-dst-dir nil
  "The destination containing directory to `rsync' files into."
  :type 'string
  :group 'ppcompile)

(defcustom ppcompile-remote-compile-command nil
  "Compile command to build the project on the remote machine."
  :type 'string
  :group 'ppcompile)

(defcustom ppcompile-path-mapping-alist nil
  "A list of cons'es tells how to map remote paths to local paths.
Each cons cell's key is remote path, and value is local path, all paths
should be in absolute path."
  :type '(alist :key-type string :value-type string)
  :group 'ppcompile)

(defconst ppcompile--with-password-script-path
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "with-password.exp")
  "The path of the helper expect script `with-password.exp'.")

(defvar ppcompile--current-buffer nil
  "Internal variable to keep current buffer, in order to fetch buffer-local variables.")

(defvar ppcompile--debug nil
  "If non-nil, log additional messages while using.")

(defun ppcompile--replace-path (buffer _finish-msg)
  "Replace paths in BUFFER, according to `ppcompile-path-mapping-alist'.
Argument _FINISH-MSG is a string describing how the process finished."
  (let ((path-mapping-list (with-current-buffer ppcompile--current-buffer
                             ppcompile-path-mapping-alist)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            from to)
        (dolist (map path-mapping-list)
          ; ensure both `from' & `to' ending with /
          (setq from (car map))
          (when (/= ?/ (aref from (1- (length from))))
            (setq from (concat from "/")))
          (setq to (cdr map))
          (when (/= ?/ (aref to (1- (length to))))
            (setq to (concat to "/")))
          (goto-char (point-min))
          (while (search-forward from nil t)
            (replace-match to)))))))

(defun ppcompile--project-root ()
  "Find the root directory of current project.
If `project-current' finds the root, return it;
or else fallback to use `git' root directory containing `.git'."
  (or (cdr (project-current))
      (locate-dominating-file default-directory (lambda (dir)
                                                  (file-directory-p (expand-file-name ".git" dir))))
      default-directory))

(defun ppcompile--ping ()
  "Rsync current project from local machine to remote one."
  (let* ((default-directory (ppcompile--project-root))
         (process-environment (cons (format "PPCOMPILE_PASSWORD=%s"
                                            (ppcompile-get-ssh-password))
                                    process-environment))
         (rsync-args (mapcar (lambda (pattern) (format "--exclude=%s" pattern))
                             ppcompile-rsync-exclude-list))
         (project-path (expand-file-name default-directory))
         (rsync-status 1)
         rsync-output)
    (push (format "--rsh=ssh -p %d %s"
                  ppcompile-ssh-port
                  ppcompile-ssh-additional-args)
          rsync-args)
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
    (setq rsync-args (nreverse rsync-args))

    (when ppcompile--debug
      (message "ppcompile ping command: expect %s rsync %s"
               ppcompile--with-password-script-path
               (mapconcat #'(lambda (arg) (format "'%s'" arg))
                          rsync-args " ")))
    (with-temp-buffer
      (setq rsync-status (apply #'call-process "expect" nil (current-buffer) nil
                                ppcompile--with-password-script-path
                                "rsync"
                                rsync-args))
      (setq rsync-output (buffer-substring-no-properties (point-min) (point-max))))
    (cons rsync-status rsync-output)))

(defun ppcompile--pong ()
  "Compile current project remotely.
And replace remote paths with local ones in the output."
  (let* ((default-directory (ppcompile--project-root))
         (compilation-environment (cons (format "PPCOMPILE_PASSWORD=%s"
                                                (ppcompile-get-ssh-password))
                                        compilation-environment))
         compile-command)
    (save-some-buffers)
    (setq ppcompile--current-buffer (current-buffer))
    (add-to-list 'compilation-finish-functions #'ppcompile--replace-path) ; XXX how to achieve this in an elegant way?
    (setq compile-command (format "expect %s ssh -p %d %s %s@%s %s"
                                  ppcompile--with-password-script-path
                                  ppcompile-ssh-port
                                  ppcompile-ssh-additional-args
                                  ppcompile-ssh-user
                                  ppcompile-ssh-host
                                  ppcompile-remote-compile-command))
    (when ppcompile--debug
      (message "ppcompile pong command: %s" compile-command))
    (compilation-start compile-command)))

;;;###autoload
(defun ppcompile (&optional dont-pong)
  "Ping-pong compile current project.

Where ping means rsync the project to a remote machine,
and pong means compiling remotely and get the
compilation output, including errors, back.

If DONT-PONG is not nil, it will only rsync the project."
  (interactive "P")
  (let* ((rsync-result (ppcompile--ping)))
    (if (not (eq 0 (car rsync-result)))
        (message "Failed to rsync current project, error: %s" (cdr rsync-result))
      (unless dont-pong
        (ppcompile--pong)))))

;;;###autoload
(defun ppcompile-toggle-debug ()
  "Toggle debugging."
  (interactive)
  (setq ppcompile--debug (not ppcompile--debug))
  (message "ppcompile debug %s" (if ppcompile--debug
                                    "on"
                                  "off")))

;;;###autoload
(defun ppcompile-get-ssh-password (&optional interactive-p)
  "Get SSH password using auth-source if `INTERACTIVE-P' is non nil.

nil returned if no password configured."
  (interactive "p")
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
    (when (functionp secret)
      (setq secret (funcall secret)))
    (when (not (stringp secret))
      (setq secret ""))
    (when (and ppcompile--debug (= 0 (length secret)))
      (message "No ppcompile password for the current project!"))
    (if interactive-p
        (message "ppcompile password for the current project: '%s'" secret)
      secret)))

;;;###autoload
(defun ppcompile-config-project ()
  "Guide you to configure variables in `.dir-locals.el' in the project root."
  (interactive)
  (save-excursion
    (let ((default-directory (ppcompile--project-root))
          host port user dst-dir compile-command modified-p)
      (setq host (read-from-minibuffer "[ppcompile] ssh host: "))
      (setq port (string-to-number (read-from-minibuffer "[ppcompile] ssh port: ")))
      (setq user (read-from-minibuffer "[ppcompile] ssh user: "))
      (setq dst-dir (read-from-minibuffer "[ppcompile] remote containing directory to rsync it: "))
      (setq compile-command (read-from-minibuffer
                             "[ppcompile] compile command (`M-n' to get started): "
                             nil nil nil nil
                             (format "make -C %s" dst-dir)))
      (when (> (length host) 0)
        (setq modified-p t)
        (add-dir-local-variable nil 'ppcompile-ssh-host host))
      (when (> port 0)
        (setq modified-p t)
        (add-dir-local-variable nil 'ppcompile-ssh-port port))
      (when (> (length user) 0)
        (setq modified-p t)
        (add-dir-local-variable nil 'ppcompile-ssh-user user))
      (when (> (length dst-dir) 0)
        (setq modified-p t)
        (add-dir-local-variable nil 'ppcompile-rsync-dst-dir dst-dir)

        ; FIXME may have duplicates
        (push (cons dst-dir (expand-file-name (concat default-directory "/../")))
              ppcompile-path-mapping-alist)
        (add-dir-local-variable nil 'eval `(setq ppcompile-path-mapping-alist
                                                 ',ppcompile-path-mapping-alist)))
      (when (> (length compile-command) 0)
        (setq modified-p t)
        (add-dir-local-variable nil 'ppcompile-remote-compile-command compile-command))

      (when modified-p
        (when (y-or-n-p (format "Save %s.dir-locals.el? " default-directory))
          (save-buffer))
        (bury-buffer)))))

(provide 'ppcompile)

;;; ppcompile.el ends here