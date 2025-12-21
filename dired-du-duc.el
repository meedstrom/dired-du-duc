;;; dired-du-duc.el --- Speed up dired-du with duc -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/dired-du-duc
;; Created:  2025-12-03
;; Keywords: files
;; Package-Requires: ((emacs "29.1") (dired-du "0.5.2"))

;;; Commentary:

;; This adapts Dired-Du to use "duc".  Get duc:

;;   https://duc.zevv.nl/
;;   https://github.com/zevv/duc

;; Enable with

;;   (global-dired-du-duc-mode)

;; You'll also want to remove any call to `dired-du-mode' in your initfiles.

;; The global mode does three things:

;; 1. Regularly run "duc index" on `dired-du-duc-directories'.

;; 2. Asynchronously run "duc index" each time a Dired buffer is opened.

;; 3. Turn `dired-du-duc-mode' on in relevant buffers when duc is ready.
;;    Option `dired-du-duc-mode-predicate' can be configured to enable it
;;    always if you are fine with the slow "du" as a fallback.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'dired-du)

(defgroup dired-du-duc ()
  "Faster Dired-Du, via the Linux utility \"duc\"."
  :group 'dired-du
  :link '(url-link "https://github.com/zevv/duc")
  :link '(url-link "https://duc.zevv.nl/"))


;;;; Local mode

(defun dired-du-duc-indexed-p ()
  "Non-nil if current directory has been indexed."
  (eq 0 (call-process "duc" nil nil nil "ls")))

(defun dired-du-duc-db-p ()
  "Non-nil if duc is in PATH and ~/.cache/duc/duc.db is writable.
Creates duc.db if it didn't exist."
  (and (executable-find "duc")
       (eq 0 (call-process "duc" nil nil nil "index" (null-device)))))

(defvar-local dired-du-duc-using-duc nil
  "Whether current buffer is using duc in place of du.")

;;;###autoload
(define-minor-mode dired-du-duc-mode
  "Show real directory sizes in Dired, using du or duc.

You should not enable both `dired-du-mode' and `dired-du-duc-mode'.
They are the same, and both obey most user configurations for Dired-Du.

Unlike `dired-du-mode', the buffer-local `dired-du-duc-mode' entails
no global side effects.
To turn it on in all relevant buffers, configure
`dired-du-duc-mode-predicate' and enable `global-dired-du-duc-mode'.

-----"
  :lighter (:eval (if dired-du-duc-using-duc " duc" " du"))
  (cond
   (dired-du-duc-mode
    (when (not (derived-mode-p 'dired-mode))
      (dired-du-duc-mode 0)
      (error "Not a Dired buffer"))
    (when (and (boundp 'ls-lisp-use-insert-directory-program)
               (null ls-lisp-use-insert-directory-program)
               (not global-dired-du-duc-mode))
      (display-warning 'dired-du-duc "No ls-lisp support without `global-dired-du-duc-mode'"))
    (when dired-du-mode
      (dired-du-mode 0))
    (if (and (dired-du-duc-db-p)
             (dired-du-duc-indexed-p))
        (setq-local dired-du-used-space-program '("duc" "ls -bD")
                    dired-du-duc-using-duc t)
      (kill-local-variable 'dired-du-used-space-program)
      (kill-local-variable 'dired-du-duc-using-duc)
      (lwarn 'dired-du-duc :debug "Falling back on du in %S" (current-buffer)))
    ;; Upstream does many gratuitous checks on variable `dired-du-mode'.
    (setq-local dired-du-mode :dired-du-duc-pretending-to-be-dired-du)
    ;; TODO: Maybe make one of these quieter, wrapping in `inhibit-message'
    (add-hook 'dired-before-readin-hook #'dired-du--drop-unexistent-files nil t)
    (add-hook 'dired-after-readin-hook #'dired-du--replace 90 t)
    (add-function :around (local 'revert-buffer-function) #'dired-du--revert)
    (dired-du--replace))

   (t
    (kill-local-variable 'dired-du-used-space-program)
    (kill-local-variable 'dired-du-duc-using-duc)
    (when (eq dired-du-mode :dired-du-duc-pretending-to-be-dired-du)
      (kill-local-variable 'dired-du-mode))
    (remove-hook 'dired-before-readin-hook #'dired-du--drop-unexistent-files t)
    (remove-hook 'dired-after-readin-hook #'dired-du--replace t)
    (remove-function (local 'revert-buffer-function) #'dired-du--revert)
    (when (derived-mode-p 'dired-mode)
      (revert-buffer)))))


;;;; Function `dired-du-duc-index'

(defvar dired-du-duc-before-index-functions nil
  "Hook run with one argument, the list of directories to index.
Called by `dired-du-duc-index' just after starting an async job.")

(defvar dired-du-duc-after-re-index-hook '(revert-buffer)
  "Hook run in a Dired buffer after duc finished indexing the directory.
As there may not be buffers for every directory indexed, this hook sees
fewer directories than `dired-du-duc-before-index-functions' does.")

(defvar dired-du-duc--process-dirs nil)
(defun dired-du-duc-index (dirs)
  "Run \"duc index\" on DIRS."
  (setq dirs
        ;; Prevent infinite loop from `dired-du-duc-after-re-index-hook' to
        ;; `dired-du-duc--try-turn-on' back to `dired-du-duc-index'.
        (cl-loop with reserved = (seq-mapcat #'cdr dired-du-duc--process-dirs)
                 for dir in dirs
                 when (and (not (member dir reserved))
                           (file-readable-p dir))
                 collect dir))
  (when dirs
    (let ((proc (apply #'start-process
                       "duc" " *duc*"
                       "duc" "index" "-v" dirs)))
      (push (cons proc dirs) dired-du-duc--process-dirs)
      (set-process-sentinel proc #'dired-du-duc--handle-done))
    (run-hook-with-args 'dired-du-duc-before-index-functions dirs)))

(defun dired-du-duc--handle-done (proc _event)
  "If PROC done, revert any buffers that show the newly indexed dirs.
Actually, run `dired-du-duc-after-re-index-hook' in those buffers,
which presumably includes the function `revert-buffer'."
  (if (and (eq (process-status proc) 'exit)
           (eq (process-exit-status proc) 0))
      (let ((newly-indexed (alist-get proc dired-du-duc--process-dirs)))
        (cl-loop for (dir . buf) in dired-buffers
                 when (and (member dir newly-indexed)
                           (buffer-live-p buf))
                 do (with-current-buffer buf
                      (when (derived-mode-p 'dired-mode)
                        (run-hooks 'dired-du-duc-after-re-index-hook))))
        (setq dired-du-duc--process-dirs
              (assq-delete-all proc dired-du-duc--process-dirs)))
    (when (buffer-live-p (get-buffer " *duc*"))
      (display-buffer " *duc*"))
    (error "Unexpected sentinel invocation for process %S" proc)))


;;;; Global mode

(defcustom dired-du-duc-delay 3600
  "Seconds between each indexing of `dired-du-duc-directories'."
  :type 'number)

(defcustom dired-du-duc-directories '("/home")
  "Directories that `global-dired-du-duc-mode' should regularly index."
  :type '(repeat directory))

(defcustom dired-du-duc-index-predicate 'dired-du-duc-local-p
  "Predicate for whether a directory should be indexed with duc."
  :type '(radio (function-item dired-du-duc-local-p)
                (function-item always)
                (function-item ignore)
                (function :tag "Custom predicate" :value (lambda ()))))

(defcustom dired-du-duc-mode-predicate 'dired-du-duc-indexed-p
  "Predicate for whether a Dired buffer should display recursive sizes.
The sizes are taken from duc if possible, or calculated anew with du.
Used by `global-dired-du-duc-mode'."
  :type '(radio (function-item dired-du-duc-indexed-p)
                (function-item always)
                (function :tag "Custom predicate" :value (lambda ()))))

(defun dired-du-duc-local-p ()
  "Non-nil if current directory is on a local filesystem."
  (not (file-remote-p default-directory)))

(defvar dired-du-duc--timer (timer-create))
(defun dired-du-duc--start-timer ()
  "Index `dired-du-duc-directories' and schedule doing it again."
  (cancel-timer dired-du-duc--timer)
  (setq dired-du-duc--timer
        (run-with-timer dired-du-duc-delay nil #'dired-du-duc--start-timer))
  (dired-du-duc-index dired-du-duc-directories))

(defun dired-du-duc--turn-on-for-find-dired (fn &rest args)
  "Maybe apply Dired-Du advice to Find-Dired.
FN is presumably `find-dired-sentinel' and ARGS its args."
  (if (funcall dired-du-duc-mode-predicate)
      (apply #'dired-du--find-dired-around fn args)
    (apply fn args)))

(defun dired-du-duc--try-turn-on ()
  "Maybe turn on `dired-du-duc-mode' in current buffer.
Maybe run `dired-du-duc-index' on current directory."
  (when (derived-mode-p 'dired-mode)
    (when (funcall dired-du-duc-index-predicate)
      (dired-du-duc-index default-directory))
    (when (funcall dired-du-duc-mode-predicate)
      (dired-du-duc-mode))))

(defvar dired-du-duc--overridden-lighter nil
  "Dired-du lighter before enabling `global-dired-du-duc-mode'.")

;;;###autoload
(define-globalized-minor-mode global-dired-du-duc-mode
  dired-du-duc-mode
  dired-du-duc--try-turn-on
  :require 'dired-du-duc
  (cond
   (global-dired-du-duc-mode
    (unless (executable-find "duc")
      (display-warning 'dired-du-duc "No executable \"duc\" in PATH"))
    (cl-loop for (_dir . buf) in dired-buffers
             do (with-current-buffer buf
                  (when dired-du-mode
                    ;; Clean up `dired-du-mode' global effects
                    (dired-du-mode 0))))
    (advice-add 'find-dired-sentinel :around #'dired-du-duc--turn-on-for-find-dired)
    (advice-add 'ls-lisp-handle-switches :override #'dired-du-ls-lisp-handle-switches)
    (add-hook 'dired-after-readin-hook #'dired-du-duc--try-turn-on)
    (dired-du-duc--start-timer)
    ;; Delete the " Dired-du" mode line lighter.
    (unless dired-du-duc--overridden-lighter
      (let ((cell (assq 'dired-du-mode minor-mode-alist)))
        (setq dired-du-duc--overridden-lighter (cdr cell))
        (when dired-du-duc--overridden-lighter
          (setcdr cell `(:eval (if (eq dired-du-mode
                                       :dired-du-duc-pretending-to-be-dired-du)
                                   nil
                                 ,dired-du-duc--overridden-lighter)))))))

   (t
    (advice-remove 'find-dired-sentinel #'dired-du-duc--turn-on-for-find-dired)
    (advice-remove 'ls-lisp-handle-switches #'dired-du-ls-lisp-handle-switches)
    (remove-hook 'dired-after-readin-hook #'dired-du-duc--try-turn-on)
    (cancel-timer dired-du-duc--timer)
    ;; Restore the " Dired-du" mode line lighter.
    (when dired-du-duc--overridden-lighter
      (let ((cell (assq 'dired-du-mode minor-mode-alist)))
        (when cell (setcdr cell dired-du-duc--overridden-lighter)))
      (setq dired-du-duc--overridden-lighter nil)))))

(provide 'dired-du-duc)

;;; dired-du-duc.el ends here
