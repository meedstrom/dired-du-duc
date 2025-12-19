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

;; Add indexing to Dired-Du, via the Linux utility "duc".

;; Get duc:

;;   https://duc.zevv.nl/
;;   https://github.com/zevv/duc

;; Enable with

;;   (global-dired-du-duc-mode)

;; You'll also want to remove any call to `dired-du-mode' in your initfiles.

;; The global mode does three things:

;; 1. Start regularly indexing `dired-du-duc-directories'.

;; 2. Index every directory that the user actually visits in Dired,
;;    and when that is done, revert the buffer so it shows correct sizes.

;; 3. Manage turning `dired-du-mode' on in relevant buffers when duc is ready.
;;    Option `dired-du-duc-mode-predicate' can be configured to enable it
;;    always if you are fine with the slow "du" as a fallback.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'dired-du)

(defgroup dired-du-duc ()
  "Pre-cache size info for Dired-Du, via the Linux utility \"duc\"."
  :group 'dired-du
  :link '(url-link "https://github.com/zevv/duc")
  :link '(url-link "https://duc.zevv.nl/"))

(defcustom dired-du-duc-delay 3600
  "Seconds between each reindex of `dired-du-duc-directories'."
  :type 'number)

(defcustom dired-du-duc-directories '("/home" "/media" "/run/media" "/mnt" "/opt")
  "Directories that `global-dired-du-duc-mode' should regularly index."
  :type '(repeat directory))

(defvar dired-du-duc--process-dirs nil)
(defun dired-du-duc-index (dirs)
  "Run \"duc index\" on DIRS."
  (setq dirs (seq-filter #'file-readable-p
                         (ensure-list (or dirs default-directory))))
  (when dirs
    (let ((proc (apply #'start-process
                       "duc" " *duc*"
                       "duc" "index" "-v" dirs)))
      (push (cons proc dirs) dired-du-duc--process-dirs)
      (set-process-sentinel proc #'dired-du-duc--handle-done))
    (dolist (dir dirs)
      (let ((default-directory dir))
        (run-hooks 'dired-du-duc-index-hook)))))

(defvar dired-du-duc-index-hook nil
  "Hook run for every directory passed to `dired-du-duc-index'.
The directory is used as `default-directory'.
This can be used to run other programs such as `updatedb'.")

(defvar dired-du-duc-after-re-index-hook '(dired-revert)
  "Hook run in a Dired buffer just after duc finished indexing its dir.")

(defun dired-du-duc--handle-done (proc _event)
  "If PROC done, revert any buffers that show the newly indexed dirs.
Actually, run `dired-du-duc-after-re-index-hook' in those buffers,
which presumably includes the function `dired-revert'."
  (if (and (eq (process-status proc) 'exit)
           (eq (process-exit-status proc) 0))
      (let ((newly-indexed (alist-get proc dired-du-duc--process-dirs)))
        (setq dired-du-duc--process-dirs
              (assq-delete-all proc dired-du-duc--process-dirs))
        (cl-loop for (dir . buf) in dired-buffers
                 when (and (member dir newly-indexed)
                           (buffer-live-p buf))
                 do (with-current-buffer buf
                      (run-hooks 'dired-du-duc-after-re-index-hook))))
    (when (buffer-live-p (get-buffer " *duc*"))
      (display-buffer " *duc*"))
    (message "Unexpected sentinel invocation for process %S" proc)))

(defun dired-du-duc-local-p ()
  "Non-nil if current directory is on a local filesystem."
  (not (file-remote-p default-directory)))

(defun dired-du-duc-indexed-p ()
  "Non-nil if current directory has been indexed."
  (with-temp-buffer
    (if (eq 0 (call-process "duc" nil t nil "ls"))
        t
      (display-warning 'dired-du-duc (buffer-string)))))

(defcustom dired-du-duc-index-predicate #'dired-du-duc-local-p
  "Predicate for whether a directory should be indexed with duc."
  :type '(radio (function-item dired-du-duc-local-p)
                (function-item always)
                (function :tag "Custom predicate" :value (lambda ()))))

(defcustom dired-du-duc-mode-predicate #'dired-du-duc-indexed-p
  "Predicate for whether a Dired buffer should display recursive sizes.
The sizes are taken from duc if possible, or calculated anew with du."
  :type '(radio (function-item dired-du-duc-indexed-p)
                (function-item always)
                (function :tag "Custom predicate" :value (lambda ()))))

(defvar-local dired-du-duc--lighter " Dired-Du")
(define-minor-mode dired-du-duc-mode
  "Use du or duc to augment file size information.

You should not enable both `dired-du-mode' and `dired-du-duc-mode'.
They are the same, and both obey most user configurations for Dired-Du.

Unlike `dired-du-mode', the buffer-local `dired-du-duc-mode' entails
no global side effects.
To turn it on in all relevant buffers, configure
`dired-du-duc-mode-predicate' and enable `global-dired-du-duc-mode'.

To support Find-Dired and some other features, it is necessary to
enable `global-dired-du-duc-mode'.
See also `dired-du-on-find-dired-ok'.

-----"
  :lighter dired-du-duc--lighter
  (cond
   (dired-du-duc-mode
    (when (and (boundp 'ls-lisp-use-insert-directory-program)
               (null ls-lisp-use-insert-directory-program)
               (not global-dired-du-duc-mode))
      (display-warning
       'dired-du-duc "No ls-lisp support without `global-dired-du-duc-mode'"))
    (when dired-du-mode
      (dired-du-mode 0))
    (if (dired-du-duc-indexed-p)
        (setq-local dired-du-used-space-program '("duc" "ls -bD")
                    dired-du-duc--lighter " Dired-Du-Duc")
      (kill-local-variable 'dired-du-used-space-program)
      (kill-local-variable 'dired-du-duc--lighter)
      (lwarn 'dired-du-duc :debug "Using du without duc in %S" (current-buffer)))
    ;; Upstream has unnecessary checks on variable `dired-du-mode' everywhere.
    (setq-local dired-du-mode :dired-du-duc-pretending)
    (add-hook 'dired-before-readin-hook #'dired-du--drop-unexistent-files nil t)
    (add-hook 'dired-after-readin-hook #'dired-du--replace 'append t)
    (dired-du--replace)
    (add-function :around (local 'revert-buffer-function) #'dired-du--revert))

   (t
    (kill-local-variable 'dired-du-used-space-program)
    (kill-local-variable 'dired-du-duc--lighter)
    (remove-hook 'dired-before-readin-hook #'dired-du--drop-unexistent-files t)
    (remove-hook 'dired-after-readin-hook #'dired-du--replace t)
    (remove-function (local 'revert-buffer-function) #'dired-du--revert)
    (dired-revert))))

(defvar dired-du-duc--timer (timer-create))
(defun dired-du-duc--start-timer ()
  (cancel-timer dired-du-duc--timer)
  (setq dired-du-duc--timer
        (run-with-timer dired-du-duc-delay nil #'dired-du-duc--start-timer))
  (dired-du-duc-index dired-du-duc-directories))

(defun dired-du-duc--turn-on-for-find-dired (fn &rest args)
  "Apply Dired-Du advice on Find-Dired if `dired-du-duc-mode-predicate'."
  (if (funcall dired-du-duc-mode-predicate)
      (apply #'dired-du--find-dired-around fn args)
    (apply fn args)))

(defun dired-du-duc--turn-on ()
  "Maybe turn on `dired-du-duc-mode' in current buffer.
Maybe run \"duc index\" too."
  (when (derived-mode-p 'dired-mode)
    (when (funcall dired-du-duc-index-predicate)
      (dired-du-duc-index default-directory))
    (when (funcall dired-du-duc-mode-predicate)
      (dired-du-duc-mode))))

;;;###autoload
(define-globalized-minor-mode global-dired-du-duc-mode
  dired-du-duc-mode
  dired-du-duc--turn-on
  :require 'dired-du-duc
  (cond (global-dired-du-duc-mode
         (unless (executable-find "duc")
           (display-warning 'dired-du-duc "No executable \"duc\" in PATH"))
         (cl-loop for (_dir . buf) in dired-buffers
                  do (with-current-buffer buf
                       (when dired-du-mode
                         ;; Clean up upstream `dired-du-mode' global effects
                         (dired-du-mode 0))))
         (add-hook 'dired-after-readin-hook #'dired-du-duc--turn-on)
         (advice-add 'find-dired-sentinel :around #'dired-du-duc--turn-on-for-find-dired)
         (advice-add 'ls-lisp-handle-switches :override #'dired-du-ls-lisp-handle-switches)
         (dired-du-duc--start-timer))

        (t
         (remove-hook 'dired-after-readin-hook #'dired-du-duc--turn-on)
         (advice-remove 'find-dired-sentinel #'dired-du-duc--turn-on-for-find-dired)
         (advice-remove 'ls-lisp-handle-switches #'dired-du-ls-lisp-handle-switches)
         (cancel-timer dired-du-duc--timer))))

(provide 'dired-du-duc)

;;; dired-du-duc.el ends here
