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

;;; Commentary:

;; Add indexing to Dired-Du, via the Linux utility \"duc\".

;; Get duc:

;;   https://duc.zevv.nl/
;;   https://github.com/zevv/duc

;; Enable with (global-dired-du-duc-mode).
;; You'll also want to remove any call to `dired-du-mode' in your initfiles.

;; The global mode does three things:

;; 1. Start regularly indexing a configurable set of directories.
;; 2. Index every directory that the user actually visits in Dired,
;;    and when that index is done, revert the buffer so it shows correct sizes.
;;    (This is also repeated every time it reverts for any reason.)
;; 3. Manage turning `dired-du-mode' on or off in relevant buffers,
;;    in case you prefer to fall back on nothing when the index isn't ready,
;;    rather than fall back on the slow "du".

;; To configure this, type:

;; M-x customize-group RET dired-du-duc RET

;;; Code:

(defgroup dired-du-duc ()
  "Add indexing to Dired-Du, via the Linux utility \"duc\".

Get duc:

  https://duc.zevv.nl/
  https://github.com/zevv/duc"
  :group 'dired-du)

(defvar dired-du-duc--timer (timer-create))
(defcustom dired-du-duc-interval 3600
  "Seconds between each reindex of `dired-du-duc-roots'."
  :type 'number)

(defcustom dired-du-duc-roots '("/home" "/media" "/run/media" "/mnt" "/opt")
  "Directories that `global-dired-du-duc-mode' should regularly index."
  :type '(repeat directory))

(defvar dired-du-duc--process-dirs nil)
(defun dired-du-duc-index (&rest dirs)
  (when (setq dirs (seq-filter #'file-readable-p (or dirs dired-du-duc-roots)))
    (let ((proc (apply #'start-process
                       "duc" " *duc*"
                       "duc" "index" "-v" dirs)))
      (set-process-sentinel proc #'dired-du-duc--handle-done)
      (push (cons proc dirs) dired-du-duc--process-dirs))))

(defun dired-du-duc--handle-done (proc _event)
  "Revert any Dired buffers of the dirs newly indexed by PROC."
  (let ((newly-indexed (alist-get proc dired-du-duc--process-dirs)))
    (cl-loop for (dir . buf) in dired-buffers
             when (and (member dir newly-indexed)
                       (buffer-live-p buf))
             do (with-current-buffer buf
                  (dired-revert)))
    (assq-delete-all proc dired-du-duc--process-dirs)))

(defun dired-du-duc-indexed-p ()
  "Non-nil if current directory has been indexed."
  (eq 0 (call-process "duc" nil nil nil "ls")))

(defun dired-du-duc-not-remote-p ()
  "Non-nil if current directory is on a local filesystem."
  (not (file-remote-p default-directory)))

(defcustom dired-du-duc-du-mode-predicate #'dired-du-duc-indexed-p
  "Predicate for whether a Dired buffer should enable `dired-du-mode'."
  :type '(radio (function-item dired-du-duc-indexed-p)
                (function-item :tag "Always enable dired-du-mode" always)
                (function :tag "Custom predicate" :value (lambda ()))))

(defcustom dired-du-duc-directory-predicate #'dired-du-duc-not-remote-p
  "Predicate for whether a directory should be indexed."
  :type '(radio (function-item dired-du-duc-not-remote-p)
                (function-item :tag "Always index any directory" always)
                (function :tag "Custom predicate" :value (lambda ()))))

;;;###autoload
(define-minor-mode dired-du-duc-mode
  "Du duktig!"
  (cond ((and (derived-mode-p 'dired-mode) dired-du-duc-mode)
         (dired-du-mode 0)
         (kill-local-variable 'dired-du-used-space-program)
         (when (funcall dired-du-duc-directory-predicate)
           (dired-du-duc-index default-directory))
         (when (funcall dired-du-duc-du-mode-predicate)
           (when (dired-du-duc-indexed-p)
             (setq-local dired-du-used-space-program '("duc" "ls -bD")))
           (dired-du-mode)
           ;; Undo a confused side effect in dired-du that adds a hook
           ;; globally every time its buffer-local mode is enabled.
           (remove-hook 'dired-mode-hook #'dired-du-mode))
         ;; Make it so that when this Dired buffer refreshes for any reason,
         ;; it'll also re-run the above checks, so it can potentially turn on
         ;; `dired-du-mode' even if it didn't before.
         (add-hook 'dired-after-readin-hook #'dired-du-duc-mode nil t))

        ((derived-mode-p 'dired-mode)
         (kill-local-variable 'dired-du-used-space-program)
         (dired-du-mode 0)
         (remove-hook 'dired-mode-hook #'dired-du-duc-mode t))))

(defun dired-du-duc--turn-on ()
  (when (derived-mode-p 'dired-mode)
    (dired-du-duc-mode)))

;;;###autoload
(define-globalized-minor-mode global-dired-du-duc-mode
  dired-du-duc-mode
  dired-du-duc--turn-on
  (cancel-timer dired-du-duc--timer)
  (when global-dired-du-duc-mode
    (setq dired-du-duc--timer
          (run-with-timer 0 dired-du-duc-interval #'dired-du-duc-index))))

(provide 'dired-du-duc)

;;; dired-du-duc.el ends here
