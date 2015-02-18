;;; racket-profile.el

;; Copyright (c) 2013-2015 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

(require 'cl-lib)
(require 'dash)
(require 'racket-edit)

(defvar racket--profile-results nil)
(defvar racket--profile-sort-col 1)

(defun racket-profile ()
  "Run with profiling enabled. Navigate results."
  (interactive)
  (racket--do-run 'profile)
  (racket--eval/sexpr "(void") ;; FIXME: hack in case REPL just started
  (setq racket--profile-results (racket--eval/sexpr ",get-profile"))
  (setq racket--profile-sort-col 1)
  (with-current-buffer (get-buffer-create "*Racket Profile*")
    (racket-profile-mode)
    (racket--profile-sort)
    (pop-to-buffer (current-buffer))))

(defun racket--profile-sort ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (setq racket--profile-sort-col
        (cl-case racket--profile-sort-col
          (0 1)
          (t 0)))
  ;; TODO: Would be nice to set the Count and Time column widths based
  ;; on max values.
  (let ((xss (sort (cl-copy-list racket--profile-results)
                    (lambda (a b) (> (nth racket--profile-sort-col a)
                                     (nth racket--profile-sort-col b))))))
    (dolist (xs xss)
      ;; TODO: Insert property with file and locations, for RET to use.
      (insert (propertize (apply #'format "%8d %6d %-20s %s\n" xs)
                          'racket-profile-location
                          (cl-cdddr xs)))))
  (setq header-line-format (format " %8s %6s %-20s %s"
                                   "Count" "Time" "Name" "File"))
  (read-only-mode 1)
  (goto-char (point-min)))

(defvar racket--profile-visit-overlay nil)

(defun racket--profile-visit ()
  (interactive)
  (let ((win (selected-window)))
    (-let [(file beg end) (get-text-property (point) 'racket-profile-location)]
      (find-file-other-window file)
      (setq racket--profile-visit-overlay (make-overlay beg end (current-buffer)))
      (overlay-put racket--profile-visit-overlay 'face 'next-error)
      (add-hook 'pre-command-hook #'racket--profile-remove-overlay)
      (select-window win))))

(defun racket--profile-remove-overlay ()
  (delete-overlay racket--profile-visit-overlay)
  (remove-hook 'pre-command-hook #'racket--profile-remove-overlay))

(defun racket--profile-quit ()
  (interactive)
  (setq racket--profile-results nil)
  (quit-window))

(defvar racket-profile-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m nil)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("q"   racket--profile-quit)
            ("n"   forward-line)
            ("p"   previous-line)
            ("RET" racket--profile-visit)
            (","   racket--profile-sort)))
    m)
  "Keymap for Racket Profile mode.")

(define-derived-mode racket-profile-mode fundamental-mode
  "RacketProfile"
  "Major mode for view Racket profiler results.

\\{racket-profile-mode-map}"
  (setq show-trailing-whitespace nil))

(provide 'racket-profile)

;; racket-profile.el ends here
