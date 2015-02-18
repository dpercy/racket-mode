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
(require 'racket-edit)

(defvar racket--profile-results nil)
(defvar racket--profile-sort-col 1)

(defun racket-profile ()
  "Run with profiling enabled. Navigate results."
  (interactive)
  (when (eq major-mode 'racket-mode)
    (racket--do-run 'profile)
    (setq racket--profile-results (racket--eval/sexpr ",get-profile"))
    (setq racket--profile-sort-col 1)
    (with-current-buffer (get-buffer-create "*Racket Profile*")
      (racket-profile-mode)
      (racket--profile-sort)
      (pop-to-buffer (current-buffer)))))

(defun racket--profile-sort ()
  "Toggle sort between Count and Time."
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (setq truncate-lines t) ;let run off right edge
  (setq racket--profile-sort-col
        (cl-case racket--profile-sort-col
          (0 1)
          (t 0)))
  ;; TODO: Would be nice to set the Count and Time column widths based
  ;; on max values.
  (setq header-line-format
        (format " %8s %6s %-20.20s %s"
                (if (= 0 racket--profile-sort-col) "COUNT" "Count")
                (if (= 1 racket--profile-sort-col) "TIME" "Time")
                "Name"
                "File"))
  (insert (mapconcat (lambda (xs)
                       (cl-destructuring-bind (count time name file beg end) xs
                         (propertize (format "%8d %6d %-20.20s %s"
                                             count time name file)
                                     'racket-profile-location
                                     (list file beg end))))
                     (sort (cl-copy-list racket--profile-results)
                           (lambda (a b) (> (nth racket--profile-sort-col a)
                                            (nth racket--profile-sort-col b))))
                     "\n"))
  (read-only-mode 1)
  (goto-char (point-min)))

(defvar racket--profile-overlay-this nil)
(defvar racket--profile-overlay-that nil)

(defun racket--profile-visit ()
  (interactive)
  (let ((win  (selected-window))
        (prop (get-text-property (point) 'racket-profile-location)))
    (when prop
      (setq racket--profile-overlay-this
            (make-overlay (save-excursion (beginning-of-line) (point))
                          (save-excursion (end-of-line) (point))
                          (current-buffer)))
      (overlay-put racket--profile-overlay-this 'face 'next-error)
      (cl-destructuring-bind (file beg end) prop
        (find-file-other-window file)
        (setq racket--profile-overlay-that (make-overlay beg end (current-buffer)))
        (overlay-put racket--profile-overlay-that 'face 'next-error)
        (add-hook 'pre-command-hook #'racket--profile-remove-overlay)
        (select-window win)))))

(defun racket--profile-remove-overlay ()
  (delete-overlay racket--profile-overlay-this)
  (delete-overlay racket--profile-overlay-that)
  (remove-hook 'pre-command-hook #'racket--profile-remove-overlay))

(defun racket--profile-next ()
  (interactive)
  (forward-line 1)
  (racket--profile-visit))

(defun racket--profile-prev ()
  (interactive)
  (forward-line -1)
  (racket--profile-visit))

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
            ("n"   racket--profile-next)
            ("p"   racket--profile-prev)
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
