;;; racket-make-doc.el --- Major mode for Racket language.

;; Copyright (c) 2013-2015 by Greg Hendershott.

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

;;; Generate a markdown format file for Reference documentation.

(require 'racket-mode)
(require 'dash)

;;; Top

(defvar racket-make-doc/Reference.md
  (expand-file-name "Reference.md"
                    (file-name-directory (or load-file-name (buffer-file-name)))))

(defun racket-make-doc/write-reference-file ()
  (interactive)
  (let ((backup-inhibited t))
    (with-temp-buffer
      (insert (racket-make-doc/reference))
      (write-file racket-make-doc/Reference.md nil))))

(defun racket-make-doc/reference ()
  (concat "# racket-mode Reference\n"
          "\n"
          "- [Commands](#commands)\n"
          "- [Variables](#variables)\n"
          "- [Faces](#faces)\n"
          "\n"
          "---\n\n"
          "## Commands\n\n"
          (racket-make-doc/commands)
          "---\n\n"
          "## Variables\n\n"
          "> Note: You may also set these via Customize.\n\n"
          (racket-make-doc/variables)
          "---\n\n"
          "## Faces\n\n"
          "> Note: You may also set these via Customize.\n\n"
          (racket-make-doc/faces)))

;;; Commands

(defconst racket-make-doc/commands
  '(racket-run
    racket-test
    racket-racket
    racket-raco-test
    racket-send-region
    racket-send-definition
    racket-send-last-sexp
    racket-visit-definition
    racket-visit-module
    racket-unvisit
    racket-describe
    racket-fold-all-tests
    racket-unfold-all-tests
    racket-expand-region
    racket-expand-definition
    racket-expand-last-sexp
    racket-expand-again
    racket-gui-macro-stepper
    racket-tidy-requires
    racket-trim-requires
    racket-base-requires
    racket-doc
    racket-newline-and-indent
    racket-indent-or-complete
    racket-indent-line
    racket-open-require-path
    racket-find-collection
    racket-smart-open-bracket
    racket-cycle-paren-shapes
    racket-backward-up-list)
  "Commands to include in the Reference.")

(defun racket-make-doc/commands ()
  (apply #'concat
         (mapcar #'racket-make-doc/command racket-make-doc/commands)))

(defun racket-make-doc/command (symbol)
  (concat (format "### %s\n" symbol)
          (racket-make-doc/bindings-as-kbd symbol)
          (-> (or (documentation symbol) "No documentation.\n\n")
              racket-make-doc/linkify
              racket-make-doc/tweak-quotes)
          "\n\n"))

(defun racket-make-doc/bindings-as-kbd (symbol)
  (let* ((bindings (racket-make-doc/bindings symbol))
         (strs (and bindings
                    (-non-nil
                     (-map (lambda (binding)
                             (unless (eq (aref binding 0) 'menu-bar)
                               (format "<kbd>%s</kbd>"
                                       (racket-make-doc/html-escape
                                        (key-description binding)))))
                           bindings))))
         (str (if strs
                  (apply #'concat (-interpose " or " strs))
                (format "<kbd>M-x %s</kbd>" symbol))))
    (concat str "\n\n")))

(defun racket-make-doc/bindings (symbol)
  (where-is-internal symbol racket-mode-map))

(defun racket-make-doc/html-escape (str)
  (with-temp-buffer
    (insert str)
    (format-replace-strings '(("&" . "&amp;")
                              ("<" . "&lt;")
                              (">" . "&gt;")))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; Variables

(defconst racket-make-doc/variables
  '(racket-racket-program
    racket-raco-program
    racket-memory-limit
    racket-history-filter-regexp
    racket-images-inline
    racket-images-keep-last
    racket-images-system-viewer
    racket-pretty-print
    racket-wait-for-prompt-timeout
    racket-indent-curly-as-sequence
    racket-indent-sequence-depth
    racket-pretty-lambda
    racket-smart-open-bracket-enable
    racket-use-company-mode)
  "Variables to include in the Reference.")

(defun racket-make-doc/variables ()
  (apply #'concat
         (mapcar #'racket-make-doc/variable racket-make-doc/variables)))

(defun racket-make-doc/variable (symbol)
  (concat (format "### %s\n" symbol)
          (-> (or (documentation-property symbol 'variable-documentation)
                  "No documentation.\n\n")
              racket-make-doc/linkify
              racket-make-doc/tweak-quotes)
          "\n\n"))

;;; Variables

(defconst racket-make-doc/faces
  '(racket-keyword-argument-face
    racket-paren-face
    racket-selfeval-face)
  "Faces to include in the Reference.")

(defun racket-make-doc/faces ()
  (apply #'concat
         (mapcar #'racket-make-doc/face racket-make-doc/faces)))

(defun racket-make-doc/face (symbol)
  (concat (format "### %s\n" symbol)
          (-> (or (documentation-property symbol 'face-documentation)
                  "No documentation.\n\n")
              racket-make-doc/linkify
              racket-make-doc/tweak-quotes)
          "\n\n"))

;;; Utility

(defun racket-make-doc/linkify (s)
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx ?\`
                                  (group "racket-" (+ (or (syntax word)
                                                          (syntax symbol))))
                                  ?\')
                              nil t)
      (let ((name (buffer-substring-no-properties (match-beginning 1)
                                                  (match-end 1))))
        (replace-match (format "[`%s`](#%s)" name name)
                       nil nil)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-make-doc/tweak-quotes (s)
  "Change \` \' style quotes to \` \` style."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx ?\`
                                  (group (+ (or (syntax word)
                                                (syntax symbol))))
                                  ?\')
                              nil t)
      (let ((name (buffer-substring-no-properties (match-beginning 1)
                                                  (match-end 1))))
        (replace-match (format "`%s`" name)
                       nil nil)))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; racket-make-doc.el ends here
