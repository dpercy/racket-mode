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
  (with-temp-buffer
    (insert (racket-make-doc/reference))
    (let ((backup-inhibited t))
      (write-file racket-make-doc/Reference.md nil))))


(defun racket-make-doc/reference ()
  (concat "# racket-mode Reference\n"
          "\n"
          "- [Commands](#commands)\n"
          "- [Variables](#variables)\n"
          "\n"
          "---\n\n"
          "## Commands\n\n"
          (racket-make-doc/commands)
          "---\n\n"
          "## Variables\n\n"
          "> Note: You may also set these via Customize.\n\n"
          (racket-make-doc/variables)))

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
    racket-backward-up-list))

(defun racket-make-doc/commands ()
  (apply #'concat
         (mapcar #'racket-make-doc/command racket-make-doc/commands)))

(defun racket-make-doc/command (symbol)
  (concat (format "### %s\n" symbol)
          (racket-make-doc/bindings-as-kbd symbol)
          (racket-make-doc/tweak-quotes
           (racket-make-doc/linkify
            (or (documentation symbol nil)
                "No documentation.\n\n")))
          "\n\n"))

(defun racket-make-doc/bindings-as-kbd (symbol)
  (let ((bindings (racket-make-doc/bindings symbol)))
    (if bindings
        (apply #'concat
               (-snoc
                (-interpose " or "
                            (-non-nil
                             (-map (lambda (binding)
                                     (unless (eq (aref binding 0) 'menu-bar)
                                       (format "<kbd>%s</kbd>"
                                               (racket-make-doc/html-escape
                                                (key-description binding)))))
                                   bindings)))
                 "\n\n"))
      "\n")))

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
    racket-use-company-mode
    racket-keyword-argument-face
    racket-paren-face
    racket-selfeval-face))

(defun racket-make-doc/variables ()
  (apply #'concat
         (mapcar #'racket-make-doc/variable racket-make-doc/variables)))

(defun racket-make-doc/variable (symbol)
  (concat (format "### %s\n" symbol)
          (racket-make-doc/tweak-quotes
           (racket-make-doc/linkify
            (or (documentation-property symbol 'variable-documentation t)
                "No documentation.\n\n")))
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
