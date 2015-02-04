
;;; Generate a markdown format file for Reference documentation.


;;; Top

(defun racket-make-doc/write-file (&optional file-name)
  (interactive
   (let ((suggested-name "Reference.md"))
     (list (read-file-name "Write Reference markdown file: "
                           default-directory
                           suggested-name
                           nil
                           (file-name-nondirectory suggested-name)))))
  (unless file-name
    (error "no file name"))
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert (racket-make-doc/reference))
      (write-file file-name t))))


(defun racket-make-doc/reference ()
  (concat "# racket-mode Reference\n\n"
          "---\n\n"
          "# Commands\n\n"
          (racket-make-doc/commands)
          "---\n\n"
          "# Variables\n\n"
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
  (concat (format "## %s\n" symbol)
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
               `(,@(mapcar (lambda (binding)
                             (if (eq (aref binding 0) 'menu-bar)
                                 ""
                               (format "<kbd>%s</kbd> "
                                       (key-description binding))))
                           bindings)
                 "\n\n"))
      "")))

(defun racket-make-doc/bindings (symbol)
  (where-is-internal symbol racket-mode-map))


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
  (concat (format "## %s\n" symbol)
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
