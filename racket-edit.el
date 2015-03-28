;;; racket-edit.el

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

;; racket-mode per se, i.e. the .rkt file buffers

(require 'cl-lib)
(require 'cl-macs)
(require 'racket-custom)
(require 'racket-common)
(require 'racket-complete)
(require 'racket-util)
(require 'hideshow)

(defun racket-run (&optional errortracep)
  "Save and evaluate the buffer in REPL, much like DrRacket's Run.

When you run again, the file is evaluated from scratch -- the
custodian releases resources like threads and the evaluation
environment is reset to the contents of the file. In other words,
like DrRacket, this provides the predictability of a \"static\"
baseline, plus some interactive exploration.

See also `racket-run-and-switch-to-repl', which is even more like
DrRacket's Run because it selects the REPL window (gives it the
focus), too.

With a C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

Output in the `*Racket REPL*` buffer that describes a file and
position is automatically \"linkified\". To visit, move point
there and press <kdb>RET</kbd>, mouse click, or use a
Compilation mode command such as \\[next-error] (next error).
Examples of such text include:

- Racket error messages.
- `rackunit` test failure location messages.
- `print`s of `#<path>` objects.

In the `*Racket REPL*` buffer you can issue some special
commands. Some of them are the foundation for Emacs commands.
Others are available only as a command in the REPL.

- `,help`: See these commands.

- `,top`: Reset the REPL to \"no file\" (i.e. a base namespace).

- `,run <file>`: Run the file. What `racket-run' uses. Either
  `\"file.rkt\"` is `file.rkt` OK.

- `,exit`: Exit Racket. Handy in a `#lang` like r5rs where the
  `exit` procedure is not available. (Regardless of how Racket
  exits, the `*Racket REPL*` buffer is not killed and is reused
  if you `racket-run' again.)

- `,doc <symbol-or-string>`: Look for `<symbol-or-string>` in
  Racket's documentation. What `racket-doc' uses.

- `,cd`, `,pwd`: Change and show [`current-directory`].

- `,log` controls the log output level, overall, as well as for
  specific named loggers created with [`define-logger`].

    - `,log`: Show the current levels.

    - `,log <logger> <level>`: Set a logger to show at least level
      `none`, `fatal`, `error`, `warning`, `info`, or `debug`.

    - `,log <logger> <level>`: Set a logger to use the default
      level.

    - `,log <level>`: Set the default level for all other loggers
      not specified individually.
"
  (interactive "P")
  (racket--do-run (if errortracep
                      'high
                    racket-error-context)))

(defun racket--do-run (context-level)
  "Helper function for `racket-run'-like commands.
Supplies CONTEXT-LEVEL to the back-end ,run command; see run.rkt."
  (unless (eq major-mode 'racket-mode)
    (error "Current buffer is not a racket-mode buffer"))
  (when (buffer-modified-p)
    (save-buffer))
  (racket--remove-uncovered-overlays)
  (racket--invalidate-completion-cache)
  (racket--invalidate-type-cache)
  (racket--debug-kill-timer)
  (racket--repl-eval (format ",run %s %s %s %s\n"
                             (racket--quoted-buffer-file-name)
                             racket-memory-limit
                             racket-pretty-print
                             context-level)))

(defun racket-run-and-switch-to-repl (&optional errortracep)
  "This is `racket-run' followed by `racket-switch-to-repl'.

With a C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting."
  (interactive "P")
  (racket-run errortracep)
  (racket-repl))

(defun racket-racket ()
  "Do `racket <file>` in `*shell*` buffer."
  (interactive)
  (racket--shell (concat racket-racket-program
                         " "
                         (racket--quoted-buffer-file-name))))

(defvar racket--uncovered-overlays nil)

(defun racket--remove-uncovered-overlays ()
  (while racket--uncovered-overlays
    (delete-overlay (pop racket--uncovered-overlays))))

(defun racket-test (&optional coverage)
  "Do `(require (submod \".\" test))` in `*Racket REPL*` buffer.

With prefix, runs with coverage instrumentation and highlights
uncovered code.

Put your tests in a `test` submodule. For example:

    (module+ test
      (require rackunit)
      (check-true #t))

rackunit test failure messages show the location. You may use
`next-error' to jump to the location of each failing test.

See also:
- `racket-fold-all-tests'
- `racket-unfold-all-tests'
"
  (interactive "P")
  (message (if coverage "Running tests with coverage instrumentation enabled..."
             "Running tests..."))
  (racket--do-run (if coverage 'coverage racket-error-context))
  (racket--repl-eval (format "%S\n"
                             `(begin
                               (require (submod "." test))
                               (flush-output (current-output-port)))))
  (if (not coverage)
      (message "Tests done.")
    (message "Checking coverage results...")
    (let ((xs (racket--repl-cmd/sexpr ",get-uncovered")))
      (dolist (x xs)
        (let ((beg (car x))
              (end (cdr x)))
          (let ((o (make-overlay beg end)))
            (push o racket--uncovered-overlays)
            (overlay-put o 'name 'racket-uncovered-overlay)
            (overlay-put o 'priority 100)
            (overlay-put o 'face font-lock-warning-face))))
      (if (not xs)
          (message "Coverage complete.")
        (message (format "Missing coverage in %s places." (length xs)))
        (goto-char (car (car xs)))))))

(defun racket-raco-test ()
  "Do `raco test -x <file>` in `*shell*` buffer.
To run <file>'s `test` submodule."
  (interactive)
  (racket--shell (concat racket-raco-program
                         " test -x "
                         (racket--quoted-buffer-file-name))))

(defun racket--shell (cmd)
  (let ((w (selected-window)))
    (save-buffer)
    (let ((rw (get-buffer-window "*shell*")))
      (if rw
          (select-window rw)
        (other-window -1)))
    (message (concat cmd "..."))
    (shell)
    (pop-to-buffer-same-window "*shell*")
    (comint-send-string "*shell*" (concat cmd "\n"))
    (select-window w)
    (sit-for 3)
    (message nil)))


;;; visiting defs and mods

(defun racket-visit-definition (&optional prefix)
  "Visit definition of symbol at point.

Use \\[racket-unvisit] to return.

Note: Only finds symbols defined in the current namespace. You
may need to invoke `racket-run' on the current buffer, first.

Note: Only visits the definition of module level identifiers (i.e.
things for which Racket's `identifier-binding` function returns a
list, as opposed to `'lexical`).

Note: If the definition is from Racket's `#%kernel` module, it
will tell you so but won't visit the definition site."
  (interactive "P")
  (let ((sym (racket--symbol-at-point-or-prompt prefix "Visit definition of: ")))
    (when sym
      (racket--do-visit-def-or-mod "def" sym))))

(defun racket--do-visit-def-or-mod (cmd sym)
  "CMD must be \"def\" or \"mod\". SYM must be `symbolp`."
  (pcase (racket--repl-cmd/sexpr (format ",%s %s\n\n" cmd sym))
    (`(,path ,line ,col)
     (racket--push-loc)
     (find-file path)
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char col)
     (message "Type M-, to return"))
    (`kernel
     (message "`%s' defined in #%%kernel -- source not available." sym))
    (_ (when (y-or-n-p "Not found. Run current buffer and try again? ")
         (racket-run)
         (racket--do-visit-def-or-mod cmd sym)))))

(defun racket--get-def-file+line (sym)
  "For use by company-mode 'location option."
  (pcase (racket--repl-cmd/sexpr (format ",def %s\n\n" sym))
    (`(,path ,line ,_) (cons path line))
    (_ nil)))

(defun racket-visit-module (&optional prefix)
  "Visit definition of module at point, e.g. net/url or \"file.rkt\".

Use \\[racket-unvisit] to return.

Note: Only works if you've `racket-run' the buffer so that its
namespace is active.

See also: `racket-find-collection'."
  (interactive "P")
  (let* ((v (thing-at-point 'filename)) ;matches both net/url and "file.rkt"
         (v (and v (substring-no-properties v)))
         (v (if (or prefix (not v))
                (read-from-minibuffer "Visit module: " (or v ""))
              v)))
    (racket--do-visit-def-or-mod "mod" v)))

(defun racket-doc (&optional prefix)
  "View documentation of the identifier or string at point.

Uses the default external web browser.

If point is an identifier required in the current namespace that
has help, opens the web browser directly at that help
topic. (i.e. Uses the identifier variant of racket/help.)

Otherwise, opens the 'search for a term' page, where you can
choose among multiple possibilities. (i.e. Uses the string
variant of racket/help.)

With a C-u prefix, prompts for the identifier or quoted string,
instead of looking at point."
  (interactive "P")
  (let ((sym (racket--symbol-at-point-or-prompt prefix "Racket help for: ")))
    (when sym
      (racket--repl-cmd/string (format ",doc %s" sym)))))

(defvar racket--loc-stack '())

(defun racket--push-loc ()
  (push (cons (current-buffer) (point))
        racket--loc-stack))

(defun racket-unvisit ()
  "Return from previous `racket-visit-definition' or `racket-visit-module'."
  (interactive)
  (if racket--loc-stack
      (pcase (pop racket--loc-stack)
        (`(,buffer . ,pt)
         (pop-to-buffer-same-window buffer)
         (goto-char pt)))
    (message "Stack empty.")))


;;; racket-describe-mode

(defun racket-describe (&optional prefix)
"Describes the function at point in a `*Racket Describe*` buffer.

The intent is to give a quick reminder or introduction to a
function, regardless of whether it has installed documentation --
and to do so within Emacs, without switching to a web browser
window.

This buffer is also displayed when you use company-mode and press
<kbd>C-h</kbd> in the pop up completion list.

- If the function has installed Racket documentation, then a
  simplified version of the HTML is presented in the buffer,
  including the \"blue box\", documentation prose, and examples.

- Otherwise, the function's signature -- e.g. `(name arg-1-name
  arg-2-name)` is displayed. If the function has a Typed Racket
  type, or has a contract, then that is also displayed.

You can quit the buffer by pressing <kbd>q</kbd>. Also, at the
bottom of the buffer are Emacs buttons (which you may navigate among
using <kbd>TAB</kbd> for visiting the definition or opening the full
browser documentation (if any)."
  (interactive "P")
  (let ((sym (racket--symbol-at-point-or-prompt prefix "Describe: ")))
    (when sym
      (racket--do-describe sym t))))

(defun racket--do-describe (sym pop-to)
  "A helper for `racket-describe' and `racket-company-backend'.

POP-TO should be t for the former (in which case some buttons are
added) and nil for the latter.

Returns the buffer in which the description was written."
  (with-current-buffer (get-buffer-create "*Racket Describe*")
    (racket-describe-mode)
    (read-only-mode -1)
    (erase-buffer)
    (let ((html (racket--repl-cmd/string (format ",describe %s" sym)))
          (spc (string #x2020))) ;unlikely character (hopefully)
      ;; Emacs shr renderer removes leading &nbsp; from <td> elements
      ;; -- which messes up the indentation of s-expressions including
      ;; contracts. So replace &nbsp with `spc' in the source HTML,
      ;; and replace `spc' with " " after shr-insert-document outputs.
      (shr-insert-document
       (with-temp-buffer
         (insert html)
         (goto-char (point-min))
         (while (re-search-forward "&nbsp;" nil t)
           (replace-match spc t t))
         (libxml-parse-html-region (point-min) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward spc nil t)
        (replace-match " " t t)))
    (goto-char (point-max))
    (when pop-to
      (insert-text-button
       "Definition"
       'action
       `(lambda (btn)
          (racket--do-visit-def-or-mod
           "def"
           ,(substring-no-properties (format "%s" sym)))))
      (insert "   ")
      (insert-text-button
       "Documentation in Browser"
       'action
       `(lambda (btn)
          (racket--repl-cmd/buffer
           ,(substring-no-properties (format ",doc %s\n" sym)))))
      (insert "          [q]uit"))
    (read-only-mode 1)
    (goto-char (point-min))
    (display-buffer (current-buffer) t)
    (when pop-to
      (pop-to-buffer (current-buffer))
      (message "Type TAB to move to links, 'q' to restore previous window"))
    (current-buffer)))

(defvar racket-describe-mode-map
  (racket--easy-keymap-define
   '(("q"       quit-window)
     ("<tab>"   racket-describe--next-button)
     ("S-<tab>" racket-describe--prev-button)))
  "Keymap for Racket Describe mode.")

(define-derived-mode racket-describe-mode fundamental-mode
  "RacketDescribe"
  "Major mode for describing Racket functions.
\\{racket-describe-mode-map}"
  (setq show-trailing-whitespace nil))

(defun racket-describe--next-button ()
  (interactive)
  (forward-button 1 t t))

(defun racket-describe--prev-button ()
  (interactive)
  (forward-button -1 t t))


;;; code folding

;;;###autoload
(add-to-list 'hs-special-modes-alist
             '(racket-mode "(" ")" ";" nil nil))

(defun racket--for-all-tests (verb f)
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^(module[+*]? test" (point-max) t)
        (funcall f)
        (cl-incf n)
        (goto-char (match-end 0)))
      (message "%s %d test submodules" verb n))))

(defun racket-fold-all-tests ()
  "Fold (hide) all test submodules."
  (interactive)
  (racket--for-all-tests "Folded" 'hs-hide-block))

(defun racket-unfold-all-tests ()
  "Unfold (show) all test submodules."
  (interactive)
  (racket--for-all-tests "Unfolded" 'hs-show-block))


;;; macro expansion

(defun racket-expand-region (start end &optional prefix)
  "Like `racket-send-region', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "rP")
  (if (region-active-p)
      (progn
        (racket--repl-send-expand-command prefix)
        (racket--send-region-to-repl start end))
    (beep)
    (message "No region.")))

(defun racket-expand-definition (&optional prefix)
  "Like `racket-send-definition', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-definition))

(defun racket-expand-last-sexp (&optional prefix)
  "Like `racket-send-last-sexp', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-last-sexp))

(defun racket--repl-send-expand-command (prefix)
  (comint-send-string (racket--get-repl-buffer-process)
                      (if prefix ",exp!" ",exp ")))

(defun racket-expand-again ()
  "Macro expand again the previous expansion done by one of:
- `racket-expand-region'
- `racket-expand-definition'
- `racket-expand-last-sexp'
- `racket-expand-again'"
  (interactive)
  (comint-send-string (racket--get-repl-buffer-process) ",exp+\n"))

(defun racket-gui-macro-stepper ()
  "Run the DrRacket GUI macro stepper.

Runs on the active region, if any, else the entire buffer.

EXPERIMENTAL: May be changed or removed.

BUGGY: The first-ever invocation might not display a GUI window.
If so, try again."
  (interactive)
  (save-buffer)
  (racket--repl-eval
   (format "%S\n"
           `(begin
             (require macro-debugger/stepper racket/port)
             ,(if (region-active-p)
                  `(expand/step
                    (with-input-from-string ,(buffer-substring-no-properties
                                              (region-beginning)
                                              (region-end))
                                            read-syntax))
                `(expand-module/step
                  (string->path
                   ,(substring-no-properties (buffer-file-name)))))))))


;;; requires

(defun racket-tidy-requires ()
  "Make a single top-level `require`, modules sorted, one per line.

All top-level `require` forms are combined into a single form.
Within that form:

- A single subform is used for each phase level, sorted in this
  order: for-syntax, for-template, for-label, for-meta, and
  plain (phase 0).

  - Within each level subform, the modules are sorted:

    - Collection path modules -- sorted alphabetically.

    - Subforms such as `only-in`.

    - Quoted relative requires -- sorted alphabetically.

At most one module is listed per line.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

See also: `racket-trim-requires' and `racket-base-requires'."
  (interactive)
  (let* ((result (racket--kill-top-level-requires))
         (beg (nth 0 result))
         (reqs (nth 1 result))
         (new (and beg reqs
                   (racket--repl-cmd/string
                    (format ",requires/tidy %S" reqs)))))
    (when new
      (goto-char beg)
      (insert (concat (read new) "\n")))))

(defun racket-trim-requires ()
  "Like `racket-tidy-requires' but also deletes unused modules.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

See also: `racket-base-requires'."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let* ((result (racket--kill-top-level-requires))
         (beg (nth 0 result))
         (reqs (nth 1 result))
         (new (and beg reqs
                   (racket--repl-cmd/string
                    (format ",requires/trim \"%s\" %S"
                            (substring-no-properties (buffer-file-name))
                            reqs))))
         (new (and new
                   (condition-case () (read new)
                     (error (revert-buffer t t t) ;restore original requires
                            (error "Can't do, source file has error"))))))
    (when new
      (goto-char beg)
      (insert (concat new "\n")))))

(defun racket-base-requires ()
  "Change from `#lang racket` to `#lang racket/base`.

Adds explicit requires for modules that are provided by `racket`
but not by `racket/base`.

This is a recommended optimization for Racket applications.
Avoiding loading all of `racket` can reduce load time and memory
footprint.

Also, as does `racket-trim-requires', this removes unneeded
modules and tidies everything into a single, sorted require form.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

Note: Currently this only helps change `#lang racket` to
`#lang racket/base`. It does *not* help with other similar conversions,
such as changing `#lang typed/racket` to `#lang typed/racket/base`."
  (interactive)
  (when (racket--buffer-start-re "^#lang.*? racket/base$")
    (error "Already using #lang racket/base. Nothing to change."))
  (unless (racket--buffer-start-re "^#lang.*? racket$")
    (error "File does not use use #lang racket. Cannot change."))
  (when (buffer-modified-p) (save-buffer))
  (let* ((result (racket--kill-top-level-requires))
         (beg (or (nth 0 result)
                  (save-excursion
                    (goto-char 0) (forward-line 1) (insert "\n") (point))))
         (reqs (nth 1 result))
         (new (racket--repl-cmd/string
               (format ",requires/base \"%s\" %S"
                       (substring-no-properties (buffer-file-name))
                       reqs)))
         (new (and new
                   (condition-case () (read new)
                     (error (revert-buffer t t t) ;restore original requires
                            (error "Can't do, source file has error"))))))
    (when new
      (goto-char beg)
      (insert (concat new "\n")))
    (goto-char (point-min))
    (re-search-forward "^#lang.*? racket$")
    (insert "/base")))

(defun racket--buffer-start-re (re)
  (save-excursion
    (condition-case ()
        (progn
          (goto-char (point-min))
          (re-search-forward re)
          t)
      (error nil))))

(defun racket--kill-top-level-requires ()
  "Delete all top-level `require`s. Return list with two results:

The first element is point where the first require was found, or
nil.

The second element is a list of require s-expressions found.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

Note: It might work better to shift this work into Racket code,
and have it return a list of file offsets and replacements. Doing
so would make it easier to match require forms syntactically
instead of textually, and handle module and submodule forms."
  (save-excursion
    (goto-char (point-min))
    (let ((first-beg nil)
          (requires nil))
      (while (re-search-forward "^(require " nil t)
        (let* ((beg (progn (up-list -1)   (point)))
               (end (progn (forward-sexp) (point)))
               (str (buffer-substring-no-properties beg end))
               (sexpr (read str)))
          (unless first-beg (setq first-beg beg))
          (setq requires (cons sexpr requires))
          (kill-sexp -1)
          (delete-blank-lines)))
      (list first-beg requires))))


;;; racket-check-syntax

(defvar racket--highlight-overlays nil)

(defun racket--highlight (beg end defp)
  ;; Unless one of our highlight overlays already exists there...
  (let ((os (overlays-at beg)))
    (unless (cl-some (lambda (o) (member o racket--highlight-overlays)) os)
      (let ((o (make-overlay beg end)))
        (push o racket--highlight-overlays)
        (overlay-put o 'name 'racket-check-syntax-overlay)
        (overlay-put o 'priority 100)
        (overlay-put o 'face (if defp
                                 racket-check-syntax-def-face
                               racket-check-syntax-use-face))))))

(defun racket--unhighlight-all ()
  (while racket--highlight-overlays
    (delete-overlay (pop racket--highlight-overlays))))

(defun racket--point-entered (old new)
  (pcase (get-text-property new 'help-echo)
    (s (message s)))
  (pcase (get-text-property new 'racket-check-syntax-def)
    ((and uses `((,beg ,end) . ,_))
     (pcase (get-text-property beg 'racket-check-syntax-use)
       (`(,beg ,end) (racket--highlight beg end t)))
     (dolist (use uses)
       (pcase use (`(,beg ,end) (racket--highlight beg end nil))))))
  (pcase (get-text-property new 'racket-check-syntax-use)
    (`(,beg ,end)
     (racket--highlight beg end t)
     (dolist (use (get-text-property beg 'racket-check-syntax-def))
       (pcase use (`(,beg ,end) (racket--highlight beg end nil)))))))

(defun racket--point-left (old new)
  (racket--unhighlight-all))

(defun racket-check-syntax-mode-quit ()
  (interactive)
  (racket-check-syntax-mode -1))

(defun racket-check-syntax-mode-goto-def ()
  "When point is on a use, go to its definition."
  (interactive)
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,end) (goto-char beg))))

(defun racket-check-syntax-mode-forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,end)
     (pcase (get-text-property beg 'racket-check-syntax-def)
       (uses (let* ((pt (point))
                    (ix-this (cl-loop for ix from 0 to (1- (length uses))
                                      for use = (nth ix uses)
                                      when (and (<= (car use) pt) (< pt (cadr use)))
                                      return ix))
                    (ix-next (+ ix-this amt))
                    (ix-next (if (> amt 0)
                                 (if (>= ix-next (length uses)) 0 ix-next)
                               (if (< ix-next 0) (1- (length uses)) ix-next)))
                    (next (nth ix-next uses)))
               (goto-char (car next))))))
    (_ (pcase (get-text-property (point) 'racket-check-syntax-def)
         (`((,beg ,end) . ,_) (goto-char beg))))))


(defun racket-check-syntax-mode-goto-next-use ()
  "When point is on a use, go to the next (sibling) use."
  (interactive)
  (racket-check-syntax-mode-forward-use 1))

(defun racket-check-syntax-mode-goto-prev-use ()
  "When point is on a use, go to the previous (sibling) use."
  (interactive)
  (racket-check-syntax-mode-forward-use -1))

(defun racket-check-syntax-mode-help ()
  (interactive)
  (describe-function #'racket-check-syntax-mode))

(defun racket-check-syntax-mode-rename ()
  (interactive)
  ;; If we're on a def, get its uses. If we're on a use, get its def.
  (let* ((pt (point))
         (uses (get-text-property pt 'racket-check-syntax-def))
         (def  (get-text-property pt 'racket-check-syntax-use)))
    ;; If we got one, get the other.
    (when (or uses def)
      (let* ((uses (or uses (get-text-property (car def)   'racket-check-syntax-def)))
             (def  (or def  (get-text-property (caar uses) 'racket-check-syntax-use)))
             (locs (cons def uses))
             (strs (mapcar (lambda (loc)
                             (apply #'buffer-substring-no-properties loc))
                           locs)))
        ;; Proceed only if all the strings are the same. (They won't
        ;; be for e.g. import bindings.)
        (when (cl-every (lambda (s) (equal (car strs) s))
                        (cdr strs))
          (let ((new (read-from-minibuffer (format "Rename %s to: " (car strs))))
                (marker-pairs
                 (mapcar (lambda (loc)
                           (let ((beg (make-marker))
                                 (end (make-marker)))
                             (set-marker beg (nth 0 loc) (current-buffer))
                             (set-marker end (nth 1 loc) (current-buffer))
                             (list beg end)))
                         locs))
                (point-marker (let ((m (make-marker)))
                                (set-marker m (point) (current-buffer)))))
            (racket-check-syntax-mode -1)
            (dolist (marker-pair marker-pairs)
              (let ((beg (marker-position (nth 0 marker-pair)))
                    (end (marker-position (nth 1 marker-pair))))
                (delete-region beg end)
                (goto-char beg)
                (insert new)))
            (goto-char (marker-position point-marker))
            (racket-check-syntax-mode 1)))))))

(defun racket-check-syntax-mode-goto-next-def ()
  (interactive)
  (let ((pos (next-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (next-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(defun racket-check-syntax-mode-goto-prev-def ()
  (interactive)
  (let ((pos (previous-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (previous-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(define-minor-mode racket-check-syntax-mode
  "Analyze the buffer and annotate with information.

The buffer becomes read-only until you exit this minor mode.
However you may navigate the usual ways. When point is on a
definition or use, related items are highlighted and
information is displayed in the echo area. You may also use
special commands to navigate among the definition and its uses.

```
\\{racket-check-syntax-mode-map}
```
"
  :lighter " CheckSyntax"
  :keymap (racket--easy-keymap-define
           '(("q"       racket-check-syntax-mode-quit)
             ("h"       racket-check-syntax-mode-help)
             (("j" "TAB")    racket-check-syntax-mode-goto-next-def)
             (("k" "S-TAB")  racket-check-syntax-mode-goto-prev-def)
             ("."       racket-check-syntax-mode-goto-def)
             ("n"       racket-check-syntax-mode-goto-next-use)
             ("p"       racket-check-syntax-mode-goto-prev-use)
             ("r"       racket-check-syntax-mode-rename)))
  (unless (eq major-mode 'racket-mode)
    (setq racket-check-syntax-mode nil)
    (error "racket-check-syntax-mode only works with racket-mode"))
  (racket--check-syntax-stop)
  (when racket-check-syntax-mode
    (racket--check-syntax-start)))

(defun racket--check-syntax-start ()
  (racket-run) ;ensure REPL is evaluating this buffer
  (message "Analyzing...")
  (let ((xs (racket--repl-cmd/sexpr (format ",check-syntax\n\n") 30)))
    (unless xs
      (error "Requires a newer version of Racket."))
    (with-silent-modifications
      (dolist (x xs)
        (pcase x
          (`(,`info ,beg ,end ,str)
           (put-text-property beg end 'help-echo str))
          (`(,`def/uses ,def-beg ,def-end ,uses)
           (add-text-properties def-beg
                                def-end
                                (list 'racket-check-syntax-def uses
                                      'point-entered #'racket--point-entered
                                      'point-left #'racket--point-left))
           (dolist (use uses)
             (pcase-let* ((`(,use-beg ,use-end) use))
               (add-text-properties use-beg
                                    use-end
                                    (list 'racket-check-syntax-use (list def-beg
                                                                         def-end)
                                          'point-entered #'racket--point-entered
                                          'point-left #'racket--point-left)))))))
      (setq buffer-read-only t)
      (racket--point-entered (point-min) (point)) ;in case already in one
      (setq header-line-format
            "Check Syntax. Buffer is read-only. Press h for help, q to quit.")
      (racket-check-syntax-mode-goto-next-def))
    (message "")))

(defun racket--check-syntax-stop ()
  (setq header-line-format nil)
  (with-silent-modifications
    (remove-text-properties (point-min)
                            (point-max)
                            '(help-echo nil
                              racket-check-syntax-def nil
                              racket-check-syntax-use nil
                              point-entered
                              point-left))
    (racket--unhighlight-all)
    (setq buffer-read-only nil)))


;;; debug

(defvar racket--debug-break-timer nil)

(defun racket-debug ()
  "Instrument file(s) for debugging and run.

For each required file in the same directory, you get a y/n
prompt. Answering yes means you can step and break in that other
file, too.

Upon each break, the source file is shown with point at the
breakpoint. Uses of top-level and local bindings are drawn
showing the current values inline. The minor mode
`racket-debug-mode' is enabled, which provides additional
commands:

```
\\{racket-debug-mode-map}
```

During a break, the Racket REPL may be used to evaluate
expressions in the namespace of the module. Both top-level and
local bindings may be referenced and `set!` to new values.

The REPL may be used even after the program completes, because
the code remains instrumented for debugging. If you call
instrumented code, it will break before the first expression. (To
\"fully exit\" debugging, do a normal `racket-run'.)"
  (interactive)
  (racket--do-run 'debug)
  (setq racket--debug-break-timer
        (run-with-timer 0.5 nil #'racket--on-debug-break-timer)))

(defun racket--debug-kill-timer ()
  (when racket--debug-break-timer
    (cancel-timer racket--debug-break-timer)
    (setq racket--debug-break-timer nil)))

(defun racket--on-debug-break-timer ()
  (when (file-exists-p racket--repl-debug-break-output-file)
    (racket--debug-on-break
     (with-temp-buffer
       (insert-file-contents racket--repl-debug-break-output-file)
       (delete-file racket--repl-debug-break-output-file)
       (eval (read (buffer-substring (point-min) (point-max)))))))
  ;; TODO: Should we avoid setting the timer if no DEBUG: prompt in REPL?
  (setq racket--debug-break-timer
        (run-with-timer 0.5 nil #'racket--on-debug-break-timer)))

(defvar racket-debug-mode-break-data nil
  "Data for the most recent debugger break.")

(defun racket--debug-on-break (data)
  (setq racket-debug-mode-break-data data) ;save for `racket-debug-mode-step'
  (cond
   ((and (listp data)
         (eq 'also-file? (car data)))
    (let ((v (y-or-n-p (format "Also debug %s? " (cadr data)))))
      (racket--repl-eval (concat (if v "#t" "#f") "\n"))))
   ((and (listp data)
         (eq 'break (car data)))
    (let ((which (cadr data))
          (src (cadr (assoc 'src data)))
          (pos (cadr (assoc 'pos data))))
      ;; If file already visible in a window, select that window.
      ;; Otherwise pick any window except Racket REPL, then find-file.
      (let* ((buf (get-file-buffer src))
             (win (and buf (get-buffer-window buf))))
        (if win
            (select-window win)
          (let ((win (get-buffer-window racket--repl-buffer-name)))
            (when (equal win (selected-window))
              (other-window 1)))
          (find-file src)))
      ;; Go to the breakpoint position.
      (goto-char pos)
      ;; Draw bindings.
      (mapc (lambda (binding)
              (cl-destructuring-bind (uses val) binding
                (mapc (lambda (use)
                        (cl-destructuring-bind (dat src line col pos span) use
                          (let* ((beg pos)
                                 (end (+ pos span))
                                 (str (propertize (format "=%s" val)
                                                  'face racket-debug-value-face)))
                            (racket-debug-mode--add-overlay beg end str))))
                      uses)))
            (cadr (assoc 'bindings data)))
      ;; Maybe draw result values.
      (let ((vals (cadr (assoc 'vals data))))
        (when vals
          (let* ((beg (point))
                 (end (1+ beg))
                 (str (propertize (format "=>%s" vals)
                                  'face racket-debug-result-face)))
            (racket-debug-mode--add-overlay beg end str))))
      ;; Show/draw the frames window
      (racket-debug-frames-mode-draw (cadr (assoc 'frames data)))
      ;; Enter the debug break mode.
      (racket--debug-keymap-work-around) ;BEFORE racket-debug-mode
      (racket-debug-mode 1)
      (message (format "Break %s" which))))
   (t
    (error (format "Unknown reponse from debugger: %s"
                   data)))))

(defun racket-debug-mode-step (&optional change-value-p)
  "Evaluate the next expression and break."
  (interactive "P")
  (let ((cmd (if (and change-value-p
                      (cadr (assoc 'vals racket-debug-mode-break-data)))
                 (let* ((old (cadr (assoc 'vals racket-debug-mode-break-data)))
                        (new (read-string "Return Values: " old)))
                   (format ",(step %s)\n" new))
               ",(step)\n")))
    (racket-debug-mode 0)
    (racket--repl-eval cmd)))

(defun racket-debug-mode-go ()
  "Continue until the next breakpoint, if any."
  (interactive)
  (racket-debug-mode 0)
  (racket--repl-eval ",(go)\n"))

(defun racket-debug-mode--do-breakpoint (status)
  "Find breakable position near point, go there, and set
breakpoint status to STATUS, which may be t, nil, 'one-shot, or a
positive number (the break is skipped N times)"
  (let* ((str (cl-case status
                ((t)   "#t")
                ((nil) "#f")
                (t     status)))
         (cmd (format ",(break %s %s)" (point) str))
         (pos (racket--repl-cmd/sexpr cmd)))
    (if pos
        (progn (goto-char pos)
               (cl-case status
                 ((t)   (message "Breakpoint set"))
                 ((nil) (message "Breakpoint cleared"))))
      (error "Cannot find a breakable position after point"))))

(defun racket-debug-mode-set-breakpoint (&optional skip-count)
  "Set a break at the first breakable position after point.

With a numeric prefix SKIP-COUNT, will skip the breakpoint that
number of times, then become a normal breakpoint."
  (interactive "p")
  (racket-debug-mode--do-breakpoint (or skip-count t)))

(defun racket-debug-mode-clear-breakpoint ()
  "Clear a break at the first breakable position after point."
  (interactive)
  (racket-debug-mode--do-breakpoint nil))

(defun racket-debug-mode-run-to-point ()
  "Run to the first breakable position after point.

Effectively this sets a one-shot breakpoint then does
`racket-debug-mode-go'."
  (interactive)
  (racket-debug-mode--do-breakpoint 'one-shot)
  (racket-debug-mode-go))

(defun racket-debug-mode-value ()
  "Change the value at point (if any)."
  (interactive)
  (let* ((old (racket--repl-cmd/sexpr (format "(get %s)" (point))))
         (new (read-string "Value: " old)))
    (racket--repl-cmd/sexpr (format "(set %s %s)" (point) new))))

(defun racket-debug-mode-quit ()
  (interactive)
  (racket--debug-kill-timer)
  (racket-debug-mode 0)
  (racket-debug-frames-mode-quit))

;; Herein I pair program. My partner is Mr. Hankey.
;;
;; We detect the break using run-with-timer. Enabling a minor mode
;; from a timer callback means its keymap won't take effect until
;; AFTER one command is processed. See discussion here:
;;
;; https://lists.gnu.org/archive/html/emacs-pretest-bug/2007-04/msg00279.html
;;
;; Use the work-around described there -- slamming our keys into
;; (current-local-keymap). However, also save the original keys, and
;; restore them in a post-command-hook -- because by then the minor
;; mode map will be used thereafter.
;;
;; Like I said. Mr. Hankey.

(defvar racket--debug-mode-keys
  '(("<SPC>"      racket-debug-mode-step)
    ("s"          racket-debug-mode-step)
    ("g"          racket-debug-mode-go)
    ("."          racket-debug-mode-run-to-point)
    ("b"          racket-debug-mode-set-breakpoint)
    ("u"          racket-debug-mode-clear-breakpoint)
    ("C-<return>" racket-debug-mode-value)
    ("q"          racket-debug-mode-quit))
  "Defined separately as part of the work-around; see comment above")

(defvar racket--debug-keymap-orig-keys nil)

(defun racket--debug-keymap-work-around ()
  "See comment above"
  (setq racket--debug-keymap-orig-keys
        (mapcar (lambda (x)
                  (let* ((key (kbd (car x)))
                         (old (key-binding key))
                         (new (cadr x)))
                    (define-key (current-local-map) key new)
                    (list key old)))
                racket--debug-mode-keys))
  (add-hook 'post-command-hook
            #'racket--debug-keymap-work-around-post-command-hook))

(defun racket--debug-keymap-work-around-post-command-hook ()
  "See comment above"
  (mapc (lambda (x)
          (define-key (current-local-map) (car x) (cadr x)))
        racket--debug-keymap-orig-keys)
  (setq racket--debug-keymap-orig-keys nil)
  (remove-hook 'post-command-hook
               #'racket--debug-keymap-work-around-post-command-hook))

(defvar racket-debug-mode-map
  (racket--easy-keymap-define racket--debug-mode-keys)
  "Keymap for racket-debug-mode.")

(define-minor-mode racket-debug-mode
  "A minor mode for debug breaks.

Although the buffer becomes read-only, you may still use
`racket-mode' commands that don't modify the buffer.

```
\\{racket-debug-mode-map}
```

See `racket-debug' for more information.
"
  :lighter " Debug"
  (unless (eq major-mode 'racket-mode)
    (setq racket-debug-mode nil)
    (error "racket-debug-mode only works with racket-mode"))
  (if racket-debug-mode
      (setq buffer-read-only t)
    (setq buffer-read-only nil)
    (setq racket-debug-mode-break-data nil)
    (racket-debug-mode--remove-overlays)))

(defvar racket--debug-overlays nil)

(defun racket-debug-mode--add-overlay (beg end str)
  "The nice thing about 'after-string overlays is that they do
not affect positions."
  (let ((o (make-overlay beg end (current-buffer))))
    (push o racket--debug-overlays)
    (overlay-put o 'name 'racket-debug-mode-overlay)
    (overlay-put o 'after-string str)))

(defun racket-debug-mode--remove-overlays ()
    (remove-overlays (point-min) (point-max) 'racket-debug-mode-overlay)
    (while racket--debug-overlays
      (delete-overlay (pop racket--debug-overlays))))


;;; debug frames

(defvar racket-debug-frames-mode-map
  (racket--easy-keymap-define
   '(("n"   racket-debug-frames-mode-next)
     ("p"   racket-debug-frames-mode-prev)
     ("RET" racket-debug-frames-mode-visit)
     ("q"   racket-debug-frames-mode-quit)))
  "Keymap for `racket-debug-frames-mode'.")

(define-derived-mode racket-debug-frames-mode special-mode
  "RacketDebugFrames"
  "Major mode to debugger frames.

```
\\{racket-debug-frames-mode-map}
```
"
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun racket-debug-frames-mode-quit ()
  (interactive)
  (let ((win (get-buffer-window "*Debug Frames*")))
    (when win
      (delete-window win))))

(defun racket-debug-frames-mode-draw (frames)
  (let* ((buf (get-buffer-create "*Debug Frames*"))
         (win (or (get-buffer-window buf)
                  (set-window-buffer (split-window-vertically)
                                     "*Debug Frames*"))))
    (with-current-buffer buf
      (unless (eq major-mode 'racket-debug-frames-mode)
        (racket-debug-frames-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (frame frames)
          (cl-destructuring-bind (datum file line col pos span) frame
            (let* ((file-buf (get-file-buffer file))
                   (txt (if file-buf
                            (with-current-buffer file-buf
                              (replace-regexp-in-string
                               "[ \n\t]+" " "
                               (buffer-substring pos (+ pos span))))
                          datum)))
              (insert (propertize (format "%s\n" txt)
                                  'racket-debug-frame-location
                                  (list file pos))))))
        (goto-char (point-min))
        ;;(fit-window-to-buffer win 10 1)
        ))))

(defun racket-debug-frames-mode-visit ()
  (interactive)
  (let ((prop (get-text-property (point) 'racket-debug-frame-location)))
    (when prop
      (cl-destructuring-bind (file pos) prop
        (select-window (get-buffer-window (find-file-other-window file)))
        (goto-char pos)))))

(defun racket-debug-frames-mode-next ()
  (interactive)
  (forward-line 1)
  (racket-debug-frames-mode-visit))

(defun racket-debug-frames-mode-prev ()
  (interactive)
  (forward-line -1)
  (racket-debug-frames-mode-visit))



;;; misc

(defun racket--quoted-buffer-file-name ()
  "`shell-quote-argument' ∘ `buffer-file-name'

Generally this should be used instead of plain
`buffer-file-name'. For example this will handle path names
containing spaces by escaping them."
  (shell-quote-argument (buffer-file-name)))


(provide 'racket-edit)

;; Local Variables:
;; coding: utf-8
;; comment-column: 40
;; indent-tabs-mode: nil
;; require-final-newline: t
;; show-trailing-whitespace: t
;; End:

;; racket-edit.el ends here
