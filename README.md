# Racket mode for GNU Emacs

[![Build Status](https://travis-ci.org/greghendershott/racket-mode.png?branch=master)](https://travis-ci.org/greghendershott/racket-mode)
[![MELPA](http://melpa.org/packages/racket-mode-badge.svg)](http://melpa.org/#/racket-mode)

This provides a major mode to edit [Racket] source files, as well as a
major mode for a Racket REPL. The edit/run experience is similar to
[DrRacket].

- Focus on Racket.
  - Mode line and menu say `Racket`.
  - Omit stuff for various current and historical Schemes that's N/A
    for Racket.

- Use [DrRacket] concepts where applicable.
  - A simple and obvious way to "run" a file: _Run_ <kbd>F5</kbd>.
  - Allow interaction in the REPL, but the effect is wiped on the next
    _Run_ (in contrast to using [`enter!`]).
  - A simple way to run unit tests (to run the `test` submodule).

- More thorough syntax highlighting ("font-lock"):
  - All Racket keywords, built-ins, self-evals, and so on.
  - All variations of `define` for functions and variables.

- Correct indentation of Racket forms, including `for/fold` and
  `for*/fold`.

- Compatible with Emacs **24.3+**.

## Caveats

- If you've used other Lisps and Schemes before, you might prefer
  [Geiser], which is very sophisticated.

- Although I dogfood this -- use it constantly to code Racket -- it is
  beta quality. My total experience writing Emacs modes consists of
  writing this mode.

- Pull requests from smarter/wiser people are welcome.

- Please report issues [here][issues].

## Install

You can install the `racket-mode` package from [MELPA].

> **TIP**: To use MELPA in Emacs 24, add the following to your
> `.emacs` or `.emacs.d/init.el`:
>
> ```cl
> (require 'package)
> (add-to-list 'package-archives
>   '("melpa" . "http://melpa.milkbox.net/packages/") t)
> ```

## Configure

See [Variables](Reference.md#variables). You may set these directly in
your Emacs init file (`~/.emacs` or `~/.emacs.d/init.el`), or, use
<kbd>M-x Customize</kbd>, as you prefer.

To start, the only two variables you might need to set are:

- **Racket Program**, the name or pathname of the Racket executable.
  This defaults to `Racket.exe` on Windows else `racket`.

- Set **Raco Program**, the name or pathname of the Raco executable.
  This defaults to `Raco.exe` on Windows else `raco`.

> **TIP** On OS X, even if your `PATH` includes the Racket
> executables, be aware that GUI Emacs will **not** automatically use
> your path! So either set both of these in your `.emacs` to be full
> pathames, or, solve the root issue by using the excellent
> [exec-path-from-shell] package.

[exec-path-from-shell]: http://melpa.org/#/exec-path-from-shell

### Key bindings

To customize things like key bindings, you can use `racket-mode-hook`
in your Emacs init file. For example, although <kbd>F5</kbd> is bound
to the **racket-run** command, let's say you wanted <kbd>C-c r</kbd>
to be an additional binding:

```cl
(add-hook 'racket-mode-hook
          '(lambda ()
             (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
```

## Commands

See the [Reference](Reference.md).


Also see the `Racket` menu.

Most of the commands should be self-explanatory. (If not,
[report it][issues] and I'll improve the documentation.)


## Contributing

Run `make deps` to install the packages on which racket-mode depends.

### Reference.md

Reference.md is generated from doc strings.

If you edit a doc string for a `defcustom` or a command (but don't
worry about non-`interactive` `defun`s) then please `make doc` and
commit the updated Reference.md.

If you add a brand-new `defcustom` or command, then in addition to the
preceding step, in `racket-make-doc.rkt` add the `defcustom` to the
`racket-make-doc/variables` list or add the command to the
`racket-make-doc/commands` list.

### Tests

Currently tests are on the light side, but more are welcome.

Please do run `make test` to ensure you don't break the existing ones.
Travis CI will also do this automatically on your pull request.

## Background/Motivation

I started this project accidentally, while trying to figure out a
font-lock issue with [Quack] under Emacs 24.2.

Knowing nothing about how to make a mode in Emacs, I tried to isolate
the problem by making a simple major mode, then adding things until it
broke. It didn't break and I ended up with this.

I took various `.emacs.d` hacks that I'd previously made to use with
Quack, and rolled them into this mode.

Also, I'd recently spent time adding Racket fontification to the
Pygments project, and wanted richer font-lock.

Also, I had experienced issues with `enter!` not always reloading
modules in recent versions of Racket, and came up with a DrRacket-like
alternative, `run!`.

Finally, I remembered that when I was new to Racket and Emacs, I got
confused by the _Scheme_ menu. It has options that work with various
Schemes over the years, but which are N/A for Racket. I would stare it
and wonder, "Um, how do I just 'run my program'??". I figured a fresh
focus on Racket might be helpful, especially for other folks
transitioning from using DrRacket.

Update, Jan 2014: After I had used this for a long time, putting up
with its quirks, someone put it on MELPA. That nudged me to take
another look, learn more about Elisp and Emacs modes, and improve
it. Although I still feel like an amateur, it has probably improved
from alpha to beta quality.

Please see the [Acknowledgments].

[Acknowledgments]: https://github.com/greghendershott/racket-mode/blob/master/THANKS.md
[Racket]: http://www.racket-lang.org/
[DrRacket]: http://docs.racket-lang.org/drracket/
[`enter!`]: http://docs.racket-lang.org/reference/interactive.html
[Geiser]: http://www.nongnu.org/geiser/
[Quack]: http://www.neilvandyke.org/quack/
[issues]: https://www.github.com/greghendershott/racket-mode/issues
[Compilation mode command]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html
[`racket/help`]: http://docs.racket-lang.org/reference/Interactive_Help.html
[`define-logger`]: http://docs.racket-lang.org/reference/logging.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._define-logger%29%29
[`current-directory`]: http://docs.racket-lang.org/reference/Filesystem.html#%28def._%28%28quote._~23~25kernel%29._current-directory%29%29
[MELPA]: http://melpa.milkbox.net/#/getting-started
[`company-mode`]: https://github.com/company-mode/company-mode
[`identifier-binding`]: http://docs.racket-lang.org/reference/stxcmp.html#%28def._%28%28quote._~23~25kernel%29._identifier-binding%29%29
