# Package dependencies

racket-mode depends on a few packages:

- [`dash`](http://melpa.org/#/dash)
- [`faceup`](http://melpa.org/#/faceup)
- [`s`](http://melpa.org/#/s)

You can install these manually with <kbd>M-x package-install</kbd>,
or, run `make deps`. The latter is also used by `.travis.yml`.

# Reference.md

`Reference.md` is generated from doc strings for items specifically
listed in `racket-make-doc.el`

- If you add a brand-new command, `defcustom`, or `defface`, please
  also add it to appropriate list.

- If you edit a doc string for a command, `defcustom`, or `defface`,
  please `make doc` and commit the updated `Reference.md`. (Although
  this file is N/A for people _using_ racket-mode, it's useful to have
  the features documented online.)

# Tests

Currently tests are on the light side. More are welcome.

Please do run `make test` to ensure your changes pass the existing
tests. Travis CI will also do this automatically on your pull request.

## Indentation

If you change indentation, you may need to update the `example/*rkt`
reference files used in tests.

## Font-lock

If you change font-lock, you may need to use <kbd>M-x
faceup-write-file</kbd> to regenerate the `example/*rkt.faceup`
reference files used in tests.
