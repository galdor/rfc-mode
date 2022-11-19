
[![MELPA Stable](https://stable.melpa.org/packages/rfc-mode-badge.svg)](https://stable.melpa.org/#/rfc-mode)
[![MELPA](https://melpa.org/packages/rfc-mode-badge.svg)](https://melpa.org/#/rfc-mode)

# rfc-mode

## Introduction

The rfc-mode Emacs major mode is a browser and reader for RFC documents.

## Installation

The package should be installed from MELPA.

Start by loading the mode:

```elisp
(require 'rfc-mode)
```

Then set the location containing all RFC documents (the default value is the
`rfc` directory in the home directory):

```elisp
(setq rfc-mode-directory (expand-file-name "~/rfc/"))
```

RFC documents and their index will be directly downloaded from
https://www.rfc-editor.org when required. Alternatively, the entire RFC
collection can be downloadeded from https://www.rfc-editor.org/retrieve/bulk
to ensure full access without the need for an internet connection.

Call `rfc-mode-browse` to choose a RFC document to read, or `rfc-mode-read` to
enter the reference of the RFC document yourself.

## Screenshots
### Browser
![Helm-based browser](img/browser.png)

### Reader
![Reader](img/reader.png)

## Contact
If you have an idea or a question, email me at <nicolas@n16f.net>.
