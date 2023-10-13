;;; rfc-mode.el --- RFC document browser and viewer -*- lexical-binding: t -*-

;; Author: Nicolas Martyanoff <nicolas@n16f.net>
;; SPDX-License-Identifier: ISC
;; URL: https://github.com/galdor/rfc-mode
;; Version: 1.4.2
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This package makes it easy to browse and read RFC documents.

;; It offers a Helm-based browser of the list of RFCs as well as
;; some highlighting of hyperlinks when reading the actual RFCs.
;; If you want to browse the list without Helm, you might prefer
;; [rfcview](http://github.com/zeph1e/rfcview.el).

;; Todo:
;; - Use font-lock
;; - Add hyperlinks from the `Table of Contents'

;;; Code:

(require 'helm nil t)
(require 'seq)


(declare-function helm-build-sync-source "helm-source")
(declare-function helm-make-actions "helm-lib")

;;; Configuration:

(defgroup rfc-mode-group nil
  "Tools to browse and read RFC documents."
  :prefix "rfc-mode-"
  :link '(url-link :tag "GitHub" "https://github.com/galdor/rfc-mode")
  :group 'external)

(defface rfc-mode-document-header-face
  '((t :inherit font-lock-comment-face))
  "Face used for RFC document page headers.")

(defface rfc-mode-document-footer-face
  '((t :inherit font-lock-comment-face))
  "Face used for RFC document page footers.")

(defface rfc-mode-document-section-title-face
  '((t :inherit font-lock-keyword-face))
  "Face used for RFC document section titles.")

(defface rfc-mode-browser-ref-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight RFC references in the RFC browser.")

(defface rfc-mode-browser-title-face
  '((t :inherit default))
  "Face used to highlight the title of RFC documents in the RFC browser.")

(defface rfc-mode-browser-title-obsolete-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight the title of obsolete RFC documents in the RFC browser.")

(defface rfc-mode-browser-status-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight RFC document statuses in the RFC browser.")

(defcustom rfc-mode-directory (expand-file-name "~/rfc/")
  "The directory where RFC documents are stored."
  :type 'directory)

(defcustom rfc-mode-document-url
  "https://www.rfc-editor.org/rfc/rfc%s.txt"
  "A `format'able URL for fetching arbitrary RFC documents.
Assume RFC documents are named as e.g. rfc21.txt, rfc-index.txt."
  :type 'string)

(defcustom rfc-mode-browse-input-function
  (if (featurep 'helm) 'helm 'completing-read)
  "Function used by `rfc-mode-browse' to read user input.

Only `read-number', `completing-read' and `helm' are explicitly
supported.  Any other function is called with no arguments and
must return an integer.

Here `completion-read' works best if you use some completion
mode that displays candidates \"vertically\" like `helm' does.
`ivy-mode' is a popular choice.  `fido-mode' in combination
with `icomplete-vertical-mode' should also work well."
  :type '(choice (const read-number)
                 (const completing-read)
                 (const helm)
                 function))

(defcustom rfc-mode-use-original-buffer-names nil
  "Whether RFC document buffers should have the name of the document file.
If nil (the default) then use e.g. *rfc21*, otherwise use e.g. rfc21.txt."
  :type 'boolean)

(defcustom rfc-mode-browser-entry-title-width 60
  "The width of the column containing RFC titles in the browser."
  :type 'integer)

(defcustom rfc-mode-imenu-title "RFC Contents"
  "The title to use if `rfc-mode' adds a RFC Contents menu to the menubar."
  :type 'string)

;;; Misc variables:

(defvar rfc-mode-index-entries nil
  "The list of entries in the RFC index.")

(defconst rfc-mode-title-regexp "^\\(?:[0-9]+\\.\\)+\\(?:[0-9]+\\)? .*$"
  "Regular expression to model section titles in RFC documents.")

(defvar-local rfc-mode--titles nil
  "Buffer-local variable that keeps a list of section titles in this RFC.")

(defvar rfc-mode--last-title nil
  "Last section title that the user visited.")

;;; Keys:

(defvar rfc-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "<tab>")     #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "<prior>")   #'rfc-mode-backward-page)
    (define-key map (kbd "<next>")    #'rfc-mode-forward-page)
    (define-key map (kbd "g")         #'rfc-mode-goto-section)
    (define-key map (kbd "n")         #'rfc-mode-next-section)
    (define-key map (kbd "p")         #'rfc-mode-previous-section)
    map)
  "The keymap for `rfc-mode'.")

;;; Main:

(defun rfc-mode-init ()
  "Initialize the current buffer for `rfc-mode'."
  (setq-local page-delimiter "^.*?\n")
  (rfc-mode-highlight)
  (setq imenu-generic-expression (list (list nil rfc-mode-title-regexp 0)))
  (imenu-add-to-menubar rfc-mode-imenu-title))

(define-obsolete-function-alias 'rfc-mode-quit #'quit-window "rfc-mode-1.4")

(defun rfc-mode-recenter ()
  "Do the same as `recenter-top-bottom' would for the `top' position."
  (rfc-mode-header-start)
  (let ((recenter-positions '(top)))
    (recenter-top-bottom)))

(defun rfc-mode-backward-page ()
  "Scroll to the previous page of the current buffer."
  (interactive)
  (beginning-of-line)
  (unless (looking-at "")
    (backward-page))
  (backward-page)
  (beginning-of-line 1)
  (rfc-mode-recenter))

(defun rfc-mode-forward-page ()
  "Scroll to the next page of the current buffer."
  (interactive)
  (forward-page)
  (beginning-of-line 1)
  (rfc-mode-recenter))

(defun rfc-mode-goto-section (section)  ;FIXME: Why not use imenu for that?
  "Move point to SECTION."
  (interactive
   (let* ((default (if (member rfc-mode--last-title rfc-mode--titles)
                       rfc-mode--last-title
                     (car rfc-mode--titles)))
          (completion-ignore-case t)
          (prompt (concat "Go to section (default " default "): "))
          (chosen (completing-read prompt rfc-mode--titles
                                   nil nil nil nil default)))
     (list chosen)))
  (setq rfc-mode--last-title section)
  (unless (rfc-mode--goto-section section)
    (error "Section %s not found" section)))

(defun rfc-mode--goto-section (section)
  "Move point to SECTION if it exists, otherwise don't move point.
Returns t if section is found, nil otherwise."
  (let ((curpos (point))
        (case-fold-search nil))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" section) (point-max) t)
        (progn
          (beginning-of-line)
          (rfc-mode-recenter)
          t)
      (goto-char curpos)
      nil)))

(defun rfc-mode-next-section (n)
  "Move point to Nth next section (default 1)."
  (interactive "p")
  (let ((case-fold-search nil)
        (start (point)))
    (if (looking-at rfc-mode-title-regexp)
        (forward-line 1))
    (if (re-search-forward rfc-mode-title-regexp (point-max) t n)
        (progn
          (beginning-of-line)
          (rfc-mode-recenter))
      (goto-char (point-max))
      ;; The last line doesn't belong to any section.
      (forward-line -1))
    ;; Ensure we never move back from the starting point.
    (if (< (point) start) (goto-char start))))

(defun rfc-mode-previous-section (n)
  "Move point to Nth previous section (default 1)."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (looking-at rfc-mode-title-regexp)
        (forward-line -1))
    (if (re-search-backward rfc-mode-title-regexp (point-min) t n)
        (progn
          (beginning-of-line)
          (rfc-mode-recenter))
      (goto-char (point-min)))))

;;;###autoload
(defun rfc-mode-read (number)
  "Read the RFC document NUMBER.
Offer the number at point as default."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "RFC number: " (rfc-mode--integer-at-point)))))
  (display-buffer (rfc-mode--document-buffer number)))

(defun rfc-mode-reload-index ()
  "Reload the RFC document index from its original file."
  (interactive)
  (setq rfc-mode-index-entries nil))

(defun rfc-mode--index-entries ()
  (or rfc-mode-index-entries
      (let ((file (rfc-mode--document-file "-index")))
        (setq rfc-mode-index-entries
              (rfc-mode-read-index-file file)))))

;;;###autoload
(defun rfc-mode-browse ()
  "Browse through all RFC documents referenced in the index."
  (interactive)
  (pcase rfc-mode-browse-input-function
    ('read-number
     (display-buffer (rfc-mode--document-buffer
                      (read-number "View RFC document: "
                                   (rfc-mode--integer-at-point)))))
    ('helm
     (if (fboundp 'helm)
	 (helm :buffer "*helm rfc browser*"
	       :sources (rfc-mode-browser-helm-sources
			 (rfc-mode--index-entries)))
       (user-error "Helm has to be installed explicitly")))
    ('completing-read
     (let* ((default (rfc-mode--integer-at-point))
            (cands (mapcar (lambda (entry)
                             (let ((cand
                                    (rfc-mode-browser-format-candidate entry)))
                               (and (numberp default)
                                    (= (plist-get entry :number) default)
                                    (setq default (car cand)))
                               cand))
                           (rfc-mode--index-entries)))
            (choice (completing-read "View RFC document: "
                                     cands nil nil nil nil default))
            (number (or (and (string-match "\\`RFC\\([0-9]+\\)" choice)
                             (string-to-number (match-string 1 choice)))
                        (ignore-errors (string-to-number choice)))))
       (unless number
         (user-error
          "%s doesn't match a completion candidate and is not a number"
          choice))
       (display-buffer (rfc-mode--document-buffer number))))
    (_ (display-buffer (rfc-mode--document-buffer
                        (funcall rfc-mode-browse-input-function))))))

;;;###autoload
(define-derived-mode rfc-mode special-mode "rfc-mode"
  "Major mode to browse and read RFC documents."
  (rfc-mode-init))

;;;###autoload
(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\'" . rfc-mode))
(add-to-list 'auto-mode-alist '("\\*rfc[0-9]+\\*\\'" . rfc-mode))

;;; Syntax utils:

(defun rfc-mode-highlight ()
  "Highlight the current buffer."
  (setq rfc-mode--titles nil)
  ;; FIXME: Use font-lock!
  (with-silent-modifications
    (let ((inhibit-read-only t))
      ;; Headers and footers
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "" nil t)
          (beginning-of-line)
          (let ((form-feed (point)))
            (let* ((footer-end (rfc-mode-previous-footer-start))
                   (footer-start (point)))
              (put-text-property
               footer-start footer-end
               'face 'rfc-mode-document-footer-face))
            (goto-char form-feed)
            (let* ((header-end (rfc-mode-header-start))
                   (header-start (point)))
              (put-text-property
               header-start header-end
               'face 'rfc-mode-document-header-face)
              (goto-char header-end)))))
      ;; Section titles
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp rfc-mode-title-regexp nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (put-text-property start end
                               'face 'rfc-mode-document-section-title-face)
            (push (match-string 0) rfc-mode--titles)
            (goto-char end))))
      ;; Keep titles in expected top to bottom order.
      (setq rfc-mode--titles (nreverse rfc-mode--titles))
      ;; RFC references
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "RFC *\\([0-9]+\\)" nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (number (string-to-number (match-string 1))))
            (unless (= start (line-beginning-position))
              (make-text-button start end
                                'action (lambda (_button)
                                          (rfc-mode-read number))
                                'help-echo (format "Read RFC %d" number)
                                'follow-link t))
            (goto-char end)))))))

(defun rfc-mode-header-start ()
  "Move to the start of the current header.

When the point is on a form feed character, move it to the start
of the current page header and return the position of the end of
the header."
  (when (looking-at "")
    (forward-line 1)
    (move-end-of-line 1)
    (prog1 (point)
      (move-beginning-of-line 1))))

(defun rfc-mode-previous-footer-start ()
  "Move to the start of the previous footer.

When the point is on a form feed character, move it to the start
of the previous page footer and return the position of the end of
the footer."
  (when (looking-at "")
    (forward-line -1)
    (move-end-of-line 1)
    (prog1 (point)
      (move-beginning-of-line 1))))

;;; Browser utils:

(defun rfc-mode-browser-helm-sources (entries)
  "Create a Helm source for ENTRIES.

ENTRIES is a list of RFC index entries in the browser."
  (helm-build-sync-source "RFC documents"
    :candidates (mapcar #'rfc-mode-browser-format-candidate entries)
    :action (helm-make-actions
             "Read" #'rfc-mode-browser-helm-entry-read)))

(defun rfc-mode-browser-format-candidate (entry)
  "Create a Helm candidate for ENTRY.

ENTRY is a RFC index entry in the browser."
  (let* ((ref (rfc-mode--pad-string
               (format "RFC%d" (plist-get entry :number)) 7))
         (title (rfc-mode--pad-string
                 (plist-get entry :title)
                 rfc-mode-browser-entry-title-width))
         (status (or (plist-get entry :status) ""))
         (obsoleted-by (plist-get entry :obsoleted-by))
         (obsoletep (> (length obsoleted-by) 0))
         (string (format "%s  %s  %s"
                         (rfc-mode--highlight-string
                          ref 'rfc-mode-browser-ref-face)
                         (rfc-mode--highlight-string
                          title (if obsoletep
                                    'rfc-mode-browser-title-obsolete-face
                                  'rfc-mode-browser-title-face))
                         (rfc-mode--highlight-string
                          status 'rfc-mode-browser-status-face))))
    (cons string entry)))

(defun rfc-mode-browser-helm-entry-read (entry)
  "The read action the Helm candidate ENTRY in the browser."
  (let ((number (plist-get entry :number)))
    (rfc-mode-read number)))

;;; Index utils:

(defun rfc-mode-read-index-file (filename)
  "Read an RFC index file at FILENAME and return a list of entries."
  (with-temp-buffer
    (insert-file-contents filename)
    (rfc-mode-read-index (current-buffer))))

(defun rfc-mode-read-index (buffer)
  "Read an RFC index file from BUFFER and return a list of entries."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((entries nil))
      (while (search-forward-regexp "^[0-9]+ " nil t)
        (let ((start (match-beginning 0)))
          (search-forward-regexp " $")
          (let* ((end (match-beginning 0))
                 (lines (buffer-substring start end))
                 (entry-string (replace-regexp-in-string "[ \n]+" " " lines))
                 (entry (rfc-mode-parse-index-entry entry-string)))
            (unless (string= (plist-get entry :title) "Not Issued")
              (push entry entries)))))
      (nreverse entries))))

(defun rfc-mode-parse-index-entry (string)
  "Parse the RFC document index entry STRING and return it as a plist."
  (unless (string-match "\\(^[0-9]+\\) *\\(.*?\\)\\.\\(?: \\|$\\)" string)
    (error "Invalid index entry format: %S" string))
  (let* ((number-string (match-string 1 string))
         (number (string-to-number number-string))
         (title (match-string 2 string)))
    (when (zerop number)
      (error "Invalid index entry number: %S" number-string))
    (let ((entry (list :number number :title title)))
      (when (string-match "(Status: \\([^)]+\\))" string)
        (plist-put entry :status (downcase (match-string 1 string))))
      (when (string-match "(Obsoletes \\([^)]+\\))" string)
        (plist-put entry :obsoletes
                   (rfc-mode--parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Obsoleted by \\([^)]+\\))" string)
        (plist-put entry :obsoleted-by
                   (rfc-mode--parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Updates \\([^)]+\\))" string)
        (plist-put entry :updates
                   (rfc-mode--parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Updated by \\([^)]+\\))" string)
        (plist-put entry :updated-by
                   (rfc-mode--parse-rfc-refs (match-string 1 string))))
      entry)))

;;; Document utils:

(defun rfc-mode--document-buffer-name (number)
  "Return the buffer name for the RFC document NUMBER."
  (concat "*rfc" (number-to-string number) "*"))

(defun rfc-mode--document-file (number)
  "Return the absolute file name of the RFC document NUMBER."
  (let ((file
         (expand-file-name (format "rfc%s.txt" number) rfc-mode-directory)))
    (rfc-mode--ensure-directory-exists)
    (unless (file-exists-p file)
      (url-copy-file (format rfc-mode-document-url number) file))
    file))

(defun rfc-mode--document-buffer (number)
  "Return a buffer visiting the RFC document NUMBER.

The buffer is created if it does not exist."
  (let* ((buffer-name (rfc-mode--document-buffer-name number))
         (document-path (rfc-mode--document-file number)))
    (with-current-buffer (find-file-noselect document-path)
      (unless rfc-mode-use-original-buffer-names
        (rename-buffer buffer-name))
      (rfc-mode)
      (current-buffer))))

;;; Misc utils:

(defun rfc-mode--integer-at-point ()
  ;; Note that we don't use `number-at-point' as it will match
  ;; number formats that make no sense as RFC numbers (floating
  ;; point, hexadecimal, etc.).
  (save-excursion
    (skip-chars-backward "0-9")
    (and (looking-at "[0-9]")
         (string-to-number
          (buffer-substring-no-properties
           (point)
           (progn (skip-chars-forward "0-9")
                  (point)))))))

(defun rfc-mode--ensure-directory-exists ()
  "Check that `rfc-mode-directory' exists, creating it if it does not."
  (when (and (not (file-exists-p rfc-mode-directory))
             (y-or-n-p (format "Create directory %s? " rfc-mode-directory)))
    (make-directory rfc-mode-directory t)))

(defun rfc-mode--parse-rfc-ref (string)
  "Parse a reference to a RFC document from STRING.

For example: \"RFC 2822\"."
  (when (string-match "^RFC *\\([0-9]+\\)" string)
    (string-to-number (match-string 1 string))))

(defun rfc-mode--parse-rfc-refs (string)
  "Parse a list of references to RFC documents from STRING.

For example: \"RFC3401, RFC3402 ,RFC 3403\"."
  (seq-remove #'null (mapcar #'rfc-mode--parse-rfc-ref
                             (split-string string "," t " +"))))

(defun rfc-mode--pad-string (string width)
  "Pad STRING with spaces to WIDTH characters."
  (truncate-string-to-width string width 0 ?\s))

(defun rfc-mode--highlight-string (string face)
  "Highlight STRING using FACE."
  (put-text-property 0 (length string) 'face face string)
  string)

(provide 'rfc-mode)

;;; rfc-mode.el ends here
