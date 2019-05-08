;;; rfc-mode.el --- RFC document browser and viewer -*- lexical-binding: t -*-

;; Author: Nicolas Martyanoff <khaelin@gmail.com>
;; URL: https://github.com/galdor/rfc-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (helm "3.2"))

;; Copyright 2019 Nicolas Martyanoff <khaelin@gmail.com>
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
;; IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; This package makes it easy to browse and read RFC documents.

;;; Code:

;;; Configuration
(defgroup rfc-mode-group nil
  "Tools to browse and read RFC documents."
  :prefix "rfc-mode-"
  :group 'external)

(defface rfc-mode-document-header-face
  '((t :inherit font-lock-comment-face))
  "Face used for RFC document page headers.")

(defface rfc-mode-document-section-title-face
  '((t :inherit font-lock-keyword-face))
  "Face used for RFC document section titles.")

(defface rfc-mode-browser-ref-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight RFC references in the RFC browser.")

(defface rfc-mode-browser-title-face
  '((t :inherit default))
  "Face used to highlight the title of RFC documents in the RFC
  browser.")

(defface rfc-mode-browser-title-obsolete-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight the title of obsolete RFC documents in
  the RFC browser.")

(defface rfc-mode-browser-status-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight RFC document statuses in the RFC'
  browser.")

(defcustom rfc-mode-directory (expand-file-name "~/rfc/")
  "The directory where RFC documents are stored."
  :type 'directory
  :group 'rfc-mode)

(defcustom rfc-mode-browser-entry-title-width 60
  "The width of the column containing the title of each entry in
the RFC browser."
  :type 'integer
  :group 'rfc-mode)

;;; Misc variables
(defvar rfc-mode-index-path (concat rfc-mode-directory "rfc-index.txt")
  "The path of the file containing the index of all RFC documents.")

(defvar rfc-mode-index-entries nil
  "The list of entries in the RFC index.")

;;; Keys
(defvar rfc-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'rfc-mode-quit)
    (define-key map (kbd "<prior>") 'rfc-mode-backward-page)
    (define-key map (kbd "<next>") 'rfc-mode-forward-page)
    map)
  "The keymap for `rfc-mode'.")

;;; Main
(defun rfc-mode-init ()
  "Initialize the current buffer for `rfc-mode'."
  (setq-local buffer-read-only t)
  (setq-local page-delimiter "^.*?\n")
  (rfc-mode-highlight))

(defun rfc-mode-quit ()
  "Quit the current window and bury its buffer."
  (interactive)
  (quit-window))

(defun rfc-mode-backward-page ()
  "Scroll to the previous page of the current buffer."
  (interactive)
  (backward-page)
  (rfc-mode-previous-header)
  (recenter 0))

(defun rfc-mode-forward-page ()
  "Scroll to the next page of the current buffer."
  (interactive)
  (forward-page)
  (rfc-mode-previous-header)
  (recenter 0))

(defun rfc-mode-read (number)
  "Read a RFC document."
  (interactive "nRFC number: ")
  (switch-to-buffer (rfc-mode-document-buffer number)))

(defun rfc-mode-reload-index ()

  "Reload the RFC document index from its original file."
  (interactive)
  (setq rfc-mode-index-entries
        (rfc-mode-read-index-file rfc-mode-index-path)))

(defun rfc-mode-browse ()
  "Browse through all RFC documents referenced in the index using Helm."
  (interactive)
  (unless rfc-mode-index-entries
    (setq rfc-mode-index-entries
          (rfc-mode-read-index-file rfc-mode-index-path)))
  (helm :buffer "*helm rfc browser*"
        :sources (rfc-mode-browser-helm-sources rfc-mode-index-entries)))

;;;###autoload
(define-derived-mode rfc-mode fundamental-mode "rfc-mode"
  "Major mode to browse and read RFC documents."
  :syntax-table text-mode-syntax-table
  :mode-map rfc-mode-map
  (rfc-mode-init))

;;;###autoload
(add-to-list 'auto-mode-alist '("rfc[0-9]+\\.txt\\'" . rfc-mode))

;;; Syntax utils
(defun rfc-mode-highlight ()
  "Highlight the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      ;; Headers
      (save-excursion
        (goto-char (point-min))
        (cl-loop
         (let* ((end (rfc-mode-next-header))
                (start (point)))
           (unless end
             (cl-return))
           (put-text-property start end
                              'face 'rfc-mode-document-header-face)
           (goto-char end))))
      ;; Section titles
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "^\\([0-9]+\\.\\)+ .*$" nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (put-text-property start end
                               'face 'rfc-mode-document-section-title-face)
            (goto-char end))))
      ;; RFC references
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "RFC *\\([0-9]+\\)" nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (number (string-to-number (match-string 1))))
            (make-text-button start end
                              'action `(lambda (button)
                                         (rfc-mode-read ,number))
                              'help-echo (format "Read RFC %d" number))
            (goto-char end)))))))

(defun rfc-mode-header-start ()
  "When the point is on a linebreak, move it to the start of the
  current page header and return the position of the end of the
  header."
  (when (looking-at "")
    (forward-line 1)
    (move-end-of-line 1)
    (let ((end (point)))
      (forward-line -2)
      (move-beginning-of-line 1)
      end)))

(defun rfc-mode-previous-header ()
  "Move to the start of the previous page header and return the
position of its end. Return NIL if no previous header is found."
  (when (search-backward "" nil t)
    (goto-char (match-beginning 0))
    (rfc-mode-header-start)))

(defun rfc-mode-next-header ()
  "Move to the start of the next page header and return the
position of its end. Return NIL if no next header is found."
  (when (search-forward "" nil t)
    (goto-char (match-beginning 0))
    (rfc-mode-header-start)))

;;; Browser utils
(defun rfc-mode-browser-helm-sources (entries)
  "Create a Helm source for a list of RFC index entries in the browser."
  (helm-build-sync-source "RFC documents"
    :candidates (mapcar #'rfc-mode-browser-helm-candidate entries)
    :action (helm-make-actions
             "Read" #'rfc-mode-browser-helm-entry-read)))

(defun rfc-mode-browser-helm-candidate (entry)
  "Create a Helm candidate for a RFC index entry in the browser."
  (let* ((ref (rfc-mode-pad-string
               (format "RFC%d" (plist-get entry :number)) 7))
         (title (rfc-mode-pad-string
                 (plist-get entry :title)
                 rfc-mode-browser-entry-title-width))
         (status (or (plist-get entry :status) ""))
         (obsoleted-by (plist-get entry :obsoleted-by))
         (obsoletep (> (length obsoleted-by) 0))
         (string (format "%s  %s  %s"
                         (rfc-mode-highlight-string
                          ref 'rfc-mode-browser-ref-face)
                         (rfc-mode-highlight-string
                          title (if obsoletep
                                    'rfc-mode-browser-title-obsolete-face
                                  'rfc-mode-browser-title-face))
                         (rfc-mode-highlight-string
                          status 'rfc-mode-browser-status-face))))
    (cons string entry)))

(defun rfc-mode-browser-helm-entry-read (entry)
  "The read action for Helm candidates in the browser."
  (let ((number (plist-get entry :number)))
    (rfc-mode-read number)))

;;; Index utils
(defun rfc-mode-read-index-file (path)
  "Read an RFC index file at PATH and return a list of entries."
  (with-temp-buffer
    (insert-file-contents path)
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
  "Parse an entry in the RFC document index and return it as a plist."
  (unless (string-match "\\(^[0-9]+\\) *\\(.*?\\)\\.\\(?: \\|$\\)" string)
    (error "invalid index entry format: %S" string))
  (let* ((number-string (match-string 1 string))
         (number (string-to-number number-string))
         (title (match-string 2 string)))
    (unless number
      (error "invalid index entry number: ~S" number-string))
    (let ((entry (list :number number :title title)))
      (when (string-match "(Status: \\([^)]+\\))" string)
        (plist-put entry :status (downcase (match-string 1 string))))
      (when (string-match "(Obsoletes \\([^)]+\\))" string)
        (plist-put entry :obsoletes
                   (rfc-mode-parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Obsoleted by \\([^)]+\\))" string)
        (plist-put entry :obsoleted-by
                   (rfc-mode-parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Updates \\([^)]+\\))" string)
        (plist-put entry :updates
                   (rfc-mode-parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Updated by \\([^)]+\\))" string)
        (plist-put entry :updated-by
                   (rfc-mode-parse-rfc-refs (match-string 1 string))))
      entry)))

;;; Document utils
(defun rfc-mode-document-buffer-name (number)
  "Return the buffer name for a RFC document."
  (concat "*rfc" (number-to-string number) "*"))

(defun rfc-mode-document-path (number)
  "Return the absolute path of a RFC document."
  (concat rfc-mode-directory "rfc" (number-to-string number) ".txt"))

(defun rfc-mode-document-buffer (number)
  "Return a buffer visiting a RFC document, creating it if necessary."
  (let* ((buffer-name (rfc-mode-document-buffer-name number))
         (document-path (rfc-mode-document-path number)))
    (find-file document-path)
    (rename-buffer buffer-name)
    (rfc-mode)
    (current-buffer)))

;;; Misc utils

(defun rfc-mode-parse-rfc-ref (string)
  "Parse a reference to a RFC document, e.g. \"RFC 2822\"."
  (when (string-match "^RFC *\\([0-9]+\\)" string)
    (string-to-number (match-string 1 string))))

(defun rfc-mode-parse-rfc-refs (string)
  "Parse a list of references to a RFC document, e.g. \"RFC3401,
  RFC3402 ,RFC 3403\"."
  (seq-remove #'null (mapcar #'rfc-mode-parse-rfc-ref
                             (split-string string "," t " +"))))

(defun rfc-mode-pad-string (string width)
  (truncate-string-to-width string width 0 ?\s))

(defun rfc-mode-highlight-string (string face)
  (put-text-property 0 (length string) 'face face string)
  string)


(provide 'rfc-mode)

;;; rfc-mode.el ends here
