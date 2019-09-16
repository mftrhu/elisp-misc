;;; ox-telegram-md.el --- Telegram-flavoured Markdown backend for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2019 mftrhu
;; Author: mftrhu <mftrhu@inventati.org>
;; Created: 2019-09-10
;; Updated: 2019-09-16
;; Version: 0.1
;; Keywords: org-mode, markdown, telegram

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This library implements - from scratch - a backend for the Org
;; generic exporter, targeting whatever shitty flavour of Markdown is
;; available on Telegram.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ox)
(require 'ox-publish)
(require 'ox-md)

;;; Define the back-end
(org-export-define-backend 'telegram-md
  '((template . org-tg-md-template)
    (inner-template . org-tg-md-inner-template)
    ;; Structural blocks
    (section . org-tg-md-section)
    (headline . org-tg-md-headline)
    (paragraph . org-tg-md-paragraph)
    ;; Inlines
    (plain-text . org-tg-md-plain-text)
    (bold . org-tg-md-bold)
    (italic . org-tg-md-italic)
    (code . org-tg-md-verbatim)
    (verbatim . org-tg-md-verbatim)
    (inline-src-block . org-tg-md-verbatim)
    (footnote-reference . org-tg-md-footnote-reference)
    ;; Links
    (link . org-tg-md-link)
    ;; Code blocks
    (example-block . org-tg-md-example-block)
	(export-block . org-tg-md-export-block)
	(fixed-width . org-tg-md-example-block)
    (src-block . org-tg-md-example-block)
    ;; Lists
    (plain-list . org-tg-md-plain-list)
    (item . org-tg-md-item)
    ;; Quote block
    (quote-block . org-tg-md-quote-block)
    ;; Export blocks & snippets (e.g. @@telegram-md:...@@)
    (export-block . org-tg-md-export-block)
    (export-snippet . org-tg-md-export-snippet))
  :filters-alist '((:filter-final-output . org-tg-md-final-function)))

;;; Internal functions
(defun org-tg-md--unfill-string (s)
  (when (stringp s)
    (with-temp-buffer
      (let ((fill-column most-positive-fixnum))
        (insert s)
        (fill-region (point-min) (point-max))
        (buffer-string)))))

(defun org-tg-md--collect-links (contents info)
  (let ((parsetree (org-element-parse-buffer))
        (counter 0))
    (org-element-map parsetree 'link
      (lambda (link)
        ;; Only gather those links which have a description
        (when (org-element-contents link)
          (setq counter (+ 1 counter))
          ;; Annotate the link with a counter, and return it
          (org-element-put-property link :counter counter)
          link))
      info nil nil t)))

(defun org-tg-md--describe-links (links info)
  (mapconcat
   (lambda (link)
     (format "[%s] %s"
             (org-element-property :counter link)
             (org-element-property :raw-link link)))
   links "\n"))

;;; Transcode functions
;;;; Template
(defun org-tg-md-template (contents _info)
  contents)

;;;; Inner template
(defun org-tg-md-inner-template (contents info)
  (concat
   contents
   ;; Notes section
   (let ((definitions (org-export-collect-footnote-definitions info)))
     (when definitions
       (concat
        "\n**Notes**\n"
        (mapconcat
         (lambda (ref)
           (let ((id (car ref))
                 (text (org-export-data (nth 2 ref) info)))
             (format "[%s] %s"
                     (car ref)
                     (org-tg-md--unfill-string (org-trim text)))))
         definitions "\n"))))
   ;; References (link targets) section
   (let ((links (org-tg-md--collect-links contents info)))
     (when links
       (concat
        "\n\n**References**\n"
        (org-tg-md--describe-links links info))))))

;;;; Section
(defun org-tg-md-section (section contents info)
  contents)

;;;; Headline
(defun org-tg-md-headline (headline contents info)
  (let* ((level (org-export-get-relative-level headline info))
         (text (org-export-data (org-element-property :title headline) info)))
    (format "**%s**\n%s" text contents)))

;;;; Paragraph
(defun org-tg-md-paragraph (paragraph contents _info)
  contents)

;;;; Plain text
(defun org-tg-md-plain-text (text info)
  text)

;;;; Bold
(defun org-tg-md-bold (_bold contents _info)
  (format "**%s**" contents))

;;;; Italics
(defun org-tg-md-italic (_italic contents _info)
  (format "__%s__" contents))

;;;; Verbatim
(defun org-tg-md-verbatim (verbatim _contents _info)
  (let ((value (org-element-property :value verbatim)))
    (format "`%s`" value)))

;;;; Footnote reference
(defun org-tg-md-footnote-reference (footnote-reference _contents info)
  (format "[%s]" (org-export-get-footnote-number footnote-reference info)))

;;;; Links
(defun org-tg-md-link (link desc info)
  (let* ((raw-path (org-element-property :path link))
         (type (org-element-property :type link))
         (path (cond
                ((member type '("http" "https" "ftp" "gopher" "mailto"))
                 (url-encode-url (concat type ":" raw-path))))))
    (if desc
        (progn
          (plist-put info :tg-md-counter
                     (+ 1 (or (plist-get info :tg-md-counter) 0)))
          (format "[%s][%s]" desc
                  (plist-get info :tg-md-counter)))
      (format "<%s>" path))))

;;;; Source blocks
(defun org-tg-md-example-block (example-block _contents info)
  (concat
   "\n```\n"
   (org-remove-indentation
    (org-export-format-code-default example-block info))
   "```\n"))

;;;; Lists
(defun org-tg-md-plain-list (plain-list contents info)
  contents)

(defun org-tg-md-item (item contents info)
  (let* ((list-type (org-element-property :type (org-export-get-parent item)))
         (bullet
          (pcase list-type
            (`descriptive
             (format "**%s**\n    "
                     (org-export-data (org-element-property :tag item) info)))
            (`ordered
             (let* ((struct (org-element-property :structure item))
		            (bul (org-list-bullet-string
			              (org-element-property :bullet item)))
		            (num (number-to-string
			              (car (last (org-list-get-item-number
				                      (org-element-property :begin item)
				                      struct
				                      (org-list-prevs-alist struct)
				                      (org-list-parents-alist struct)))))))
               (replace-regexp-in-string "[0-9]+" num bul)))
            (_ (let ((bul (org-list-bullet-string
			               (org-element-property :bullet item))))
                 bul))))
         (indentation (if (eq list-type 'descriptive) 4
                        (string-width bullet))))
    (concat bullet (org-trim contents))
    ))

(defun org-tg-md-item (item contents info)
  "Transcode ITEM element into Markdown format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	     (struct (org-element-property :structure item))
	     (bullet (if (not (eq type 'ordered)) "-"
		           (concat (number-to-string
			                (car (last (org-list-get-item-number
					                    (org-element-property :begin item)
					                    struct
					                    (org-list-prevs-alist struct)
					                    (org-list-parents-alist struct)))))
			               "."))))
    (concat bullet
	        (make-string (- 3 (length bullet)) ? )
	        (pcase (org-element-property :checkbox item)
	          (`on "✅ ")
	          (`trans "✳️ ")
	          (`off "❎ "))
	        (let ((tag (org-element-property :tag item)))
	          (and tag (format "**%s:** "(org-export-data tag info))))
	        (and contents
		         (org-trim (replace-regexp-in-string "^" "    " contents))))))

;;;; Quote block
(defun org-tg-md-quote-block (_quote-block contents _info)
  (replace-regexp-in-string
   "^\\(.+\\)$" "> \\&"
   (replace-regexp-in-string "\n\\'" "" contents)))

;;;; Export blocks and snippets
(defun org-tg-md-export-block (export-block _contents _info)
  (when (string= (org-element-property :type export-block) "TELEGRAM")
    (org-element-property :value export-block)))

(defun org-tg-md-export-snippet (export-snippet _contents _info)
  (when (eq (org-export-snippet-backend export-snippet) 'telegram-md)
    (org-element-property :value export-snippet)))

;;; Filter functions
(defun org-tg-md-final-function (contents _backend info)
  "Remove superfluous newlines from the final output."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
      (replace-match "\n")
      (forward-char 1))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; End-user functions

;;;###autoload
(defun org-telegram-md-export-as-md
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'telegram-md "*Org Telegram Markdown Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

;;;###autoload
(defun org-telegram-md-export-to-md
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
    (let ((file (org-export-output-file-name ".tmd" subtreep)))
      (org-export-to-file 'telegram-md file
        async subtreep visible-only body-only ext-plist)))

(provide 'ox-telegram-md)

;; Local variables:
;; coding: utf-8
;; eval: (outshine-mode 1)
;; End:

;;; ox-telegram-md.el ends here
