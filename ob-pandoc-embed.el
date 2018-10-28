;;; ob-pandoc-embed --- Embed different markups in org-mode via pandoc

;; Copyright (C) 2018 mftrhu
;; Author: mftrhu <mftrhu@inventati.org>
;; Created: 2018-10-27
;; Version: 0.2
;; Keywords: org-mode, pandoc

;;; Commentary:
;; This library defines a Babel exporter for the `pandoc-embed'/
;; `pandoc' code block, allowing any of the markup languages supported
;; by Pandoc to be converted to Org and embedded on the fly in the
;; exported results.
;;
;; When exporting `pandoc-embed' blocks to HTML, the original markup
;; source is htmlized and embedded inside of a `<details>' tag for
;; ease of copy-paste.

;;; Usage:
;; Use `pandoc' or `pandoc-embed' as source in org code blocks.
;; Without specifying a language, they are treated as markdown.
;;
;;    #+BEGIN_SRC pandoc
;;    #+BEGIN_SRC pandoc-embed
;;
;; Any one of the markup languages supported by pandoc can be
;; specified afterwards, but the specified language will also be used
;; as the language of the embedded source block.  E.g.
;;
;;    #+BEGIN_SRC pandoc-embed markdown_strict
;;
;; will be exported to org without any issues, but its source won't be
;; colorized as Emacs/Org does not understand `markdown_strict' as a
;; language.

;;; Code:

(defun org-pandoc-embed--pandoc (in-file out-file source-format)
  (org-babel-eval
   (concat "pandoc"
           " " (org-babel-process-file-name in-file)
           " " "-f " source-format
           " " "-t org"
           " -o " (org-babel-process-file-name out-file)
           ) ""))

;; * Pandoc
(defvar org-babel-default-header-args:pandoc
  '((:results . "raw")
    (:exports . "results")))

(defun org-babel-expand-body:pandoc (body params &optional processed-params)
  body)

(defun org-babel-execute:pandoc (body params)
  "Exports a source block written in one of the markup formats understood by Pandoc, keeping the source.  When exporting to HTML it will be wrapped in a <details> tag.  For this to actually work - for the export to be parsed by Org-mode - it needs to be made with `:results' set to `raw'.

Without any additional parameters, it defaults to `markdown'.

This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (org-babel-temp-file "pandoc-embed-out-"))
         (in-file (org-babel-temp-file "pandoc-embed-"))
         (last-param (car (car (last params))))
         (source-format (if (eq last-param :tangle)
                            "markdown"
                          (symbol-name last-param))))
    ;; Put the contents of the block inside `in-file'
    (with-temp-file in-file
      (insert (org-babel-expand-body:pandoc body params)))
    ;; Execute pandoc to convert the source to Org-mode
    (org-pandoc-embed--pandoc in-file out-file source-format)
    (concat
     ;; Get the results of the conversion
     (with-temp-buffer
       (insert-file-contents out-file)
       (buffer-string))
     "\n")))

;; * Pandoc-embed
(defvar org-babel-default-header-args:pandoc-embed
  '((:results . "raw")
    (:exports . "results")))

(defun org-babel-expand-body:pandoc-embed (body params &optional processed-params)
  body)

(defun org-babel-execute:pandoc-embed (body params)
  "Exports a source block written in one of the markup formats understood by Pandoc, keeping the source.  When exporting to HTML it will be wrapped in a <details> tag.  For this to actually work - for the export to be parsed by Org-mode - it needs to be made with `:results' set to `raw'.

Without any additional parameters, it defaults to `markdown'.

This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (org-babel-temp-file "pandoc-embed-out-"))
         (in-file (org-babel-temp-file "pandoc-embed-"))
         (last-param (car (car (last params))))
         (source-format (if (eq last-param :tangle)
                            "markdown"
                          (symbol-name last-param))))
    ;; Put the contents of the block inside `in-file'
    (with-temp-file in-file
      (insert (org-babel-expand-body:pandoc-embed body params)))
    ;; Execute pandoc to convert the source to Org-mode
    (org-pandoc-embed--pandoc in-file out-file source-format)
    (concat
     ;; Get the results of the conversion
     (with-temp-buffer
       (insert-file-contents out-file)
       (buffer-string))
     "\n"
     ;; Also embed the original source
     "#+HTML: <details><summary>Show markdown source</summary>\n"
     "#+BEGIN_SRC " source-format "\n"
     ;;TODO: this doesn't work too well when dealing with org-mode sources
     body
     "\n#+END_SRC\n"
     "#+HTML: </details>")))

(provide 'ob-pandoc-embed)
;; ob-pandoc-embed.el ends here
