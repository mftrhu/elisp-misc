;;; ob-pandoc-embed --- Embed different markups in org-mode via pandoc

;; Copyright (C) 2018 mftrhu
;; Author: mftrhu <mftrhu@inventati.org>
;; Created: 2018-10-27
;; Version: 0.1
;; Keywords: org-mode, pandoc

;;; Commentary:
;; This library defines a Babel exporter for the `pandoc-embed' code
;; block, allowing any of the markup languages supported by Pandoc to
;; be converted to Org and embedded on the fly in the exported
;; results.
;;
;; When exporting to HTML, the original markup source is htmlized and
;; embedded inside of a `<details>' tag for ease of copy-paste.

;;; Usage:
;; Use `pandoc-embed' as source in org code blocks.  Without
;; specifying a language, they are treated as markdown.
;;
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
    (org-babel-eval
     (concat "pandoc"
             " " (org-babel-process-file-name in-file)
             " " "-f " source-format
             " " "-t org"
             " -o " (org-babel-process-file-name out-file)
             ) "")
    (concat
     ;; Get the results of the conversion
     (with-temp-buffer
       (insert-file-contents out-file)
       (buffer-string))
     "\n"
     ;; Also embed the original source
     "#+HTML: <details><summary>Show markdown source</summary>\n"
     ;;"<div class=\"org-src-container\">\n"
     ;;"<pre class=\"src src-markdown\">"
     "#+BEGIN_SRC " source-format "\n"
     ;;"{{{results(src_markdown[]{"
     ;;TODO: this doesn't work too well when dealing with org-mode sources
     body
     ;;(org-html-fontify-code body "markdown")
     ;;"})}}}"
     "\n#+END_SRC\n"
     ;;"</pre>\n"
     ;;"</div>\n"
     "#+HTML: </details>")
    ))

(provide 'ob-pandoc-embed)
;; ob-pandoc-embed.el ends here
