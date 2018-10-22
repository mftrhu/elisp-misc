;;; org-abbr.el --- Abbreviation link syntax for org-mode

(defface org-abbr-face
  `((t (:inherit default
                 :foreground "DarkOrange1")))
  "Face for org-mode abbreviation (`abbr:') links.")

(defun org-abbr--strip-properties (string)
  "Remove all the text properties from the string `string'."
  (let ((start 0)
        (end (length string)))
    (set-text-properties start end nil string)
    string))

(defun org-abbr--describe-point ()
  "Describe the abbreviation at the current point."
  (let* ((ctx (org-element-context)))
    (goto-char (org-element-property :begin ctx))
    (cond ((looking-at org-plain-link-re)
           ;; Bare abbr:PATH link - no description
           (format "%s (abbreviation)" (org-element-property :path ctx)))
          ((looking-at org-bracket-link-regexp)
           ;; Bracketed link, with or without description
           (let* ((link-text  (match-string 3))
                  (description (org-abbr--strip-properties
                                (or link-text (match-string 2))))
                  (path (org-element-property :path ctx)))
             (if (or description (not (string= link-text description)))
                 (format "%s: %s" description path)
               (format "%s (abbreviation)" path)
               )))
          (t
           "No match"))))

(defun org-abbr-export (path description format)
  (cond
   ((eq format 'html)
    (format "<abbr title=\"%s\">%s</abbr>" path description))))

(defun org-abbr-tooltip (window object position)
  (save-excursion
    (select-window window)
    (goto-char position)
    (org-abbr--describe-point)))

(defun org-abbr-follow (path)
  (message (org-abbr--describe-point)))

(org-link-set-parameters
 "abbr"
 :face 'org-abbr-face
 :follow 'org-abbr-follow
 :export 'org-abbr-export
 :help-echo 'org-abbr-tooltip)

(provide 'org-abbr)
