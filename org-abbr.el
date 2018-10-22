;;; org-abbr.el --- Abbreviation link syntax for org-mode

(defface org-abbr-face
  `((t (:inherit default
                 :foreground "DarkOrange1")))
  "Face for org-mode abbreviation (`abbr:') links.")

(defun org-abbr--strip-properties (string)
  (let ((start 0)
        (end (length string)))
    (set-text-properties start end nil string)
    string))

(defun org-abbr--describe-point ()
  (let* ((ctx (org-element-context))
         (begin (org-element-property :begin ctx)))
    ;;(message (org-element-property :begin ctx))
    (message (format "%s" begin))
    (goto-char begin)
    (cond ((looking-at org-plain-link-re)
           ;; Bare abbr:PATH link - no description
           ;;(format "Looking at %s with mouse at %s" (match-string 0) position)
           (format "%s (abbreviation)" (org-element-property :path ctx)))
          ((looking-at org-bracket-link-regexp)
           ;; Bracketed link, with or without description
           (let ((description ;(buffer-substring-no-properties
                               ;(org-element-property :contents-begin ctx)
                               ;(org-element-property :contents-end ctx))
                  (org-abbr--strip-properties (match-string 3)) )
                 (path (org-element-property :path ctx)))
             (format "%s: %s" description path)))
          (t
           "No match"))))

(defun org-abbr-tooltip (window object position)
  (save-excursion
    (select-window window)
    (goto-char position)
    (let ((ctx (org-element-context)))
      (goto-char (org-element-property :begin ctx))
      (cond ((looking-at org-plain-link-re)
             (format "Looking at %s with mouse at %s" (match-string 0) position)
             (format "???: %s" (org-element-property :path ctx)))
            ((looking-at org-bracket-link-regexp)
             (format "%s: %s"
                     (match-string 3)
                     ;;(org-element-property :contents-begin (org-element-context))
                     ;;(org-element-property :contents-end (org-element-context))
                     (org-element-property :path ctx)
                     ;;(match-string 0) position
                     ))
            (t
             "No match")))))

(defun org-abbr-follow (path)
  (message (org-abbr--describe-point)))

(org-link-set-parameters
 "abbr"
 :follow 'org-abbr-follow
 :export (lambda (path description format)
           (cond
            ((eq format 'html)
             (format "<abbr title=\"%s\">%s</abbr>" path description))))
 :face 'org-abbr-face
 :help-echo 'org-abbr-tooltip)

(provide 'org-abbr)
