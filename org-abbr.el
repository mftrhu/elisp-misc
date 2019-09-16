;;; org-abbr.el --- Abbreviation link syntax for org-mode

;; Copyright (C) 2018 mftrhu
;; Author: mftrhu <mftrhu@inventati.org>
;; Created: 2018-10-22
;; Updated: 2018-11-02
;; Version: 0.2
;; Keywords: org-mode

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; This library provides one new type of org-mode link, `abbr:', to be
;; used for marking up abbreviations.  It can display a tooltip with
;; the description when hovering on the link, or a message in the
;; minibuffer when clicking on it.
;;
;; It has the usual org-link limitations.

;;;; References:
;; - "How to strip decorations (text properties) from a string?" on
;;   Emacs StackExchange, <https://emacs.stackexchange.com/a/31226> -
;; - "Org Element API" on Worg,
;;   <https://orgmode.org/worg/dev/org-element-api.html>
;; - "New link features in org 9" on The Kitchin Research Group,
;;   <http://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/>

;;;; Usage:
;; `abbr' links can be used either inline, like this:
;;
;;     [[abbr:HyperText Transfer Protocol][HTTP]]
;;
;; or with link abbreviations:
;;
;;     #+LINK: HTTP abbr:HyperText Transfer Protocol
;;     [[HTTP]]

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
           ;; N.B. `org-bracket-link-regexp' matches three groups on
           ;; the string "[[abbr:path][description]]":
           ;;  - "abbr:path" with (match-string 1);
           ;;  - "[description]" with (match-string 2);
           ;;  - "description" with (match-string 3).
           (let* ((raw-link (org-element-property :raw-link ctx))
                  ;; `is-local-link' will be true when the link is
                  ;; defined locally - that is, when the link is not a
                  ;; link abbreviation made with #+LINK: elsewhere.
                  (is-local-link (string= (match-string 1) raw-link))
                  (link-text  (match-string 3))
                  ;; `abbreviation' is either available from
                  ;; `link-text', if the link is local, or it's the
                  ;; first subgroup (that is, what should be the
                  ;; path).
                  (abbreviation (if is-local-link
                                    link-text
                                  (match-string 1)))
                  (description (org-element-property :path ctx))
                  ;; `has-description' will be true if `link-text' is
                  ;; true (if the description is given locally) or if
                  ;; the link is not a local link, and defined
                  ;; elsewhere.
                  (has-description (or link-text (not is-local-link))))
             (if has-description
                 (format "%s: %s"
                         (org-abbr--strip-properties abbreviation)
                         description)
               (format "%s (abbreviation)" description))))
          (t
           "No match"))))

(defun org-abbr-export (path description format)
  (cond
   ((eq format 'html)
    (if description
        (format "<abbr title=\"%s\">%s</abbr>" path description)
      ;; This, dirty hack is.
      (let* ((apath (concat "abbr:" path))
             (element (rassoc apath org-link-abbrev-alist-local))
             (description (car element)))
        (format "<abbr title=\"%s\">%s</abbr>" path description))))))

(defun org-abbr-tooltip (window object position)
  (save-excursion
    (select-window window)
    (goto-char position)
    (org-abbr--describe-point)))

(defun org-abbr-follow (path)
  (message (org-abbr--describe-point)))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "abbr"
     :face 'org-abbr-face
     :follow 'org-abbr-follow
     :export 'org-abbr-export
     :help-echo 'org-abbr-tooltip)
  (org-add-link-type
   "abbr"
   'org-abbr-follow
   'org-abbr-export))

;;; Footer

(provide 'org-abbr)

;;; org-abbr.el ends here
