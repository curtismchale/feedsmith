;;; feedsmith-org.el --- Org refile integration for Feedsmith -*- lexical-binding: t; -*-

;; Author: Curtis McHale
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Provides `feedsmith-org-refile' which creates a temporary Org buffer
;; with the article formatted as a headline and calls `org-refile'.

;;; Code:

(require 'feedsmith-backend)
(require 'org)
(require 'shr)

;;;; Customization

(defcustom feedsmith-org-refile-summary-paragraphs 3
  "Number of paragraphs to include in the Org refile body."
  :type 'integer
  :group 'feedsmith)

;;;; Body extraction

(defun feedsmith-org--extract-plain-text (html)
  "Convert HTML to plain text using shr, return as string."
  (with-temp-buffer
    (let ((dom (with-temp-buffer
                 (insert html)
                 (libxml-parse-html-region (point-min) (point-max)))))
      (let ((shr-use-fonts nil)
            (shr-width 72))
        (shr-insert-document dom)))
    (buffer-string)))

(defun feedsmith-org--first-n-paragraphs (text n)
  "Return the first N paragraphs from TEXT.
Paragraphs are separated by blank lines."
  (let ((paragraphs (split-string text "\n\n+" t "[ \t\n]*"))
        (result nil))
    (dotimes (i (min n (length paragraphs)))
      (push (nth i paragraphs) result))
    (string-join (nreverse result) "\n\n")))

;;;; Refile command

;;;###autoload
(defun feedsmith-org-refile (entry)
  "Refile ENTRY to an Org file using `org-refile'.
Creates a temporary Org buffer with the entry formatted as a headline,
positions point on it, and invokes `org-refile'."
  (let* ((title (or (feedsmith-entry-title entry) "(no title)"))
         (url (feedsmith-entry-url entry))
         (date (feedsmith-entry-published entry))
         (author (feedsmith-entry-author entry))
         (feed (feedsmith-entry-feed-title entry))
         (html (or (feedsmith-entry-content entry)
                   (feedsmith-entry-summary entry)
                   ""))
         (body (if (string-empty-p html)
                   ""
                 (feedsmith-org--first-n-paragraphs
                  (feedsmith-org--extract-plain-text html)
                  feedsmith-org-refile-summary-paragraphs)))
         ;; Ensure org-refile-targets has a fallback
         (org-refile-targets
          (or org-refile-targets
              `((,(expand-file-name "inbox.org" "~/org") :maxlevel . 2)))))
    (with-current-buffer (get-buffer-create "*feedsmith-refile*")
      (erase-buffer)
      (org-mode)
      ;; Build the headline
      (insert "* " (if url (format "[[%s][%s]]" url title) title) "\n")
      (insert ":PROPERTIES:\n")
      (when url    (insert ":URL: " url "\n"))
      (when date   (insert ":DATE: " date "\n"))
      (when author (insert ":AUTHOR: " author "\n"))
      (when feed   (insert ":FEED: " feed "\n"))
      (insert ":END:\n\n")
      (when (not (string-empty-p body))
        (insert body "\n"))
      ;; Position on headline and refile
      (goto-char (point-min))
      (condition-case err
          (progn
            (org-refile)
            (message "Feedsmith: refiled \"%s\"" title))
        (quit (message "Feedsmith: refile cancelled")))
      (kill-buffer "*feedsmith-refile*"))))

(provide 'feedsmith-org)
;;; feedsmith-org.el ends here
