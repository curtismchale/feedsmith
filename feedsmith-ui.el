;;; feedsmith-ui.el --- UI for Feedsmith -*- lexical-binding: t; -*-

;; Author: Curtis McHale
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Feed list (tabulated-list-mode) and article view (shr) for Feedsmith.

;;; Code:

(require 'feedsmith)
(require 'tabulated-list)
(require 'shr)
(require 'dom)

;;;; Faces

(defface feedsmith-unread-face
  '((t :weight bold))
  "Face for unread entries in the feed list."
  :group 'feedsmith)

(defface feedsmith-read-face
  '((t :weight normal))
  "Face for read entries in the feed list."
  :group 'feedsmith)

(defface feedsmith-starred-face
  '((((class color) (background light)) :foreground "#b8860b")
    (((class color) (background dark)) :foreground "#ffd700"))
  "Face for the star indicator."
  :group 'feedsmith)

;;;; Feed list mode

(defvar feedsmith-list--unread-only nil
  "When non-nil, show only unread entries.")

(defvar feedsmith-list--search-filter nil
  "When non-nil, filter entries by this search string.")

(defvar feedsmith-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'feedsmith-list-open)
    (define-key map (kbd "o") #'feedsmith-list-open)
    (define-key map (kbd "O") #'feedsmith-list-open-browser)
    (define-key map (kbd "r") #'feedsmith-list-toggle-read)
    (define-key map (kbd "s") #'feedsmith-list-toggle-starred)
    (define-key map (kbd "R") #'feedsmith-list-org-refile)
    (define-key map (kbd "g") #'feedsmith-list-refresh)
    (define-key map (kbd "G") #'feedsmith-list-sync)
    (define-key map (kbd "u") #'feedsmith-list-toggle-unread-filter)
    (define-key map (kbd "/") #'feedsmith-list-search)
    (define-key map (kbd "q") #'feedsmith-list-quit)
    map)
  "Keymap for `feedsmith-list-mode'.")

(define-derived-mode feedsmith-list-mode tabulated-list-mode "Feedsmith"
  "Major mode for the Feedsmith feed list."
  (setq tabulated-list-format
        [("" 1 nil)           ; unread dot
         ("" 1 nil)           ; star
         ("Date" 10 feedsmith-list--sort-by-date)
         ("Feed" 20 t)
         ("Title" 0 t)])
  (setq tabulated-list-sort-key '("Date" . t))
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun feedsmith-list--sort-by-date (a b)
  "Sort entries A and B by published date descending."
  (let ((da (aref (cadr a) 2))
        (db (aref (cadr b) 2)))
    (string> da db)))

(defun feedsmith-list--format-date (iso-string)
  "Format ISO-STRING as YYYY-MM-DD."
  (if (and iso-string (stringp iso-string) (>= (length iso-string) 10))
      (substring iso-string 0 10)
    ""))

(defun feedsmith-list--entries ()
  "Return the list of tabulated-list entries to display."
  (let ((result nil))
    (when feedsmith--entries
      (maphash
       (lambda (id entry)
         (let ((unread (feedsmith-entry-unread entry))
               (starred (feedsmith-entry-starred entry))
               (title (or (feedsmith-entry-title entry) "(no title)"))
               (feed (or (feedsmith-entry-feed-title entry) ""))
               (date (feedsmith-list--format-date
                      (feedsmith-entry-published entry))))
           ;; Apply filters
           (when (and (or (not feedsmith-list--unread-only) unread)
                      (or (not feedsmith-list--search-filter)
                          (string-match-p
                           (regexp-quote feedsmith-list--search-filter)
                           (concat title " " feed))))
             (let ((face (if unread 'feedsmith-unread-face 'feedsmith-read-face)))
               (push (list id
                           (vector
                            (if unread
                                (propertize "\u2022" 'face 'feedsmith-unread-face)
                              " ")
                            (if starred
                                (propertize "\u2605" 'face 'feedsmith-starred-face)
                              " ")
                            (propertize date 'face face)
                            (propertize feed 'face face)
                            (propertize title 'face face)))
                     result)))))
       feedsmith--entries))
    result))

(defun feedsmith-list-refresh ()
  "Refresh the feed list display."
  (interactive)
  (when (derived-mode-p 'feedsmith-list-mode)
    (setq tabulated-list-entries (feedsmith-list--entries))
    (tabulated-list-print t)
    (let ((total (hash-table-count feedsmith--entries))
          (unread (if feedsmith--unread-ids
                      (hash-table-count feedsmith--unread-ids) 0)))
      (setq mode-line-process
            (format " [%d/%d unread%s%s]"
                    unread total
                    (if feedsmith-list--unread-only " (filtered)" "")
                    (if feedsmith-list--search-filter
                        (format " /%s/" feedsmith-list--search-filter) ""))))))

(defun feedsmith-list-show ()
  "Show the Feedsmith feed list buffer."
  (let ((buf (get-buffer-create "*feedsmith*")))
    (switch-to-buffer buf)
    (unless (derived-mode-p 'feedsmith-list-mode)
      (feedsmith-list-mode))
    (feedsmith-list-refresh)))

;;;; Feed list commands

(defun feedsmith-list--entry-at-point ()
  "Return the `feedsmith-entry' at point, or nil."
  (when-let ((id (tabulated-list-get-id)))
    (gethash id feedsmith--entries)))

(defun feedsmith-list-open ()
  "Open the entry at point in the article view."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    ;; Auto-mark read
    (when (feedsmith-entry-unread entry)
      (feedsmith-mark-read (feedsmith-entry-id entry)))
    (feedsmith-article-show entry)
    (feedsmith-list-refresh)))

(defun feedsmith-list-open-browser ()
  "Open the entry at point in an external browser."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (when-let ((url (feedsmith-entry-url entry)))
      (browse-url url))))

(defun feedsmith-list-toggle-read ()
  "Toggle read state of the entry at point."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (feedsmith-toggle-read (feedsmith-entry-id entry))
    (feedsmith-list-refresh)))

(defun feedsmith-list-toggle-starred ()
  "Toggle starred state of the entry at point."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (feedsmith-toggle-starred (feedsmith-entry-id entry))
    (feedsmith-list-refresh)))

(defun feedsmith-list-org-refile ()
  "Refile the entry at point to Org."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (require 'feedsmith-org)
    (feedsmith-org-refile entry)))

(defun feedsmith-list-sync ()
  "Sync and refresh the feed list."
  (interactive)
  (feedsmith-sync)
  (feedsmith-list-refresh))

(defun feedsmith-list-toggle-unread-filter ()
  "Toggle showing only unread entries."
  (interactive)
  (setq feedsmith-list--unread-only (not feedsmith-list--unread-only))
  (feedsmith-list-refresh)
  (message "Feedsmith: %s" (if feedsmith-list--unread-only
                               "showing unread only"
                             "showing all entries")))

(defun feedsmith-list-search ()
  "Filter entries by a search string."
  (interactive)
  (let ((query (read-string "Search: " feedsmith-list--search-filter)))
    (setq feedsmith-list--search-filter
          (if (string-empty-p query) nil query))
    (feedsmith-list-refresh)))

(defun feedsmith-list-quit ()
  "Quit Feedsmith."
  (interactive)
  (feedsmith--stop-auto-sync)
  (feedsmith--save-cache)
  (quit-window t))

;;;; Article view mode

(defvar feedsmith-article--current-entry nil
  "The entry currently displayed in the article view.")

(defvar feedsmith-article-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "O") #'feedsmith-article-open-browser)
    (define-key map (kbd "R") #'feedsmith-article-org-refile)
    (define-key map (kbd "r") #'feedsmith-article-toggle-read)
    (define-key map (kbd "s") #'feedsmith-article-toggle-starred)
    (define-key map (kbd "n") #'feedsmith-article-next)
    (define-key map (kbd "p") #'feedsmith-article-prev)
    (define-key map (kbd "q") #'feedsmith-article-quit)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "S-SPC") #'scroll-down-command)
    map)
  "Keymap for `feedsmith-article-mode'.")

(define-derived-mode feedsmith-article-mode special-mode "Feedsmith-Article"
  "Major mode for viewing a Feedsmith article."
  (setq-local feedsmith-article--current-entry nil))

(defun feedsmith-article-show (entry)
  "Display ENTRY in the article view buffer."
  (let ((buf (get-buffer-create "*feedsmith-article*")))
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (feedsmith-article-mode)
      (setq feedsmith-article--current-entry entry)
      ;; Header
      (let ((title (or (feedsmith-entry-title entry) "(no title)"))
            (author (feedsmith-entry-author entry))
            (feed (feedsmith-entry-feed-title entry))
            (date (feedsmith-entry-published entry))
            (url (feedsmith-entry-url entry)))
        (insert (propertize title 'face '(:weight bold :height 1.3)) "\n")
        (when author (insert "Author: " author "\n"))
        (when feed (insert "Feed:   " feed "\n"))
        (when date (insert "Date:   " (feedsmith-list--format-date date) "\n"))
        (when url (insert "URL:    " url "\n"))
        (insert "\n" (make-string 72 ?-) "\n\n"))
      ;; Body
      (let ((content (or (feedsmith-entry-content entry)
                         (feedsmith-entry-summary entry)
                         "<p>No content available.</p>")))
        (feedsmith-article--render-html content))
      (goto-char (point-min)))))

(defun feedsmith-article--render-html (html)
  "Render HTML string into the current buffer using shr."
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region (point-min) (point-max)))))
    (shr-insert-document dom)))

;;;; Article commands

(defun feedsmith-article-open-browser ()
  "Open the current article in an external browser."
  (interactive)
  (when-let ((url (and feedsmith-article--current-entry
                       (feedsmith-entry-url feedsmith-article--current-entry))))
    (browse-url url)))

(defun feedsmith-article-org-refile ()
  "Refile the current article to Org."
  (interactive)
  (when feedsmith-article--current-entry
    (require 'feedsmith-org)
    (feedsmith-org-refile feedsmith-article--current-entry)))

(defun feedsmith-article-toggle-read ()
  "Toggle read state of the current article."
  (interactive)
  (when feedsmith-article--current-entry
    (feedsmith-toggle-read
     (feedsmith-entry-id feedsmith-article--current-entry))))

(defun feedsmith-article-toggle-starred ()
  "Toggle starred state of the current article."
  (interactive)
  (when feedsmith-article--current-entry
    (feedsmith-toggle-starred
     (feedsmith-entry-id feedsmith-article--current-entry))))

(defun feedsmith-article--sorted-entry-ids ()
  "Return entry IDs sorted by date descending."
  (let ((entries nil))
    (maphash (lambda (_k v) (push v entries)) feedsmith--entries)
    (setq entries (sort entries
                        (lambda (a b)
                          (string> (or (feedsmith-entry-published a) "")
                                   (or (feedsmith-entry-published b) "")))))
    (mapcar #'feedsmith-entry-id entries)))

(defun feedsmith-article--navigate (direction)
  "Navigate to the next or previous article.
DIRECTION is 1 for next, -1 for previous."
  (when feedsmith-article--current-entry
    (let* ((ids (feedsmith-article--sorted-entry-ids))
           (current-id (feedsmith-entry-id feedsmith-article--current-entry))
           (pos (cl-position current-id ids :test #'equal))
           (new-pos (when pos (+ pos direction))))
      (when (and new-pos (>= new-pos 0) (< new-pos (length ids)))
        (let ((new-entry (gethash (nth new-pos ids) feedsmith--entries)))
          (when new-entry
            ;; Auto-mark read
            (when (feedsmith-entry-unread new-entry)
              (feedsmith-mark-read (feedsmith-entry-id new-entry)))
            (feedsmith-article-show new-entry)))))))

(defun feedsmith-article-next ()
  "Show the next article."
  (interactive)
  (feedsmith-article--navigate 1))

(defun feedsmith-article-prev ()
  "Show the previous article."
  (interactive)
  (feedsmith-article--navigate -1))

(defun feedsmith-article-quit ()
  "Quit article view and return to the feed list."
  (interactive)
  (when (get-buffer "*feedsmith*")
    (switch-to-buffer "*feedsmith*")
    (feedsmith-list-refresh))
  (kill-buffer "*feedsmith-article*"))

(provide 'feedsmith-ui)
;;; feedsmith-ui.el ends here
