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
  '((t :inherit bold))
  "Face for unread entries in the feed list."
  :group 'feedsmith)

(defface feedsmith-read-face
  '((t :inherit shadow))
  "Face for read entries in the feed list."
  :group 'feedsmith)

(defface feedsmith-starred-face
  '((t :inherit warning))
  "Face for the star indicator."
  :group 'feedsmith)

(defface feedsmith-date-face
  '((t :inherit font-lock-comment-face))
  "Face for dates in the feed list."
  :group 'feedsmith)

(defface feedsmith-feed-face
  '((t :inherit font-lock-keyword-face))
  "Face for feed names in the feed list."
  :group 'feedsmith)

(defface feedsmith-title-face
  '((t :inherit font-lock-string-face))
  "Face for entry titles in the feed list."
  :group 'feedsmith)

(defface feedsmith-title-unread-face
  '((t :inherit (feedsmith-title-face bold)))
  "Face for unread entry titles in the feed list."
  :group 'feedsmith)

(defface feedsmith-article-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.3))
  "Face for the article title in the article view."
  :group 'feedsmith)

(defface feedsmith-article-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for article metadata (author, feed, date, URL)."
  :group 'feedsmith)

(defface feedsmith-link-number-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for numbered link indicators in article view."
  :group 'feedsmith)

;;;; Feed list mode

(defvar feedsmith-list--unread-only t
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
  (tabulated-list-init-header)
  (add-hook 'window-size-change-functions #'feedsmith--centering-hook nil t))

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
             (let ((title-face (if unread 'feedsmith-title-unread-face 'feedsmith-title-face)))
               (push (list id
                           (vector
                            (if unread
                                (propertize "\u2022" 'face 'feedsmith-unread-face)
                              " ")
                            (if starred
                                (propertize "\u2605" 'face 'feedsmith-starred-face)
                              " ")
                            (propertize date 'face 'feedsmith-date-face)
                            (propertize feed 'face 'feedsmith-feed-face)
                            (propertize title 'face title-face)))
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
    (feedsmith-list-refresh)
    (when-let ((win (get-buffer-window buf)))
      (feedsmith--update-centering win))))

;;;; Feed list commands

(defun feedsmith-list--entry-at-point ()
  "Return the `feedsmith-entry' at point, or nil."
  (when-let ((id (tabulated-list-get-id)))
    (gethash id feedsmith--entries)))

(defun feedsmith-list-open ()
  "Open the entry at point in the article view."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (when (feedsmith-entry-unread entry)
      (feedsmith-mark-read (feedsmith-entry-id entry))
      (feedsmith-list--update-entry-at-point))
    (feedsmith-article-show entry)))

(defun feedsmith-list-open-browser ()
  "Open the entry at point in an external browser."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (when (feedsmith-entry-unread entry)
      (feedsmith-mark-read (feedsmith-entry-id entry))
      (feedsmith-list--update-entry-at-point))
    (when-let ((url (feedsmith-entry-url entry)))
      (browse-url url))))

(defun feedsmith-list--update-entry-at-point ()
  "Update the visual display of the entry at point without rebuilding the list."
  (when-let ((id (tabulated-list-get-id))
             (entry (gethash id feedsmith--entries)))
    (let* ((unread (feedsmith-entry-unread entry))
           (starred (feedsmith-entry-starred entry))
           (title (or (feedsmith-entry-title entry) "(no title)"))
           (feed (or (feedsmith-entry-feed-title entry) ""))
           (date (feedsmith-list--format-date (feedsmith-entry-published entry)))
           (title-face (if unread 'feedsmith-title-unread-face 'feedsmith-title-face))
           (inhibit-read-only t))
      (tabulated-list-set-col 0 (if unread
                                    (propertize "\u2022" 'face 'feedsmith-unread-face)
                                  " "))
      (tabulated-list-set-col 1 (if starred
                                    (propertize "\u2605" 'face 'feedsmith-starred-face)
                                  " "))
      (tabulated-list-set-col 2 (propertize date 'face 'feedsmith-date-face))
      (tabulated-list-set-col 3 (propertize feed 'face 'feedsmith-feed-face))
      (tabulated-list-set-col 4 (propertize title 'face title-face)))))

(defun feedsmith-list-toggle-read ()
  "Toggle read state of the entry at point."
  (interactive)
  (when-let ((entry (feedsmith-list--entry-at-point)))
    (feedsmith-toggle-read (feedsmith-entry-id entry))
    (feedsmith-list--update-entry-at-point)))

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

(defvar feedsmith-article--links nil
  "List of URLs in the current article, indexed by number.")

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
    (define-key map (kbd "f") #'feedsmith-article-follow-link)
    map)
  "Keymap for `feedsmith-article-mode'.")

(define-derived-mode feedsmith-article-mode special-mode "Feedsmith-Article"
  "Major mode for viewing a Feedsmith article."
  (setq-local feedsmith-article--current-entry nil)
  (setq-local feedsmith-article--links nil)
  (add-hook 'window-size-change-functions #'feedsmith--centering-hook nil t))

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
        (insert (propertize title 'face 'feedsmith-article-title-face) "\n")
        (when author (insert (propertize "Author: " 'face 'feedsmith-article-meta-face) author "\n"))
        (when feed (insert (propertize "Feed:   " 'face 'feedsmith-article-meta-face) feed "\n"))
        (when date (insert (propertize "Date:   " 'face 'feedsmith-article-meta-face)
                           (feedsmith-list--format-date date) "\n"))
        (when url (insert (propertize "URL:    " 'face 'feedsmith-article-meta-face) url "\n"))
        (insert "\n" (make-string 72 ?-) "\n\n"))
      ;; Body
      (let ((content (or (feedsmith-entry-content entry)
                         (feedsmith-entry-summary entry)
                         "<p>No content available.</p>")))
        (feedsmith-article--render-html content))
      (goto-char (point-min)))
    (when-let ((win (get-buffer-window buf)))
      (feedsmith--update-centering win))))

(defun feedsmith-article--render-html (html)
  "Render HTML string into the current buffer using shr."
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region (point-min) (point-max)))))
    (shr-insert-document dom))
  (feedsmith-article--number-links))

(defun feedsmith-article--number-links ()
  "Number all links in the current buffer and collect them.
Links are numbered starting from 1, and URLs are stored in
`feedsmith-article--links' for access via `feedsmith-article-open-link'."
  (setq feedsmith-article--links nil)
  (let ((link-num 0)
        (pos (point-min))
        links-alist)
    ;; Scan buffer for links (text with shr-url property)
    (while (< pos (point-max))
      (let ((url (get-text-property pos 'shr-url))
            (next-change (next-single-property-change pos 'shr-url nil (point-max))))
        (when (and url (not (assoc url links-alist)))
          ;; New unique link found
          (setq link-num (1+ link-num))
          (push (cons url link-num) links-alist)
          ;; Insert link number after the link text
          (save-excursion
            (goto-char next-change)
            (insert (propertize (format "[%d]" link-num)
                                'face 'feedsmith-link-number-face))))
        (setq pos next-change)))
    ;; Store links as a vector for O(1) access by number
    (setq feedsmith-article--links (make-vector (1+ link-num) nil))
    (dolist (pair links-alist)
      (aset feedsmith-article--links (cdr pair) (car pair)))))

;;;; Article commands

(defun feedsmith-article-open-browser ()
  "Open the current article in an external browser."
  (interactive)
  (when-let ((url (and feedsmith-article--current-entry
                       (feedsmith-entry-url feedsmith-article--current-entry))))
    (browse-url url)))

(defun feedsmith-article-follow-link (num)
  "Follow a numbered link in the article.
Prompts for link NUM and opens it in the default browser."
  (interactive "nLink number: ")
  (if (and feedsmith-article--links
           (> num 0)
           (< num (length feedsmith-article--links))
           (aref feedsmith-article--links num))
      (browse-url (aref feedsmith-article--links num))
    (message "No link #%d in this article" num)))

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
  "Navigate to the next or previous unread article.
DIRECTION is 1 for next, -1 for previous.
Returns to the feed list if no unread articles remain."
  (when feedsmith-article--current-entry
    (let* ((ids (feedsmith-article--sorted-entry-ids))
           (current-id (feedsmith-entry-id feedsmith-article--current-entry))
           (pos (cl-position current-id ids :test #'equal))
           (search-pos (when pos (+ pos direction)))
           (found nil))
      ;; Search for the next unread article in the given direction
      (while (and search-pos (>= search-pos 0) (< search-pos (length ids)) (not found))
        (let ((entry (gethash (nth search-pos ids) feedsmith--entries)))
          (if (and entry (feedsmith-entry-unread entry))
              (setq found entry)
            (setq search-pos (+ search-pos direction)))))
      (if found
          (progn
            (feedsmith-mark-read (feedsmith-entry-id found))
            (feedsmith-article-show found))
        ;; No unread articles remain, return to feed list
        (feedsmith-article-quit)))))

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

;;;; Responsive centering

(defun feedsmith--update-centering (window)
  "Update window margins on WINDOW to center feedsmith buffers.
When the frame is wider than `feedsmith-center-threshold' pixels
and WINDOW is displaying a feedsmith buffer, equal margins are
applied so the content is `feedsmith-center-width' columns wide."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (if (and (or (derived-mode-p 'feedsmith-list-mode)
                   (derived-mode-p 'feedsmith-article-mode))
               (> (frame-pixel-width (window-frame window))
                  feedsmith-center-threshold))
          (let ((margin (max 0 (/ (- (window-total-width window)
                                     feedsmith-center-width)
                                  2))))
            (set-window-margins window margin margin))
        (when (or (derived-mode-p 'feedsmith-list-mode)
                  (derived-mode-p 'feedsmith-article-mode))
          (set-window-margins window 0 0))))))

(defun feedsmith--centering-hook (frame)
  "Update centering for all windows in FRAME showing feedsmith buffers."
  (walk-windows
   (lambda (win)
     (feedsmith--update-centering win))
   'no-minibuf frame))

(defun feedsmith--on-focus-change ()
  "Update centering for feedsmith windows when frame focus changes.
This catches the case where a frame is moved between monitors,
which changes `frame-pixel-width' without triggering
`window-size-change-functions'."
  (dolist (frame (frame-list))
    (when (frame-focus-state frame)
      (feedsmith--centering-hook frame))))

(add-function :after after-focus-change-function #'feedsmith--on-focus-change)

(provide 'feedsmith-ui)
;;; feedsmith-ui.el ends here
