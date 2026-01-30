;;; feedsmith.el --- An Emacs RSS reader with Feedbin sync -*- lexical-binding: t; -*-

;; Author: Curtis McHale
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: news, rss
;; URL: https://github.com/curtismchale/feedsmith

;;; Commentary:

;; Feedsmith is a purpose-built Emacs RSS reader with Feedbin sync,
;; a pluggable backend protocol, Evil keybindings, and Org refile
;; integration.
;;
;; Usage:
;;   M-x feedsmith
;;
;; See README.md for setup instructions.

;;; Code:

(require 'feedsmith-backend)

;;;; Customization

(defgroup feedsmith nil
  "An Emacs RSS reader."
  :group 'news
  :prefix "feedsmith-")

(defcustom feedsmith-backend nil
  "The backend instance used for syncing.
Should be an object inheriting from `feedsmith-backend'."
  :type 'sexp
  :group 'feedsmith)

(defcustom feedsmith-cache-directory
  (expand-file-name "feedsmith" user-emacs-directory)
  "Directory for Feedsmith cache files."
  :type 'directory
  :group 'feedsmith)

(defcustom feedsmith-max-entries 500
  "Maximum number of entries to keep in the cache."
  :type 'integer
  :group 'feedsmith)

(defcustom feedsmith-auto-sync-interval 300
  "Auto-sync interval in seconds.  Set to nil to disable."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'feedsmith)

;;;; Internal state

(defvar feedsmith--subscriptions nil
  "List of `feedsmith-subscription' structs.")

(defvar feedsmith--entries nil
  "Hash table mapping entry ID to `feedsmith-entry' struct.")

(defvar feedsmith--unread-ids nil
  "Hash table of unread entry IDs (ID -> t).")

(defvar feedsmith--starred-ids nil
  "Hash table of starred entry IDs (ID -> t).")

(defvar feedsmith--last-sync-time nil
  "ISO-8601 timestamp of last successful sync, or nil.")

(defvar feedsmith--sync-timer nil
  "Timer for auto-sync.")

;;;; Cache

(defun feedsmith--cache-file ()
  "Return the path to the cache file."
  (expand-file-name "feedsmith-cache.eld" feedsmith-cache-directory))

(defun feedsmith--entry-to-plist (entry)
  "Convert a `feedsmith-entry' ENTRY to a plist for serialization."
  (list :id (feedsmith-entry-id entry)
        :feed-id (feedsmith-entry-feed-id entry)
        :title (feedsmith-entry-title entry)
        :author (feedsmith-entry-author entry)
        :url (feedsmith-entry-url entry)
        :content (feedsmith-entry-content entry)
        :summary (feedsmith-entry-summary entry)
        :published (feedsmith-entry-published entry)
        :unread (feedsmith-entry-unread entry)
        :starred (feedsmith-entry-starred entry)
        :feed-title (feedsmith-entry-feed-title entry)))

(defun feedsmith--plist-to-entry (plist)
  "Convert a PLIST back to a `feedsmith-entry'."
  (make-feedsmith-entry
   :id (plist-get plist :id)
   :feed-id (plist-get plist :feed-id)
   :title (plist-get plist :title)
   :author (plist-get plist :author)
   :url (plist-get plist :url)
   :content (plist-get plist :content)
   :summary (plist-get plist :summary)
   :published (plist-get plist :published)
   :unread (plist-get plist :unread)
   :starred (plist-get plist :starred)
   :feed-title (plist-get plist :feed-title)))

(defun feedsmith--sub-to-plist (sub)
  "Convert a `feedsmith-subscription' SUB to a plist."
  (list :id (feedsmith-subscription-id sub)
        :feed-id (feedsmith-subscription-feed-id sub)
        :title (feedsmith-subscription-title sub)
        :feed-url (feedsmith-subscription-feed-url sub)
        :site-url (feedsmith-subscription-site-url sub)))

(defun feedsmith--plist-to-sub (plist)
  "Convert a PLIST back to a `feedsmith-subscription'."
  (make-feedsmith-subscription
   :id (plist-get plist :id)
   :feed-id (plist-get plist :feed-id)
   :title (plist-get plist :title)
   :feed-url (plist-get plist :feed-url)
   :site-url (plist-get plist :site-url)))

(defun feedsmith--save-cache ()
  "Serialize state to the cache file."
  (let ((dir feedsmith-cache-directory))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((entries-list nil))
      (when feedsmith--entries
        (maphash (lambda (_k v) (push (feedsmith--entry-to-plist v) entries-list))
                 feedsmith--entries))
      (let ((unread-list nil)
            (starred-list nil))
        (when feedsmith--unread-ids
          (maphash (lambda (k _v) (push k unread-list)) feedsmith--unread-ids))
        (when feedsmith--starred-ids
          (maphash (lambda (k _v) (push k starred-list)) feedsmith--starred-ids))
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file (feedsmith--cache-file)
            (let ((print-level nil)
                  (print-length nil)
                  (print-escape-nonascii nil)
                  (print-escape-multibyte nil))
              (prin1 (list :subscriptions (mapcar #'feedsmith--sub-to-plist feedsmith--subscriptions)
                           :entries entries-list
                           :unread-ids unread-list
                           :starred-ids starred-list
                           :last-sync-time feedsmith--last-sync-time)
                     (current-buffer)))))))))

(defun feedsmith--load-cache ()
  "Load state from the cache file."
  (let ((file (feedsmith--cache-file)))
    (when (file-exists-p file)
      (let ((data (let ((coding-system-for-read 'utf-8))
                    (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer))))))
        (setq feedsmith--subscriptions
              (mapcar #'feedsmith--plist-to-sub (plist-get data :subscriptions)))
        (setq feedsmith--entries (make-hash-table :test 'equal))
        (dolist (plist (plist-get data :entries))
          (let ((entry (feedsmith--plist-to-entry plist)))
            (puthash (feedsmith-entry-id entry) entry feedsmith--entries)))
        (setq feedsmith--unread-ids (make-hash-table :test 'equal))
        (dolist (id (plist-get data :unread-ids))
          (puthash id t feedsmith--unread-ids))
        (setq feedsmith--starred-ids (make-hash-table :test 'equal))
        (dolist (id (plist-get data :starred-ids))
          (puthash id t feedsmith--starred-ids))
        (setq feedsmith--last-sync-time (plist-get data :last-sync-time))))))

;;;; Sync

(defun feedsmith--subscription-title-for-feed-id (feed-id)
  "Return the subscription title for FEED-ID, or nil."
  (cl-loop for sub in feedsmith--subscriptions
           when (equal (feedsmith-subscription-feed-id sub) feed-id)
           return (feedsmith-subscription-title sub)))

(defun feedsmith--denormalize-entries ()
  "Set `feed-title' on all entries from subscriptions."
  (when feedsmith--entries
    (maphash (lambda (_id entry)
               (unless (feedsmith-entry-feed-title entry)
                 (let ((title (feedsmith--subscription-title-for-feed-id
                               (feedsmith-entry-feed-id entry))))
                   (when title
                     (setf (feedsmith-entry-feed-title entry) title)))))
             feedsmith--entries)))

(defun feedsmith--update-entry-flags ()
  "Update unread/starred flags on cached entries from ID sets."
  (when feedsmith--entries
    (maphash (lambda (id entry)
               (setf (feedsmith-entry-unread entry)
                     (and feedsmith--unread-ids
                          (gethash id feedsmith--unread-ids nil)))
               (setf (feedsmith-entry-starred entry)
                     (and feedsmith--starred-ids
                          (gethash id feedsmith--starred-ids nil))))
             feedsmith--entries)))

(defun feedsmith--trim-entries ()
  "Remove oldest entries beyond `feedsmith-max-entries'."
  (when (and feedsmith--entries
             (> (hash-table-count feedsmith--entries) feedsmith-max-entries))
    (let ((entries nil))
      (maphash (lambda (_k v) (push v entries)) feedsmith--entries)
      (setq entries (sort entries
                          (lambda (a b)
                            (string> (or (feedsmith-entry-published a) "")
                                     (or (feedsmith-entry-published b) "")))))
      (let ((keep (seq-take entries feedsmith-max-entries))
            (new-table (make-hash-table :test 'equal)))
        (dolist (e keep)
          (puthash (feedsmith-entry-id e) e new-table))
        (setq feedsmith--entries new-table)))))

(defun feedsmith-sync ()
  "Synchronize with the backend."
  (interactive)
  (unless feedsmith-backend
    (user-error "No backend configured.  Set `feedsmith-backend'"))
  (message "Feedsmith: syncing...")
  (condition-case err
      (progn
        ;; Subscriptions
        (message "Feedsmith: fetching subscriptions...")
        (setq feedsmith--subscriptions
              (feedsmith-backend-fetch-subscriptions feedsmith-backend))
        ;; Unread/starred IDs
        (message "Feedsmith: fetching unread IDs...")
        (let ((unread (feedsmith-backend-fetch-unread-ids feedsmith-backend)))
          (setq feedsmith--unread-ids (make-hash-table :test 'equal))
          (dolist (id unread)
            (puthash id t feedsmith--unread-ids)))
        (message "Feedsmith: fetching starred IDs...")
        (let ((starred (feedsmith-backend-fetch-starred-ids feedsmith-backend)))
          (setq feedsmith--starred-ids (make-hash-table :test 'equal))
          (dolist (id starred)
            (puthash id t feedsmith--starred-ids)))
        ;; Entries
        (message "Feedsmith: fetching entries...")
        (let ((new-entries (feedsmith-backend-fetch-entries
                            feedsmith-backend feedsmith--last-sync-time)))
          (unless feedsmith--entries
            (setq feedsmith--entries (make-hash-table :test 'equal)))
          (dolist (entry new-entries)
            (puthash (feedsmith-entry-id entry) entry feedsmith--entries)))
        ;; Post-process
        (feedsmith--denormalize-entries)
        (feedsmith--update-entry-flags)
        (feedsmith--trim-entries)
        (setq feedsmith--last-sync-time
              (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time)))
        (feedsmith--save-cache)
        (message "Feedsmith: sync complete (%d entries, %d unread)"
                 (hash-table-count feedsmith--entries)
                 (hash-table-count feedsmith--unread-ids)))
    (error
     (message "Feedsmith: sync failed: %s" (error-message-string err)))))

;;;; Mark/star commands

(defun feedsmith-mark-read (id)
  "Mark entry ID as read locally and on the backend."
  (when feedsmith--unread-ids
    (remhash id feedsmith--unread-ids))
  (when-let ((entry (and feedsmith--entries (gethash id feedsmith--entries))))
    (setf (feedsmith-entry-unread entry) nil))
  (when feedsmith-backend
    (condition-case err
        (feedsmith-backend-mark-read feedsmith-backend (list id))
      (error (message "Feedsmith: failed to mark read: %s"
                      (error-message-string err))))))

(defun feedsmith-mark-unread (id)
  "Mark entry ID as unread locally and on the backend."
  (unless feedsmith--unread-ids
    (setq feedsmith--unread-ids (make-hash-table :test 'equal)))
  (puthash id t feedsmith--unread-ids)
  (when-let ((entry (and feedsmith--entries (gethash id feedsmith--entries))))
    (setf (feedsmith-entry-unread entry) t))
  (when feedsmith-backend
    (condition-case err
        (feedsmith-backend-mark-unread feedsmith-backend (list id))
      (error (message "Feedsmith: failed to mark unread: %s"
                      (error-message-string err))))))

(defun feedsmith-toggle-read (id)
  "Toggle read state of entry ID."
  (if (and feedsmith--unread-ids (gethash id feedsmith--unread-ids))
      (feedsmith-mark-read id)
    (feedsmith-mark-unread id)))

(defun feedsmith-toggle-starred (id)
  "Toggle starred state of entry ID."
  (unless feedsmith--starred-ids
    (setq feedsmith--starred-ids (make-hash-table :test 'equal)))
  (let ((currently-starred (gethash id feedsmith--starred-ids)))
    (if currently-starred
        (remhash id feedsmith--starred-ids)
      (puthash id t feedsmith--starred-ids))
    (when-let ((entry (and feedsmith--entries (gethash id feedsmith--entries))))
      (setf (feedsmith-entry-starred entry) (not currently-starred)))
    (when feedsmith-backend
      (condition-case err
          (feedsmith-backend-set-starred feedsmith-backend id (not currently-starred))
        (error (message "Feedsmith: failed to toggle starred: %s"
                        (error-message-string err)))))))

;;;; Entry point

(defun feedsmith--init-state ()
  "Initialize internal state if needed."
  (unless feedsmith--entries
    (setq feedsmith--entries (make-hash-table :test 'equal)))
  (unless feedsmith--unread-ids
    (setq feedsmith--unread-ids (make-hash-table :test 'equal)))
  (unless feedsmith--starred-ids
    (setq feedsmith--starred-ids (make-hash-table :test 'equal))))

(defun feedsmith--start-auto-sync ()
  "Start the auto-sync timer if configured."
  (feedsmith--stop-auto-sync)
  (when feedsmith-auto-sync-interval
    (setq feedsmith--sync-timer
          (run-with-timer feedsmith-auto-sync-interval
                          feedsmith-auto-sync-interval
                          (lambda ()
                            (when (get-buffer "*feedsmith*")
                              (feedsmith-sync)
                              (when-let ((win (get-buffer-window "*feedsmith*")))
                                (with-selected-window win
                                  (when (derived-mode-p 'feedsmith-list-mode)
                                    (feedsmith-list-refresh))))))))))

(defun feedsmith--stop-auto-sync ()
  "Stop the auto-sync timer."
  (when feedsmith--sync-timer
    (cancel-timer feedsmith--sync-timer)
    (setq feedsmith--sync-timer nil)))

;;;###autoload
(defun feedsmith ()
  "Start Feedsmith RSS reader."
  (interactive)
  (feedsmith--load-cache)
  (feedsmith--init-state)
  (require 'feedsmith-ui)
  (feedsmith-list-show)
  (feedsmith-sync)
  (feedsmith-list-refresh)
  (feedsmith--start-auto-sync))

(provide 'feedsmith)
;;; feedsmith.el ends here
