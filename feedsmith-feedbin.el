;;; feedsmith-feedbin.el --- Feedbin backend for Feedsmith -*- lexical-binding: t; -*-

;; Author: Curtis McHale
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Feedbin REST API (v2) backend for Feedsmith.
;; Credentials are read from auth-source (~/.authinfo.gpg).

;;; Code:

(require 'feedsmith-backend)
(require 'url)
(require 'url-http)
(require 'json)
(require 'auth-source)

;;;; Class

(defclass feedsmith-feedbin (feedsmith-backend)
  ((api-url :initarg :api-url
            :initform "https://api.feedbin.com/v2"
            :type string
            :documentation "Base URL for the Feedbin API.")
   (username :initarg :username
             :type string
             :documentation "Feedbin account email.")
   (password :initarg :password
             :type string
             :documentation "Feedbin account password."))
  :documentation "Feedbin v2 API backend.")

;;;; Constructor

;;;###autoload
(defun feedsmith-feedbin-create ()
  "Create a `feedsmith-feedbin' backend using auth-source credentials.
Looks for an entry matching machine=api.feedbin.com in ~/.authinfo.gpg."
  (let ((found (car (auth-source-search :host "api.feedbin.com" :max 1))))
    (unless found
      (user-error "No credentials found for api.feedbin.com in auth-source"))
    (let ((user (plist-get found :user))
          (secret (plist-get found :secret)))
      (unless (and user secret)
        (user-error "Incomplete credentials for api.feedbin.com"))
      (feedsmith-feedbin
       :username user
       :password (if (functionp secret) (funcall secret) secret)))))

;;;; HTTP helper

(defun feedsmith-feedbin--auth-header (backend)
  "Return the Basic Auth header value for BACKEND."
  (concat "Basic "
          (base64-encode-string
           (concat (oref backend username) ":" (oref backend password))
           t)))

(defun feedsmith-feedbin--request (backend method endpoint &optional body)
  "Make an HTTP request to BACKEND.
METHOD is \"GET\", \"POST\", \"DELETE\", etc.
ENDPOINT is appended to the api-url (e.g. \"/subscriptions.json\").
BODY, if non-nil, is an alist that will be JSON-encoded.

Returns a cons cell (STATUS-CODE . PARSED-JSON).
For 204 responses, the cdr is nil."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(feedsmith-feedbin--auth-header backend))
            ("Content-Type" . "application/json; charset=utf-8")
            ("Accept" . "application/json")))
         (url-request-data
          (when body (encode-coding-string (json-encode body) 'utf-8)))
         (url (concat (oref backend api-url) endpoint))
         (buffer (url-retrieve-synchronously url t nil 30)))
    (unless buffer
      (error "Feedsmith-feedbin: no response from %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          ;; Parse status code
          (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (error "Feedsmith-feedbin: malformed HTTP response"))
          (let ((status (string-to-number (match-string 1))))
            (when (>= status 400)
              (error "Feedsmith-feedbin: HTTP %d from %s %s" status method endpoint))
            ;; Move past headers
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil t)
            (let ((json-data
                   (if (= status 204)
                       nil
                     (let ((json-array-type 'list)
                           (json-object-type 'alist)
                           (json-key-type 'symbol))
                       (condition-case nil
                           (json-read)
                         (error nil))))))
              (cons status json-data))))
      (kill-buffer buffer))))

(defun feedsmith-feedbin--get-link-next (buffer)
  "Extract the next page URL from Link headers in BUFFER, or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^Link:.*<\\([^>]+\\)>; rel=\"next\"" nil t)
      (match-string 1))))

(defun feedsmith-feedbin--paginated-get (backend endpoint)
  "GET all pages from ENDPOINT on BACKEND, returning combined JSON list."
  (let ((all-results nil)
        (url (concat (oref backend api-url) endpoint))
        (url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(feedsmith-feedbin--auth-header backend))
           ("Accept" . "application/json"))))
    (while url
      (let ((buffer (url-retrieve-synchronously url t nil 30)))
        (unless buffer
          (error "Feedsmith-feedbin: no response from %s" url))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (let ((next-url (feedsmith-feedbin--get-link-next buffer)))
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n" nil t)
                (let ((json-array-type 'list)
                      (json-object-type 'alist)
                      (json-key-type 'symbol))
                  (condition-case nil
                      (let ((page (json-read)))
                        (when (listp page)
                          (setq all-results (nconc all-results page))))
                    (error nil)))
                (setq url next-url)))
          (kill-buffer buffer))))
    all-results))

;;;; Method implementations

(cl-defmethod feedsmith-backend-authenticate ((backend feedsmith-feedbin))
  "Verify Feedbin credentials for BACKEND."
  (let ((result (feedsmith-feedbin--request backend "GET" "/authentication.json")))
    (= (car result) 200)))

(cl-defmethod feedsmith-backend-fetch-subscriptions ((backend feedsmith-feedbin))
  "Fetch subscriptions from Feedbin."
  (let ((data (feedsmith-feedbin--paginated-get backend "/subscriptions.json")))
    (mapcar (lambda (item)
              (make-feedsmith-subscription
               :id (alist-get 'id item)
               :feed-id (alist-get 'feed_id item)
               :title (alist-get 'title item)
               :feed-url (alist-get 'feed_url item)
               :site-url (alist-get 'site_url item)))
            data)))

(cl-defmethod feedsmith-backend-fetch-entries ((backend feedsmith-feedbin) &optional since)
  "Fetch entries from Feedbin, optionally SINCE a timestamp."
  (let ((endpoint (if since
                      (format "/entries.json?since=%s&per_page=100"
                              (url-hexify-string since))
                    "/entries.json?per_page=100")))
    (let ((data (feedsmith-feedbin--paginated-get backend endpoint)))
      (mapcar #'feedsmith-feedbin--parse-entry data))))

(defun feedsmith-feedbin--parse-entry (item)
  "Parse a Feedbin entry JSON ITEM into a `feedsmith-entry'."
  (make-feedsmith-entry
   :id (alist-get 'id item)
   :feed-id (alist-get 'feed_id item)
   :title (alist-get 'title item)
   :author (alist-get 'author item)
   :url (alist-get 'url item)
   :content (alist-get 'content item)
   :summary (alist-get 'summary item)
   :published (alist-get 'published item)
   :unread nil
   :starred nil
   :feed-title nil))

(cl-defmethod feedsmith-backend-fetch-unread-ids ((backend feedsmith-feedbin))
  "Fetch all unread entry IDs from Feedbin."
  (let ((result (feedsmith-feedbin--request backend "GET" "/unread_entries.json")))
    (cdr result)))

(cl-defmethod feedsmith-backend-fetch-starred-ids ((backend feedsmith-feedbin))
  "Fetch all starred entry IDs from Feedbin."
  (let ((result (feedsmith-feedbin--request backend "GET" "/starred_entries.json")))
    (cdr result)))

(cl-defmethod feedsmith-backend-mark-read ((backend feedsmith-feedbin) ids)
  "Mark IDS as read on Feedbin."
  (when ids
    ;; Feedbin accepts max 1000 IDs per request
    (let ((chunks (feedsmith-feedbin--chunk ids 1000)))
      (dolist (chunk chunks)
        (feedsmith-feedbin--request
         backend "DELETE" "/unread_entries.json"
         `((unread_entries . ,chunk)))))))

(cl-defmethod feedsmith-backend-mark-unread ((backend feedsmith-feedbin) ids)
  "Mark IDS as unread on Feedbin."
  (when ids
    (let ((chunks (feedsmith-feedbin--chunk ids 1000)))
      (dolist (chunk chunks)
        (feedsmith-feedbin--request
         backend "POST" "/unread_entries.json"
         `((unread_entries . ,chunk)))))))

(cl-defmethod feedsmith-backend-set-starred ((backend feedsmith-feedbin) id starred)
  "Set STARRED state for entry ID on Feedbin."
  (if starred
      (feedsmith-feedbin--request
       backend "POST" "/starred_entries.json"
       `((starred_entries . (,id))))
    (feedsmith-feedbin--request
     backend "DELETE" "/starred_entries.json"
     `((starred_entries . (,id))))))

;;;; Utilities

(defun feedsmith-feedbin--chunk (list size)
  "Split LIST into sublists of at most SIZE elements."
  (let ((result nil))
    (while list
      (push (seq-take list size) result)
      (setq list (seq-drop list size)))
    (nreverse result)))

(provide 'feedsmith-feedbin)
;;; feedsmith-feedbin.el ends here
