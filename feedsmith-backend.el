;;; feedsmith-backend.el --- Backend protocol for Feedsmith -*- lexical-binding: t; -*-

;; Author: Curtis McHale
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Defines the abstract backend protocol and shared data structures
;; for Feedsmith.  New backends implement the `feedsmith-backend' EIEIO
;; class and its generic methods.

;;; Code:

(require 'cl-lib)
(require 'eieio)

;;;; Data structures

(cl-defstruct feedsmith-entry
  "An RSS/Atom entry."
  id feed-id title author url content summary
  published unread starred feed-title)

(cl-defstruct feedsmith-subscription
  "A feed subscription."
  id feed-id title feed-url site-url)

;;;; Abstract backend class

(defclass feedsmith-backend ()
  ()
  :abstract t
  :documentation "Abstract base class for Feedsmith backends.")

;;;; Generic methods

(cl-defgeneric feedsmith-backend-authenticate (backend)
  "Verify credentials for BACKEND.  Return non-nil on success.")

(cl-defgeneric feedsmith-backend-fetch-subscriptions (backend)
  "Return a list of `feedsmith-subscription' structs from BACKEND.")

(cl-defgeneric feedsmith-backend-fetch-entries (backend &optional since)
  "Return a list of `feedsmith-entry' structs from BACKEND.
If SINCE is non-nil it is an ISO-8601 timestamp string; only return
entries created after that time.")

(cl-defgeneric feedsmith-backend-fetch-unread-ids (backend)
  "Return a list of unread entry ID integers from BACKEND.")

(cl-defgeneric feedsmith-backend-fetch-starred-ids (backend)
  "Return a list of starred entry ID integers from BACKEND.")

(cl-defgeneric feedsmith-backend-mark-read (backend ids)
  "Mark entry IDS (list of integers) as read on BACKEND.")

(cl-defgeneric feedsmith-backend-mark-unread (backend ids)
  "Mark entry IDS (list of integers) as unread on BACKEND.")

(cl-defgeneric feedsmith-backend-set-starred (backend id starred)
  "Set starred state of entry ID on BACKEND.
STARRED is a boolean.")

(provide 'feedsmith-backend)
;;; feedsmith-backend.el ends here
