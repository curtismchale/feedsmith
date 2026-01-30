;;; feedsmith-evil.el --- Evil keybindings for Feedsmith -*- lexical-binding: t; -*-

;; Author: Curtis McHale
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Evil keybindings for Feedsmith feed list and article view modes.
;; Only loaded when evil is available.

;;; Code:

(require 'feedsmith-ui)

(when (require 'evil nil t)

  ;; Both modes use normal state
  (evil-set-initial-state 'feedsmith-list-mode 'normal)
  (evil-set-initial-state 'feedsmith-article-mode 'normal)

  ;; Feed list bindings
  (evil-define-key 'normal feedsmith-list-mode-map
    (kbd "RET") #'feedsmith-list-open
    "o"  #'feedsmith-list-open
    "O"  #'feedsmith-list-open-browser
    "r"  #'feedsmith-list-toggle-read
    "s"  #'feedsmith-list-toggle-starred
    "R"  #'feedsmith-list-org-refile
    "gr" #'feedsmith-list-sync
    "j"  #'next-line
    "k"  #'previous-line
    "gg" #'beginning-of-buffer
    "G"  #'end-of-buffer
    "/"  #'feedsmith-list-search
    "u"  #'feedsmith-list-toggle-unread-filter
    "q"  #'feedsmith-list-quit)

  ;; Article view bindings
  (evil-define-key 'normal feedsmith-article-mode-map
    "O"       #'feedsmith-article-open-browser
    "R"       #'feedsmith-article-org-refile
    "r"       #'feedsmith-article-toggle-read
    "s"       #'feedsmith-article-toggle-starred
    "n"       #'feedsmith-article-next
    "p"       #'feedsmith-article-prev
    "q"       #'feedsmith-article-quit
    (kbd "SPC")   #'scroll-up-command
    (kbd "S-SPC") #'scroll-down-command))

(provide 'feedsmith-evil)
;;; feedsmith-evil.el ends here
