# Feedsmith

An Emacs RSS reader with Feedbin sync, pluggable backends, Evil keybindings, and Org refile integration.

## Requirements

- Emacs 28.1+ (built-in json, eieio, libxml)
- `evil` (for Evil keybindings, optional)
- `org` (for Org refile, optional)

## Installation

### Doom Emacs

In `packages.el`:

```elisp
(package! feedsmith :recipe (:local-repo "~/Projects/feedsmith"))
```

In `config.el`:

```elisp
(use-package! feedsmith
  :commands (feedsmith)
  :config
  (setq feedsmith-backend (feedsmith-feedbin-create))
  (require 'feedsmith-evil))
```

Then reload: `SPC h r r` inside Emacs, followed by `doom sync` in a terminal. Then quit and restart Emacs for the command to show up.

### straight.el / use-package

```elisp
(use-package feedsmith
  :straight (:local-repo "~/Projects/feedsmith")
  :commands (feedsmith)
  :config
  (setq feedsmith-backend (feedsmith-feedbin-create))
  (require 'feedsmith-evil))
```

## Credential Setup

Feedsmith reads Feedbin credentials from `auth-source` (typically `~/.authinfo.gpg`).

### GPG Key Setup

If you don't already have a GPG key, you'll need one to encrypt your credentials file. Check if you have an existing key:

```bash
gpg --list-keys
```

If no keys are listed, generate one:

```bash
gpg --full-generate-key
```

Accept the defaults (RSA and RSA, 3072 bits), enter your name and email, and set a passphrase. For a more detailed walkthrough, see the [GitHub GPG guide](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key).

### Creating the Credentials File

The safest way to create the encrypted credentials file is from within Emacs, which handles encryption automatically:

```
C-x C-f ~/.authinfo.gpg RET
```

Type your credentials:

```
machine api.feedbin.com login YOUR_EMAIL password YOUR_PASSWORD
```

Save with `C-x C-s`. Emacs will prompt you to select your GPG key for encryption.

You can verify the file is properly encrypted:

```bash
file ~/.authinfo.gpg
```

This should report something like `PGP RSA encrypted session key` rather than plain ASCII text.

## Usage

```
M-x feedsmith
```

This opens the feed list, loads cached entries, syncs with Feedbin, and starts auto-sync.

## Keybindings

### Feed List

| Key     | Action                     |
|---------|----------------------------|
| `RET/o` | Open article               |
| `O`     | Open in browser            |
| `r`     | Toggle read/unread         |
| `s`     | Toggle starred             |
| `R`     | Refile to Org              |
| `gr`    | Sync with backend          |
| `j/k`   | Navigate up/down           |
| `gg/G`  | Go to top/bottom           |
| `/`     | Search/filter entries      |
| `u`     | Toggle unread-only filter  |
| `q`     | Quit                       |

### Article View

| Key       | Action                   |
|-----------|--------------------------|
| `O`       | Open in browser          |
| `R`       | Refile to Org            |
| `r`       | Toggle read/unread       |
| `s`       | Toggle starred           |
| `n/p`     | Next/previous article    |
| `q`       | Back to feed list        |
| `SPC`     | Scroll down              |
| `S-SPC`   | Scroll up                |

## Customization

```elisp
;; Cache directory (default: ~/.emacs.d/feedsmith/)
(setq feedsmith-cache-directory "~/.emacs.d/feedsmith/")

;; Max cached entries (default: 500)
(setq feedsmith-max-entries 500)

;; Auto-sync interval in seconds (default: 300, nil to disable)
(setq feedsmith-auto-sync-interval 300)

;; Org refile summary paragraphs (default: 3)
(setq feedsmith-org-refile-summary-paragraphs 3)
```

## Adding a Backend

Feedsmith uses a pluggable backend protocol via EIEIO. To add a new backend:

1. Create a new file (e.g. `feedsmith-mybackend.el`)
2. Define an EIEIO class inheriting from `feedsmith-backend`
3. Implement all 8 generic methods:

```elisp
(require 'feedsmith-backend)

(defclass feedsmith-mybackend (feedsmith-backend)
  ((api-url :initarg :api-url :type string))
  :documentation "My custom backend.")

(cl-defmethod feedsmith-backend-authenticate ((backend feedsmith-mybackend))
  ;; Verify credentials, return non-nil on success
  )

(cl-defmethod feedsmith-backend-fetch-subscriptions ((backend feedsmith-mybackend))
  ;; Return list of feedsmith-subscription structs
  )

(cl-defmethod feedsmith-backend-fetch-entries ((backend feedsmith-mybackend) &optional since)
  ;; Return list of feedsmith-entry structs
  )

(cl-defmethod feedsmith-backend-fetch-unread-ids ((backend feedsmith-mybackend))
  ;; Return list of unread entry ID integers
  )

(cl-defmethod feedsmith-backend-fetch-starred-ids ((backend feedsmith-mybackend))
  ;; Return list of starred entry ID integers
  )

(cl-defmethod feedsmith-backend-mark-read ((backend feedsmith-mybackend) ids)
  ;; Mark IDs as read
  )

(cl-defmethod feedsmith-backend-mark-unread ((backend feedsmith-mybackend) ids)
  ;; Mark IDs as unread
  )

(cl-defmethod feedsmith-backend-set-starred ((backend feedsmith-mybackend) id starred)
  ;; Set starred state
  )
```

4. Set your backend: `(setq feedsmith-backend (feedsmith-mybackend :api-url "https://..."))`

## Roadmap

- [ ] support other sync services (create an issue)
- [ ] show read items
- [x] support your emacs theme for colouring the text in feed views
