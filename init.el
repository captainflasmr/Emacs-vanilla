;; -*- lexical-binding: t; -*-

;; -*- lexical-binding: t; -*-
;;
;; -> requires-core
;;
(require 'org)
(require 'grep)
(require 'bookmark)
(require 'dired)
(require 'ox-md)
(require 'color)

;;
;; -> custom-settings-redirect
;;
(setq custom-file (make-temp-file "emacs-custom-"))

;;
;; -> completion-core
;;
(setq-default abbrev-mode t)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs try-expand-dabbrev
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially try-complete-lisp-symbol))

(setq completion-category-overrides
      '((project-file (styles substring basic partial-completion))
        (file (styles substring basic partial-completion))
        (buffer (styles substring basic partial-completion))
        (command (styles substring basic partial-completion))))

;;
;; -> modeline-completion-core
;;
(defun my-fido-completion-styles-advice (&rest _args)
  "Override completion styles after fido setup."
  (when (and fido-mode (icomplete-simple-completing-p))
    (setq-local completion-styles '(substring basic partial-completion))))

(advice-add 'icomplete--fido-mode-setup :after #'my-fido-completion-styles-advice)

(if (fboundp 'fido-vertical-mode)
    (fido-vertical-mode 1)
  (fido-mode 1))

;;
;; -> modeline-completion-core
;;
(custom-set-faces
 '(icomplete-first-match
   ((t (:foreground "#7c7c75" :background "#3a3a3a" :weight bold))))
 '(icomplete-selected-match
   ((t (:foreground "#ffffff" :background "#5f87af" :weight bold))))
 '(completions-common-part
   ((t (:foreground "#87ceeb"))))
 '(completions-first-difference
   ((t (:foreground "#ffb6c1")))))
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)

;;
;; -> global-overrides
;;
(defvar my-overrides-mode-map (make-sparse-keymap)
  "Keymap for the `my-overrides-mode'.")

(define-minor-mode my-overrides-mode
  "Activate the `my-overrides-mode-map'."
  :global t
  :init-value nil
  :keymap my-overrides-mode-map)

(my-overrides-mode 1)

;;
;; -> keys-navigation-core
;;
(defvar my-jump-keymap (make-sparse-keymap))
(global-set-key (kbd "M-l") my-jump-keymap)
(define-key my-jump-keymap (kbd "=") #'tab-bar-new-tab)
(define-key my-jump-keymap (kbd "b") (lambda () (interactive) (find-file "~/bin")))
(define-key my-jump-keymap (kbd "d")
            (lambda () (interactive)
              (find-file (if (eq system-type 'windows-nt)
                             (if (getenv "USERPROFILE")
                                 (concat (getenv "USERPROFILE") "\\Downloads")
                               "~/Downloads")
                           "~/Downloads"))))
(define-key my-jump-keymap (kbd "e")
            (lambda ()
              (interactive)
              (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key my-jump-keymap (kbd "g")
            (lambda () (interactive)
              (find-file (if (eq system-type 'windows-nt)
                             (or (getenv "APPDATA") "~/AppData/Roaming")
                           "~/.config"))))
(define-key my-jump-keymap (kbd "h")
            (lambda () (interactive)
              (find-file (if (eq system-type 'windows-nt)
                             (or (getenv "USERPROFILE") "~")
                           "~"))))
(define-key my-jump-keymap (kbd "i") (lambda () (interactive) (find-file "~/.emacs.d/Emacs-vanilla")))
(define-key my-jump-keymap (kbd "l") #'my/fido-recentf)
(define-key my-jump-keymap (kbd "o") #'bookmark-jump)
(define-key my-jump-keymap (kbd "r") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(define-key my-jump-keymap (kbd "s") (lambda () (interactive) (find-file "~/source")))
(define-key my-jump-keymap (kbd "y") (lambda () (interactive) (find-file "~/.emacs.d/Emacs-DIYer")))
(define-key my-jump-keymap (kbd "-") #'tab-close)

;; 1. First, rescue 'kill-region' by binding it to something else
(global-set-key (kbd "C-x C-k") 'kill-region)

;; 2. Clear out C-w to act as a prefix key instead 
(define-prefix-command 'my-window-map)

;; 3. Bind your hjkl keys (vim C-w compatibility)
(define-key my-window-map (kbd "h") 'windmove-left)
(define-key my-window-map (kbd "j") 'windmove-down)
(define-key my-window-map (kbd "k") 'windmove-up)
(define-key my-window-map (kbd "l") 'windmove-right)

;; 4. Splits
(define-key my-window-map (kbd "s") 'split-window-below)
(define-key my-window-map (kbd "v") 'split-window-right)

;; 5. Close / other
(define-key my-window-map (kbd "q") 'delete-window)
(define-key my-window-map (kbd "c") 'delete-window)
(define-key my-window-map (kbd "o") 'delete-other-windows)
(define-key my-overrides-mode-map (kbd "C-c c") 'delete-other-windows)

;; 6. Cycle
(define-key my-window-map (kbd "w") 'other-window)

;; 7. Resize
(define-key my-window-map (kbd "=") 'balance-windows)
(define-key my-window-map (kbd "+") 'enlarge-window)
(define-key my-window-map (kbd "-") 'shrink-window)
(define-key my-window-map (kbd ">") 'enlarge-window-horizontally)
(define-key my-window-map (kbd "<") 'shrink-window-horizontally)

;; 8. Move window position
(define-key my-window-map (kbd "H") 'windmove-swap-states-left)
(define-key my-window-map (kbd "J") 'windmove-swap-states-down)
(define-key my-window-map (kbd "K") 'windmove-swap-states-up)
(define-key my-window-map (kbd "L") 'windmove-swap-states-right)

;;
;; -> keys-visual-core
;;
(add-hook 'text-mode-hook 'visual-line-mode)

;;
;; -> eldoc-box-core
;;
(defvar my/eldoc-box--child-frame nil
  "Holds the current eldoc child frame, if any.")

(defun my/eldoc-box--make-frame ()
  "Show eldoc documentation in a child frame near point."
  (interactive)
  (when (frame-live-p my/eldoc-box--child-frame)
    (delete-frame my/eldoc-box--child-frame))
  (let* ((parent (selected-frame))
         (origin-major-mode major-mode)
         (buffer (eldoc-doc-buffer))
         (line-count (with-current-buffer buffer
                       (count-lines (point-min) (point-max))))
         (desired-lines (min 20 (max 0 line-count)))
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (internal-border-width . 1)
                   (undecorated . t)
                   (fullscreen . nil)
                   (left . ,(+ (window-pixel-left)
                               (car (posn-x-y (posn-at-point)))))
                   (top . ,(+ (cdr (posn-x-y (posn-at-point)))
                              (frame-char-height)))
                   (width . 60)
                   (height . ,desired-lines)
                   (minibuffer . nil)
                   (visibility . nil)
                   (desktop-dont-save . t)
                   (right-fringe . 0)
                   (left-fringe . 0)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (line-spacing . 0)
                   (unsplittable . t)
                   (cursor-type . nil)
                   (mouse-wheel-frame . nil)
                   (no-other-frame . t)
                   (inhibit-double-buffering . t)
                   (drag-internal-border . t)
                   (no-special-glyphs . t)
                   (name . "my-eldoc-box")))))

    (with-current-buffer buffer
      (when (memq origin-major-mode
                  '(typescript-ts-mode tsx-ts-mode js-ts-mode))
        (ignore-errors (markdown-ts-mode) (font-lock-ensure)))
      (when (memq origin-major-mode '(go-ts-mode))
        (ignore-errors (go-ts-mode) (font-lock-ensure)))
      (when (memq origin-major-mode '(rust-ts-mode))
        (ignore-errors (rust-ts-mode) (font-lock-ensure)))
      (ignore-errors (flymake-mode -1))
      (visual-line-mode 1)
      (display-line-numbers-mode -1))

    (walk-windows
     (lambda (win)
       (when (eq (window-frame win) frame)
         (set-window-parameter win 'mode-line-format 'none)
         (set-window-parameter win 'header-line-format 'none))
       nil frame))

    (set-window-buffer (frame-root-window frame) buffer)
    (set-frame-parameter frame 'visibility t)

    (let* ((bg (face-background 'default nil parent))
           (rgb (color-name-to-rgb bg))
           (darker (apply #'color-rgb-to-hex
                          (mapcar (lambda (c) (* 0.9 c)) rgb))))
      (set-frame-parameter frame 'background-color darker)
      (with-current-buffer buffer
        (face-remap-add-relative 'default `(:background ,darker))))

    (setq my/eldoc-box--child-frame frame)
    (my/eldoc-box--enable-auto-close)

    (let ((key (read-key "Eldoc Box: q(uit) / o(pen) doc in window")))
      (cond
       ((equal key ?q)
        (my/eldoc-box--delete-frame))
       ((equal key ?o)
        (my/eldoc-box--delete-frame)
        (run-with-idle-timer 0.05 nil
                             (lambda () (eldoc-doc-buffer t))))
       (t
        (my/eldoc-box--delete-frame))))

    frame))

(defun my/eldoc-box--delete-frame ()
  "Close the eldoc child frame."
  (interactive)
  (when (frame-live-p my/eldoc-box--child-frame)
    (delete-frame my/eldoc-box--child-frame)
    (setq my/eldoc-box--child-frame nil))
  (remove-hook 'post-command-hook #'my/eldoc-box--maybe-close-frame))

(defvar my/eldoc-box--last-point nil
  "Stores the last known position of point to detect movement.")

(defun my/eldoc-box--maybe-close-frame ()
  "Close the eldoc child frame if point has moved."
  (when (and my/eldoc-box--child-frame
             (frame-live-p my/eldoc-box--child-frame)
             (not (equal my/eldoc-box--last-point (point))))
    (my/eldoc-box--delete-frame)))

(defun my/eldoc-box--enable-auto-close ()
  "Enable automatic closing of eldoc box when point moves."
  (setq my/eldoc-box--last-point (point))
  (add-hook 'post-command-hook #'my/eldoc-box--maybe-close-frame))

(global-set-key (kbd "C-c h") #'my/eldoc-box--make-frame)

;;
;; -> keys-visual-core
;;
(defvar my-win-keymap (make-sparse-keymap))
 (define-key my-overrides-mode-map (kbd "C-z") my-win-keymap)
(define-key my-win-keymap (kbd "b") #'(lambda () (interactive)(tab-bar-mode 'toggle)))
(define-key my-win-keymap (kbd "c") #'display-fill-column-indicator-mode)
(define-key my-win-keymap (kbd "d") #'window-divider-mode)
(define-key my-win-keymap (kbd "e") #'whitespace-mode)
(define-key my-win-keymap (kbd "f") #'font-lock-mode)
(define-key my-win-keymap (kbd "g") #'global-hl-line-mode)
(define-key my-win-keymap (kbd "h") #'font-lock-update)
(define-key my-win-keymap (kbd "l") #'my/sync-ui-accent-color)
(define-key my-win-keymap (kbd "n") #'display-line-numbers-mode)
(define-key my-win-keymap (kbd "o") #'my/center-buffer-mode)
(define-key my-win-keymap (kbd "p") #'variable-pitch-mode)
(define-key my-win-keymap (kbd "q") #'toggle-menu-bar-mode-from-frame)
(define-key my-win-keymap (kbd "r") #'my/rainbow-mode)
(define-key my-win-keymap (kbd "u") #'set-cursor-color)
(define-key my-win-keymap (kbd "U") #'set-foreground-color)
(define-key my-win-keymap (kbd "B") #'set-background-color)
(define-key my-win-keymap (kbd "t")
            (lambda () (interactive)
              (org-table-map-tables 'org-table-align)))

;;
;; -> keys-other-core
;;
(global-set-key (kbd "M-s =") #'ediff-buffers)
(global-set-key (kbd "M-s +") #'ediff-regions-linewise)
(define-key my-overrides-mode-map (kbd "M-h") #'my/mark-block)
(defun my/occur-from-isearch (&optional nlines)
  "Run `occur' with the current isearch string, using global `case-fold-search'."
  (interactive "P")
  (occur (if isearch-regexp
             isearch-string
           (regexp-quote isearch-string))
         nlines))
(global-set-key (kbd "M-c") #'occur)
(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "M-c") #'my/occur-from-isearch))
(global-set-key (kbd "M-s x") #'diff-buffer-with-file)
(global-set-key (kbd "C-c b") #'my/copy-buffer-to-kill-ring)
(global-set-key (kbd "C-c f") #'my/find-file)
(global-set-key (kbd "C-c g") #'my/grep)
(define-key my-overrides-mode-map (kbd "C-c o") #'bookmark-jump)

;;
;; -> keybinding-core
;;
(put 'windmove-left  'repeat-map 'windmove-repeat-map)
(put 'windmove-right 'repeat-map 'windmove-repeat-map)
(put 'windmove-up    'repeat-map 'windmove-repeat-map)
(put 'windmove-down  'repeat-map 'windmove-repeat-map)

(defvar windmove-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'windmove-left)
    (define-key map (kbd "j") #'windmove-down)
    (define-key map (kbd "k") #'windmove-up)
    (define-key map (kbd "l") #'windmove-right)
    map))

(define-key my-overrides-mode-map (kbd "M-L") (lambda () (interactive)
                              (my/adaptive-resize t -2)))
(define-key my-overrides-mode-map (kbd "M-H") (lambda () (interactive)
                              (my/adaptive-resize t 2)))
(define-key my-overrides-mode-map (kbd "M-J") (lambda () (interactive)
                              (my/adaptive-resize nil -1)))
(define-key my-overrides-mode-map (kbd "M-K") (lambda () (interactive)
                              (my/adaptive-resize nil 1)))
(global-set-key (kbd "C--") (lambda ()(interactive)(text-scale-adjust -1)))
(global-set-key (kbd "C-=") (lambda ()(interactive)(text-scale-adjust 1)))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "<f12>") #'(lambda ()(interactive)(async-shell-command "do_backup home" "*backup*")))
(global-set-key (kbd "C-c c") #'org-capture)
(define-key my-overrides-mode-map (kbd "M-[") #'my/shell-menu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") #'kill-buffer)
(define-key my-overrides-mode-map (kbd "C-c j") #'(lambda() (interactive)(tab-bar-history-back)(my/repeat-history)))
(define-key my-overrides-mode-map (kbd "C-c k") #'(lambda() (interactive)(tab-bar-history-forward)(my/repeat-history)))
(global-set-key (kbd "C-x l") #'scroll-lock-mode)
(global-set-key (kbd "C-x v e") 'vc-ediff)
(global-set-key (kbd "C-x x g") #'revert-buffer)
(global-set-key (kbd "C-x x t") #'toggle-truncate-lines)
(global-set-key (kbd "C-;") #'my/comment-or-uncomment)
(define-key my-overrides-mode-map (kbd "M-0") 'delete-window)
(define-key my-overrides-mode-map (kbd "M-1") #'delete-other-windows)
(define-key my-overrides-mode-map (kbd "M-2") #'split-window-vertically)
(define-key my-overrides-mode-map (kbd "M-3") #'split-window-horizontally)
(setq tab-bar-select-tab-modifiers '(control))
(defun my/dired-jump-or-up ()
  "If in Dired, go up a directory; otherwise dired-jump for current buffer."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-up-directory)
    (dired-jump)))
(define-key my-overrides-mode-map (kbd "M-e") #'my/dired-jump-or-up)
(global-set-key (kbd "M-g i") #'imenu)
(global-set-key (kbd "M-g o") #'org-goto)
(define-key my-overrides-mode-map (kbd "C-w") 'my-window-map)
(define-key my-overrides-mode-map (kbd "M-u") #'tab-bar-switch-to-prev-tab)
(define-key my-overrides-mode-map (kbd "M-i") #'tab-bar-switch-to-next-tab)
(define-key my-overrides-mode-map (kbd "C-x [") #'beginning-of-buffer)
(define-key my-overrides-mode-map (kbd "C-x ]") #'end-of-buffer)
(define-key my-overrides-mode-map (kbd "M-j") #'(lambda ()(interactive)(scroll-up (/ (window-height) 4))))
(define-key my-overrides-mode-map (kbd "M-k") #'(lambda ()(interactive)(scroll-down (/ (window-height) 4))))
(define-key my-overrides-mode-map (kbd "M-a") #'save-buffer)
(define-key my-overrides-mode-map (kbd "M-;") #'my/quick-window-jump)
(define-key my-overrides-mode-map (kbd "M-o") #'other-window)

(global-set-key (kbd "C-c U") #'my/disk-space-query)
(global-set-key (kbd "M-z") #'visual-line-mode)
(global-set-key (kbd "M-s i") #'my/convert-markdown-clipboard-to-org)
(global-set-key (kbd "M-s u") #'my/org-promote-all-headings)
(defun my/vc-dir-diff-stay ()
  "Show the file at point from vc-dir, keeping point in the vc-dir window.
For unregistered files just display the file in another window;
otherwise run `vc-diff'."
  (interactive)
  (let* ((win (selected-window))
         (node (ewoc-locate vc-ewoc))
         (state (and node (vc-dir-fileinfo->state (ewoc-data node)))))
    (if (eq state 'unregistered)
        (display-buffer (find-file-noselect (vc-dir-current-file)))
      (call-interactively #'vc-diff))
    (select-window win)))
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "e") #'vc-ediff)
  (define-key vc-dir-mode-map (kbd "SPC") #'my/vc-dir-diff-stay))
(defun my/vc-dir-here ()
  "Run vc-dir on the current directory (dired's dir when called from dired)."
  (interactive)
  (vc-dir default-directory))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-x v D") #'my/vc-dir-here))
(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "M-j") #'nil)
  (define-key diff-mode-map (kbd "M-k") #'nil))

;;
;; -> vc-git-skip-worktree-core
;;
(defun my/vc-git-skip-worktree-p (file)
  "Return non-nil if FILE has git's skip-worktree bit set."
  (and (eq (vc-backend file) 'Git)
       (with-temp-buffer
         (vc-git-command (current-buffer) 0 (list file) "ls-files" "-v"
                         "--" (file-relative-name file (vc-git-root file)))
         (goto-char (point-min))
         (looking-at "S "))))

(defun my/vc-git-toggle-skip-worktree (file)
  "Toggle git --skip-worktree on FILE.
Interactively, FILE is the file at point in `vc-dir' or the current
buffer's file otherwise."
  (interactive
   (list (if (derived-mode-p 'vc-dir-mode)
             (vc-dir-current-file)
           buffer-file-name)))
  (if (or (not file) (not (eq (vc-backend file) 'Git)))
      (user-error "Not under git")
    (let ((turning-on (not (my/vc-git-skip-worktree-p file))))
      (vc-git-command nil 0 (list file) "update-index"
                      (if turning-on "--skip-worktree" "--no-skip-worktree")
                      (file-relative-name file (vc-git-root file)))
      (vc-resynch-buffer file nil nil)
      (message "skip-worktree %s for %s"
               (if turning-on "SET" "CLEAR")
               (file-relative-name file (vc-git-root file)))
      (when (derived-mode-p 'vc-dir-mode) (vc-dir-refresh)))))

(defun my/vc-git-clear-skip-worktree (file)
  "Clear git --skip-worktree on FILE.
Interactively, FILE is the file at point in `vc-dir' or the current
buffer's file otherwise.  No-op if the bit is not set."
  (interactive
   (list (if (derived-mode-p 'vc-dir-mode)
             (vc-dir-current-file)
           buffer-file-name)))
  (if (or (not file) (not (eq (vc-backend file) 'Git)))
      (user-error "Not under git")
    (if (not (my/vc-git-skip-worktree-p file))
        (message "skip-worktree not set for %s"
                 (file-relative-name file (vc-git-root file)))
      (vc-git-command nil 0 (list file) "update-index" "--no-skip-worktree"
                      (file-relative-name file (vc-git-root file)))
      (vc-resynch-buffer file nil nil)
      (message "skip-worktree CLEARED for %s"
               (file-relative-name file (vc-git-root file)))
      (when (derived-mode-p 'vc-dir-mode) (vc-dir-refresh)))))

(defun my/vc-git-clear-all-skip-worktree ()
  "Clear git --skip-worktree on every flagged file in the current repo."
  (interactive)
  (let ((root (vc-git-root default-directory)))
    (if (not root)
        (user-error "Not in a git repository")
      (let ((files (mapcar #'car (my/vc-git--skip-worktree-entries root))))
        (if (null files)
            (message "No skip-worktree files in %s" root)
          (when (y-or-n-p (format "Clear skip-worktree on %d file(s)? "
                                  (length files)))
            (dolist (f files)
              (let ((abs (expand-file-name f root)))
                (vc-git-command nil 0 (list abs) "update-index"
                                "--no-skip-worktree" f)))
            (vc-resynch-buffer root nil nil)
            (message "skip-worktree CLEARED for %d file(s)" (length files))
            (when (derived-mode-p 'vc-dir-mode) (vc-dir-refresh))))))))

;;; Inject skip-worktree files into the vc-dir listing.
;;;
;;; On its own git reports a skip-worktree file as clean, so `vc-dir'
;;; never shows it.  We wrap `vc-git-dir-status-files' (the backend
;;; status collector) and append an entry with state `skip-worktree'
;;; for every flagged file under the vc-dir directory, fetched once
;;; synchronously from `git ls-files -v -z'.

(defun my/vc-git--skip-worktree-entries (dir)
  "Return vc-dir entries (FILE STATE EXTRA) for skip-worktree files under DIR."
  (let (entries tokens)
    (with-temp-buffer
      (let ((default-directory dir))
        (vc-git-command (current-buffer) 0 nil "ls-files" "-v" "-z"))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (skip-chars-forward "^\0")
          (push (buffer-substring-no-properties start (point)) tokens)
          (unless (eobp) (forward-char 1))))     ; consume the NUL
      (dolist (tok tokens)
        (when (string-prefix-p "S " tok)
          (push (list (substring tok 2) 'skip-worktree nil) entries))))
    entries))

(defun my/vc-git-dir-status-files--inject-skip-worktree (orig-fn dir files update-function)
  "Wrap UPDATE-FUNCTION so skip-worktree files appear in the vc-dir listing."
  (let* ((skip-entries (my/vc-git--skip-worktree-entries dir))
         (orig-update update-function))
    (funcall orig-fn dir files
             (lambda (entries &optional more-to-come)
               (funcall orig-update
                        (if (or more-to-come (null skip-entries))
                            entries
                          (let ((present (mapcar #'car entries)))
                            (append entries
                                    (delq nil
                                          (mapcar (lambda (e)
                                                    (unless (member (car e) present) e))
                                                  skip-entries)))))
                        more-to-come)))))

(defun my/vc-dir-printer--skip-marker (fn fileentry)
  "Append a warning-coloured {S} marker to skipped entries when rendering vc-dir."
  (funcall fn fileentry)
  (when (eq (vc-dir-fileinfo->state fileentry) 'skip-worktree)
    (let ((inhibit-read-only t))
      (insert (propertize "  {S}" 'face 'warning)))))

(with-eval-after-load 'vc-git
  (advice-add 'vc-git-dir-status-files :around
              #'my/vc-git-dir-status-files--inject-skip-worktree))
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "* s") #'my/vc-git-toggle-skip-worktree)
  (define-key vc-dir-mode-map (kbd "* u") #'my/vc-git-clear-skip-worktree)
  (define-key vc-dir-mode-map (kbd "* U") #'my/vc-git-clear-all-skip-worktree)
  (advice-add 'vc-dir-printer :around #'my/vc-dir-printer--skip-marker))

;;
;; -> modes-core
;;
(tooltip-mode -1)
(column-number-mode 1)
(desktop-save-mode -1)
(display-time-mode -1)
(global-auto-revert-mode t)
(savehist-mode 1)
(show-paren-mode t)
(tab-bar-history-mode 1)
(global-font-lock-mode t)
(repeat-mode 1)

;;
;; -> bell-core
;;
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;
;; -> coding-core
;;
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;
;; -> setqs-core
;;
(setq mouse-highlight nil)
(setq show-help-function nil)
(setq custom-safe-themes t)
(setq enable-local-variables :all)
(setq frame-title-format "%f")
(setq kill-whole-line t)
(setq-default truncate-lines t)
(setq frame-inhibit-implied-resize t)
(setq native-comp-async-report-warnings-errors nil)
(setq max-mini-window-height 10)
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil)

;;
;; -> confirm-core
;;
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(set-buffer-modified-p nil)
(setq delete-by-moving-to-trash t)

;;
;; -> trash-core
;;
;; When t, file deletion uses the async rsync/PowerShell trash mechanism.
;; When nil (default), falls back to Emacs' built-in blocking deletion
;; (which still honours `delete-by-moving-to-trash' on supported platforms).
(defvar my/async-trash-enabled nil
  "If non-nil, use async rsync/PowerShell to move files to trash.
When nil, rely on Emacs' default blocking deletion mechanism.")

(defun my/async-move-file-to-trash (filename)
  "Move FILENAME to trash asynchronously via rsync (Linux) or PowerShell (Win).
Writes the .trashinfo sidecar synchronously (tiny, instant), then launches
an async process for the actual file move with header-line progress."
  (setq filename (directory-file-name (expand-file-name filename)))
  (cond
   ((eq system-type 'windows-nt)
    (let ((pwsh (or (executable-find "powershell.exe")
                    (executable-find "powershell")))
          (origin-buf (current-buffer)))
      (if pwsh
          (let* ((escaped (replace-regexp-in-string "'" "''" filename))
                 (ps-script
                  (format
                   "Add-Type -AssemblyName Microsoft.VisualBasic; $ErrorActionPreference='Stop'; $p='%s'; try { if (Test-Path -LiteralPath $p -PathType Container) { [Microsoft.VisualBasic.FileIO.FileSystem]::DeleteDirectory($p,'OnlyErrorDialogs','SendToRecycleBin') } else { [Microsoft.VisualBasic.FileIO.FileSystem]::DeleteFile($p,'OnlyErrorDialogs','SendToRecycleBin') } } catch { Write-Error $_; exit 1 }"
                   escaped))
                 (proc (start-process "trash" nil pwsh
                                      "-NoProfile" "-NonInteractive"
                                      "-Command" ps-script)))
            (set-process-sentinel
             proc
             (lambda (proc event)
               (when (buffer-live-p origin-buf)
                 (with-current-buffer origin-buf
                   (when (derived-mode-p 'dired-mode)
                     (revert-buffer))))
               (when (string-prefix-p "exited abnormally" event)
                 (message "Trash failed for %s" filename)))))
        (if (file-directory-p filename)
            (delete-directory filename t nil)
          (delete-file filename nil)))))
   (t
    ;; Linux: freedesktop.org trash via mv (dirs) or rsync (files)
    (let* ((trash-base (or (getenv "XDG_DATA_HOME")
                           (expand-file-name "~/.local/share")))
           (trash-dir (expand-file-name "Trash" trash-base))
           (files-dir (expand-file-name "files" trash-dir))
           (info-dir (expand-file-name "info" trash-dir))
           (name (file-name-nondirectory filename))
           (trash-file (expand-file-name name files-dir))
           (counter 1))
      (make-directory files-dir t)
      (make-directory info-dir t)
      (while (file-exists-p trash-file)
        (setq trash-file (expand-file-name
                          (format "%s.%d%s" (file-name-base name) counter
                                  (or (file-name-extension name) ""))
                          files-dir)
              counter (1+ counter)))
      (let ((info-file (expand-file-name
                        (concat (file-name-nondirectory trash-file) ".trashinfo")
                        info-dir)))
        (unless (file-exists-p info-file)
          (with-temp-file info-file
            (insert (format "[Trash Info]\nPath=%s\nDeletionDate=%s\n"
                            filename (format-time-string "%Y-%m-%dT%H:%M:%S"))))))
      (let* ((is-dir (file-directory-p filename))
             (origin-buf (current-buffer))
             (proc (if is-dir
                       (start-process "trash" nil "mv" filename trash-file)
                     (start-process "trash" nil "rsync" "-a" "--remove-source-files"
                                    "--info=progress2" filename trash-file)))
             (buf (generate-new-buffer
                   (format "*trash %s*" (file-name-nondirectory filename)))))
        (set-process-buffer proc buf)
        (set-process-filter proc #'my/async-transfer--filter)
        (push proc my/async-transfer-rsync-jobs)
        (my/async-transfer-header-start)
        (set-process-sentinel
         proc
         (lambda (proc event)
           (setq my/async-transfer-rsync-progress nil)
           (my/async-transfer-header-update)
           (cond
            ((string= event "finished\n")
             (when (and is-dir (file-directory-p filename))
               (ignore-errors (delete-directory filename t)))
             (when (buffer-live-p origin-buf)
               (with-current-buffer origin-buf
                 (when (derived-mode-p 'dired-mode)
                   (revert-buffer)))))
            ((string-prefix-p "exited abnormally" event)
             (message "Trash failed for %s" (file-name-nondirectory filename))
             (when (buffer-live-p origin-buf)
               (with-current-buffer origin-buf
                 (when (derived-mode-p 'dired-mode)
                   (revert-buffer)))))))))))))

(defun my/dired-async-do-delete (&optional arg)
  "Delete marked files, trashing asynchronously via rsync or PowerShell.
With prefix ARG, permanently delete instead of trashing.
When `my/async-trash-enabled' is nil, delegates to the standard
`dired-do-delete' (blocking Emacs deletion) instead."
  (interactive "P")
  (if (not my/async-trash-enabled)
      (dired-do-delete arg)
    (let ((files (dired-get-marked-files nil current-prefix-arg nil nil t)))
      (if (or (not dired-deletion-confirmer)
              (and (functionp dired-deletion-confirmer)
                   (funcall dired-deletion-confirmer
                            (format (if arg "Permanently delete %d file%s? "
                                      "Trash %d file%s? ")
                                    (length files)
                                    (if (= 1 (length files)) "" "s")))))
          (dolist (file files)
            (if arg
                (progn
                  (if (file-directory-p file)
                      (delete-directory file t nil)
                    (delete-file file nil))
                  (ignore-errors (dired-remove-file file)))
              (my/async-move-file-to-trash file)
              (ignore-errors (dired-remove-file file))))
        (message "Aborted")))))

;; Replace the built-in move-file-to-trash with the async version when enabled.
;; This makes D and x (flagged delete) in dired use the async path automatically.
(when (and my/async-trash-enabled (not (eq system-type 'windows-nt)))
  (defalias 'system-move-file-to-trash #'my/async-move-file-to-trash))

;;
;; -> backups-core
;;
(setq make-backup-files 1)
(setq backup-directory-alist '(("." . "~/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 10   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;;
;; -> custom-settings-core
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monospace" :foundry "ADBO" :slant normal :weight regular :height 100 :width normal))))
 '(mode-line ((t (:height 140 :underline nil :overline nil :box nil))))
 '(mode-line-inactive ((t (:height 140 :underline nil :overline nil :box nil))))
 '(org-level-1 ((t (:inherit default :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-3 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-4 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-5 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-6 ((t (:inherit default :weight regular :height 1.0))))
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(font-lock-warning-face ((t (:foreground "#930000" :inverse-video nil))))
 '(org-link ((t (:underline nil))))
 '(indent-guide-face ((t (:background "#282828" :foreground "#666666"))))
 '(widget-button ((t (:inherit fixed-pitch :weight regular))))
 '(window-divider ((t (:foreground "black"))))
 '(org-tag ((t (:height 0.9))))
 '(vertical-border ((t (:foreground "#000000")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(warning-suppress-log-types '((frameset)))
 '(warning-suppress-types '((frameset))))

(set-cursor-color "white")

;;
;; -> defun-core
;;
(defun insert-default-background-color ()
  "Insert the default background color at point."
  (interactive)
  (insert (downcase (face-attribute 'default :background))))

(defun my/save-macro (name)
  "Save a macro by NAME."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline))

(defun my/comment-or-uncomment ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))

(defun my/dired-duplicate-file (arg)
  "Duplicate a file from DIRED with an incremented number.
If ARG is provided, it sets the starting counter.
Copies asynchronously via rsync (Linux) or copy/robocopy (Windows)
with header-line progress."
  (interactive "P")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file)
         (dired-dir (dired-current-directory))
         (is-dir (file-directory-p file))
         (rsync-source (if is-dir (file-name-as-directory file) file)))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (let* ((proc
            (cond
             ((eq system-type 'windows-nt)
              (if is-dir
                  (start-process
                   "dup" nil "robocopy"
                   (directory-file-name file)
                   new-file
                   "/E" "/COPY:DAT" "/R:0" "/W:0" "/NP" "/NJH" "/NJS"
                   "/NDL" "/NFL" "/BYTES" "/MT:8")
                (let ((pwsh (or (executable-find "powershell.exe")
                                (executable-find "powershell"))))
                  (if pwsh
                      (start-process "dup" nil pwsh
                                     "-NoProfile" "-NonInteractive"
                                     "-Command"
                                     (format "Copy-Item -LiteralPath '%s' -Destination '%s'"
                                             (replace-regexp-in-string "'" "''" file)
                                             (replace-regexp-in-string "'" "''" new-file)))
                    (start-process "dup" nil "cmd" "/c" "copy" "/Y"
                                   file new-file)))))
             (t
              (start-process "dup" nil "rsync" "-a" "--info=progress2"
                             rsync-source new-file))))
           (buf (generate-new-buffer
                 (format "*dup %s*" (file-name-nondirectory new-file)))))
      (set-process-buffer proc buf)
      (set-process-filter proc #'my/async-transfer--filter)
      (push proc my/async-transfer-rsync-jobs)
      (my/async-transfer-header-start)
      (set-process-sentinel
       proc
       (lambda (proc event)
         (setq my/async-transfer-rsync-progress nil)
         (my/async-transfer-header-update)
         (let ((exit-code (or (process-exit-status proc) -1)))
           (cond
            ((and is-dir (eq system-type 'windows-nt)
                  (<= exit-code 7))
             (message "Duplicated: %s" (file-name-nondirectory new-file))
             (with-current-buffer (find-file-noselect dired-dir)
               (revert-buffer nil t)))
            ((string= event "finished\n")
             (message "Duplicated: %s" (file-name-nondirectory new-file))
             (with-current-buffer (find-file-noselect dired-dir)
               (revert-buffer nil t)))
            ((string-prefix-p "exited abnormally" event)
             (message "Duplicate failed for %s (exit %d)"
                      (file-name-nondirectory new-file) exit-code)))))))))

(defun my/mark-block ()
  "Marking a block of text surrounded by a newline."
  (interactive)
  (when (not (region-active-p))
    (backward-char))
  (skip-chars-forward " \n\t")
  (re-search-backward "^[ \t]*\n" nil 1)
  (skip-chars-forward " \n\t")
  (when (not (region-active-p))
    (push-mark nil t t))
  (re-search-forward "^[ \t]*\n" nil 1)
  (skip-chars-backward " \n\t"))
;;
(defun my/repeat-history ()
  "Set up a transient keymap for navigating tab bar history."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") (lambda () (interactive)
                                (tab-bar-history-back)))
    (define-key map (kbd "k") (lambda () (interactive)
                                (tab-bar-history-forward)))
    (set-transient-map map t)))

;; `tab-bar-history-mode' deliberately deduplicates consecutive invocations
;; of the *same command* (see the eq check against `tab-bar-history-done-command'
;; in tab-bar.el's `tab-bar--history-change').  That is helpful for interactive
;; window resizing but means repeated `dired-find-file' / `dired-up-directory'
;; navigations only record the first step, so `tab-bar-history-back' jumps
;; higher up the tree than expected.  Clearing the done-command before each
;; dired navigation command makes the eq check fail and the intermediate
;; window configurations get recorded normally.
(defun my/tab-bar-history-allow-repeat (&rest _)
  "Let `tab-bar-history-mode' record consecutive invocations of a command."
  (when (bound-and-true-p tab-bar-history-mode)
    (setq tab-bar-history-done-command nil)))

(with-eval-after-load 'dired
  (dolist (cmd '(dired-find-file
                 dired-find-alternate-file
                 dired-find-file-other-window
                 dired-up-directory
                 dired-jump))
    (advice-add cmd :before #'my/tab-bar-history-allow-repeat)))

(defun my/get-window-position ()
  "Return the position of the current window as 'left', 'right', 'top', or 'bottom'."
  (let* ((edges (window-edges))
         (min-x (nth 0 edges))
         (min-y (nth 1 edges))
         (max-x (nth 2 edges))
         (max-y (nth 3 edges))
         (frame-width (frame-width))
         (frame-height (frame-height)))
    (cond
     ((<= min-x 0) 'left)
     ((>= max-x frame-width) 'right)
     ((= min-y 0) 'top)
     ((= max-y frame-height) 'bottom)
     (t 'center))))

(defun my/adaptive-resize (horizontal delta)
  "Resize the current window adaptively based on its position.
HORIZONTAL is non-nil for horizontal resizing (left/right).
DELTA is the amount to resize (positive to grow, negative to shrink)."
  (let ((pos (my/get-window-position)))
    (cond
     ((and horizontal (eq pos 'left)) (enlarge-window (- delta) t))
     ((and horizontal (eq pos 'right)) (enlarge-window delta t))
     ((and (not horizontal) (eq pos 'top)) (enlarge-window delta nil))
     ((and (not horizontal) (eq pos 'bottom)) (enlarge-window (- delta) nil))
     (t (enlarge-window delta horizontal)))))

(defun my/dired-du--unix-path (path)
  "Convert a Windows drive-letter PATH to MSYS2/Git-Portable Unix style.
e.g. c:/Users/foo -> /c/Users/foo, C:\\Users\\foo -> /c/Users/foo.
Non-Windows or non-drive-letter paths are returned unchanged."
  (if (and (eq system-type 'windows-nt)
           (string-match "\\`\\([a-zA-Z]\\):[/\\]" path))
      (concat "/" (downcase (match-string 1 path)) "/"
              (replace-regexp-in-string "\\\\" "/" (substring path (match-end 0))))
    path))

(defun my/dired-du (&optional depth)
  "Show disk usage of the directory under the cursor in Dired.
Lists each entry's size and file count, sorted largest-first, in a
buffer named *dired-du*.  DEPTH controls how many directory levels
deep to descend (passed to `du --max-depth'); it defaults to 1 and is
taken from the numeric prefix argument, so e.g. \\[universal-argument] 3
\\[my/dired-du] shows three levels.

On Windows the Unix pipeline is run through bash (from PortableGit)
with the directory path converted to MSYS2 style (/c/Users/...)."
  (interactive "p")
  (let ((current-dir (dired-get-file-for-visit))
        (depth (max 1 (or depth 1))))
    (if (file-directory-p current-dir)
        (let* ((buf (get-buffer-create "*dired-du*"))
               (unix-dir (my/dired-du--unix-path current-dir))
               (dir (shell-quote-argument unix-dir))
               ;; On Windows, use bash so Unix pipes work; otherwise
               ;; rely on the default shell.
               (shell-file-name
                (if (eq system-type 'windows-nt)
                    (or (executable-find "bash") shell-file-name)
                  shell-file-name))
               (shell-command-switch
                (if (eq system-type 'windows-nt) "-c" shell-command-switch))
               ;; List entries by size (KiB) descending, then render
               ;; each as: SIZE  FILES  NAME with aligned columns.
               ;; `column' may be absent on Windows (Git Portable);
               ;; fall back to cat so the pipeline still succeeds.
               (command
                (format "du -k --max-depth=%d %s 2>/dev/null | sort -rn | \
awk -v base=%s 'BEGIN{print \"SIZE\\tFILES\\tNAME\"}
{
  size=$1; $1=\"\"; sub(/^[ \\t]+/,\"\"); path=$0;
  # human-readable size
  h=size; unit=\"K\";
  if (h>=1048576){h=h/1048576; unit=\"G\"}
  else if (h>=1024){h=h/1024; unit=\"M\"}
  hs=sprintf(\"%%.1f%%s\", h, unit);
  # count files under this path
  cmd=\"find \\\"\" path \"\\\" -type f 2>/dev/null | wc -l\";
  cmd | getline files; close(cmd);
  name=path; sub(base \"/?\", \"\", name); if(name==\"\") name=\".\";
  printf \"%%s\\t%%s\\t%%s\\n\", hs, files, name;
}' | { column -t -s '\\t' 2>/dev/null || cat; }"
                        depth dir unix-dir)))
          (with-current-buffer buf
            (erase-buffer)
            (let ((process (start-process-shell-command "dired-du" buf command)))
              (set-process-sentinel
               process
               (lambda (_proc event)
                 (when (and (string-match "finished" event) (buffer-live-p buf))
                   (with-current-buffer buf
                     (goto-char (point-min))
                     (insert (format "Disk usage for %s (depth %d, largest first)\n\n"
                                     current-dir depth))))))
              (pop-to-buffer buf))))
      (message "The current point is not a directory."))))

(defun adjust-color (color percent)
  "Adjust COLOR by PERCENT (positive to lighten, negative to darken).
For very dark backgrounds, ensures a minimum visible difference."
  (let* ((rgb (color-values color))
         (factor (/ (+ 100 percent) 100.0))
         (min-increment 4096)  ; minimum increment for very dark colors
         (new-rgb (mapcar (lambda (x)
                            (if (> percent 0)
                                ;; When lightening, ensure minimum increment
                                (max (+ x min-increment)
                                     (round (* x factor)))
                              ;; When darkening, just use factor
                              (max 0 (round (* x factor)))))
                          rgb)))
    (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (/ x 256)) new-rgb))))

(defun set-simple-hl-line ()
  "Set the hl-line background based on current theme.
Lightens dark themes by 20%, darkens light themes by 5%."
  (interactive)
  (require 'hl-line)
  (unless global-hl-line-mode
    (global-hl-line-mode 1))
  (when (facep 'hl-line)
    (let* ((bg (face-background 'default))
           (rgb (color-values bg))
           (luminance (/ (+ (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)) 3))
           (is-dark (< luminance 32768))
           (adjusted-bg (if is-dark
                            (adjust-color bg 20)
                          (adjust-color bg -5))))
      (custom-set-faces
       `(hl-line ((t (:background ,adjusted-bg))))))))

(defun my/html-flush-divs ()
  "Flush the divs in export to improve on Confluence import."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((org-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (flush-lines "</?div.*>?")
      (write-region (point-min) (point-max) html-file))))

(defun my/html-promote-headers ()
  "Promote all headers in the HTML file by one level (e.g., h2 -> h1, h3 -> h2, etc.), accounting for attributes."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((org-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (while (re-search-forward "<\\(/\\)?h\\([2-6]\\)\\([^>]*\\)>" nil t)
        (let* ((closing (match-string 1))
               (level (string-to-number (match-string 2)))
               (attrs (match-string 3))
               (new-level (1- level)))
          (replace-match (format "<%sh%d%s>" (or closing "") new-level (or attrs "")))))
      (write-region (point-min) (point-max) html-file))))

(defun my/copy-buffer-to-kill-ring ()
  "Copy the entire buffer to the kill ring without changing the point."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message (concat (buffer-file-name) " Copied")))

(defun my/dired-file-to-org-link ()
  "Transform the file path under the cursor in Dired to an Org mode
  link and copy to kill ring.
  This function transforms the current file path in Dired mode into
  an Org link with attributes for both org-mode and HTML width
  settings. The generated link is then copied to the kill ring for
  easy pasting."
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (if file-path
        (let* ((relative-path (file-relative-name file-path
                                                  (project-root-safe)))
               (org-link (concat "#+attr_org: :width 300px\n"
                                 "#+attr_html: :width 100%\n"
                                 "[[file:" relative-path "]]\n")))
          (kill-new org-link)
          (message "Copied to kill ring: %s" org-link))
      (message "No file under the cursor"))))

(defun my/collate-issues-into-table ()
  "Insert all Org headings in the current buffer into the Org file."
  (interactive)
  (let ((rows '())
        (header '("TODO" "Title" "Parent Title")) ;; Table header
        (issue-tag "issues")) ;; The tag to filter for
    (save-excursion
      (goto-char (point-max)) ;; Ensure we append the results at the end
      (org-map-entries
       (lambda ()
         (let* ((todo (org-get-todo-state))
                (title (org-get-heading t t t t))
                (parent))
           (save-excursion
             (when (org-up-heading-safe) ;; Move to parent heading if it exists
               (setq parent (org-get-heading t t t t))))
           (when (member issue-tag (org-get-tags))
             (push (list (or todo "") title (or parent "")) rows))))
       nil 'file))
    (push 'hline rows)
    (cons header rows)))

(defun my/kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end))
(global-set-key [remap kill-ring-save] 'my/kill-ring-save)
(define-key my-overrides-mode-map (kbd "C-c w") 'my/kill-ring-save)

(defun my/disk-space-query ()
  "Run 'df -h' and display the output in a new buffer.
On Windows the command is run through bash (from PortableGit) since
`df' is a Unix tool and cmdproxy cannot handle it."
  (interactive)
  (let ((output-buffer-name "*Disk Space*")
        (shell-file-name
         (if (eq system-type 'windows-nt)
             (or (executable-find "bash") shell-file-name)
           shell-file-name))
        (shell-command-switch
         (if (eq system-type 'windows-nt) "-c" shell-command-switch)))
    (with-current-buffer (get-buffer-create output-buffer-name)
      (erase-buffer)
      (let* ((command "df -h")
             (process (start-process-shell-command "disk-space" output-buffer-name command)))
        (set-process-sentinel process 
                              (lambda (proc event)
                                (when (string-match "finished" event)
                                  (with-current-buffer output-buffer-name
                                    (goto-char (point-min))))))
        (pop-to-buffer output-buffer-name)))))

(defvar my/highlight-rules
  '((th . (("TODO" . "#999")))
    (td . (("\\&gt" . "#bbb")
           ("-\\&gt" . "#ccc")
           ("- " . "#ddd")
           ("- - - - " . "#eee")
           ("- - - - - - - - " . "#fff")
           ("HDR" . "#ffd")
           ("TODO" . "#fdd")
           ("DOING" . "#ddf")
           ("DONE" . "#dfd"))))
  "Alist of elements ('th or 'td) and associated keywords/colors for row highlighting.")

(defun my/apply-row-style (row-start row-attributes color)
  "Apply a background COLOR to the row starting at ROW-START with ROW-ATTRIBUTES."
  (goto-char row-start)
  (delete-region (line-beginning-position) (line-end-position))
  (insert (format "<tr%s style=\"background: %s\">" row-attributes color)))

(defun my/highlight-row-by-rules (row-start row-end row-attributes element)
  "Highlight a row based on ELEMENT ('th or 'td) keyword rules within ROW-START to ROW-END."
  (let ((rules (cdr (assoc element my/highlight-rules))))
    (dolist (rule rules)
      (let ((keyword (car rule))
            (color (cdr rule)))
        (when (save-excursion
                (and (re-search-forward (format "<%s.*>%s.*</%s>" element keyword element) row-end t)
                     (goto-char row-start)))
          (my/apply-row-style row-start row-attributes color))))))

(defun my/html-org-table-highlight ()
  "Open the exported HTML file and add background styles to rows containing keywords."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((org-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (while (re-search-forward "<table.*>" nil t)
        (let ((table-start (point))
              (table-end (save-excursion
                           (when (re-search-forward "</table>" nil t)
                             (point)))))
          (when table-end
            (save-restriction
              (narrow-to-region table-start table-end)
              (goto-char (point-min))
              (while (re-search-forward "<tr\\(.*\\)>" nil t)
                (let ((row-start (match-beginning 0))
                      (row-attributes (match-string 1))
                      (row-end (save-excursion (search-forward "</tr>"))))
                  (my/highlight-row-by-rules row-start row-end row-attributes 'th)
                  (my/highlight-row-by-rules row-start row-end row-attributes 'td)))))))
      (write-region (point-min) (point-max) html-file))))

(defun my/format-to-table (&optional match properties-to-display)
  "Format Org headings into a structured alist, optionally filtered by MATCH
   and displaying only specified PROPERTIES-TO-DISPLAY (e.g., '(\"ID\" \"PRIORITY\"))."
  (interactive)
  (let ((rows '())
        (header (list "TODO" "Tags" "Title" "Contents" "Properties")))
    
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))  ; Clean heading without TODO/tags
              (level (org-outline-level))
              (tags (org-get-tags))
              (todo (org-get-todo-state))
              (properties (org-entry-properties))
              (contents ""))
         
         ;; Extract first paragraph content
         (save-excursion
           (org-end-of-meta-data t)
           (when (looking-at-p "[ \t]*[^*\n]") ; Not another heading
             (let ((start (point)))
               (forward-paragraph)
               (setq contents (string-trim 
                               (replace-regexp-in-string 
                                "[ \t\n]+" " " 
                                (buffer-substring-no-properties start (point))))))))
         
         ;; Format title with indentation
         (let ((formatted-title 
                (concat (mapconcat (lambda (_) " - ") (make-list (max 0 (- level 2)) nil) "")
                        (cond ((= level 1) "▶ ")
                              ((= level 2) "▷ ")
                              (t "• "))
                        heading))
               ;; Filter properties
               (filtered-props
                (if properties-to-display
                    (string-join
                     (delq nil
                           (mapcar (lambda (prop)
                                     (let ((val (cdr (assoc prop properties))))
                                       (when val (format "%s:%s" prop val))))
                                   properties-to-display))
                     " ")
                  "")))
           
           (push (list (or todo "")
                       (if tags (string-join tags ":") "")
                       formatted-title
                       contents
                       filtered-props)
                 rows))))
     match)
    
    (when rows
      (setq rows (reverse rows))
      (push 'hline rows))
    (cons header rows)))

(defun my/vc-git-reset-and-clean ()
  "Discard all tracked changes and delete all unregistered files in the current project."
  (interactive)
  (let ((root (vc-git-root default-directory)))
    (if (not root)
        (error "Not in a Git repository")
      (when (yes-or-no-p "Permanently discard ALL changes and delete UNTRACKED files? ")
        (let ((default-directory root))
          (shell-command "git reset --hard HEAD && git clean -fd")
          (vc-resynch-buffer root t t)
          (message "Project wiped clean."))))))

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "K") #'my/vc-git-reset-and-clean))

(with-eval-after-load 'vc-hooks
  (define-key vc-prefix-map (kbd "K") #'my/vc-git-reset-and-clean))

;;
;; -> window-positioning-core
;;
(add-to-list 'display-buffer-alist
             '("\\*\\(.*shell\\|.*term.*\\)"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height . 0.4)))
(add-to-list 'display-buffer-alist
             '("\\*\\(eldoc.*\\*\\|Flymake.*\\)"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height . 0.4)))
(add-to-list 'display-buffer-alist
             '("\\*\\(Completions.*\\)"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height . 0.2)))
(add-to-list 'display-buffer-alist
             '("\\*grep"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.4)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*compilation"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.3)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-same-window)))
(add-to-list 'display-buffer-alist
             '("\\*Async" display-buffer-no-window
               (allow-no-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Messages" display-buffer-same-window))
(add-to-list 'display-buffer-alist
             '("\\*Process" display-buffer-same-window))
(add-to-list 'display-buffer-alist
             '("\\*vc-dir\\*"
               (display-buffer-same-window)))
(setq kill-buffer-query-functions nil)

;;
;; -> org-core
;;
(setq org-table-convert-region-max-lines 9999)
(setq org-src-tab-acts-natively t)
(setq org-log-done t)
(setq org-export-with-sub-superscripts nil)
(setq org-deadline-warning-days 365)
(setq org-image-actual-width (list 50))
(setq org-return-follows-link t)
(setq org-use-fast-todo-selection 'expert)
(setq org-reverse-note-order t)
(setq org-src-preserve-indentation t)
(setq org-cycle-separator-lines 1)
(setq org-edit-src-content-indentation 0)
(setq org-tags-sort-function 'org-string-collate-greaterp)
(setq org-startup-indented t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(g)" "REVIEW(r)" "ORDR(o)" "SENT(s)"
                  "|" "DONE(n)" "CANCELLED(c)" "CLOSED(l)")))
(setq org-todo-keyword-faces
      '(("TODO" . "#ee6273")
        ("DOING" . "#6e8baa")
        ("REVIEW" . "#d2b596")
        ("ORDR" . "#c96eee")
        ("SENT" . "#c86bee")
        ("DONE" . "#77aa66")
        ("CANCELLED" . "#426b3e")
        ("CLOSED" . "#6b6b6b")))
(defconst my/org-todo-emoji-map
  '(("TODO" . "📋")
    ("DOING" . "🔄")
    ("REVIEW" . "✍️")
    ("ORDR" . "📝")
    ("SENT" . "📤")
    ("DONE" . "✅")
    ("CANCELLED" . "❌")
    ("CLOSED" . "🔒"))
  "Mapping of TODO keywords to emoji prefixes for HTML export.")
(defun my/org-export-prepend-todo-emoji (text _backend _info)
  "Prepend emoji to TODO keywords in TEXT during HTML export."
  (dolist (pair my/org-todo-emoji-map text)
    (let ((keyword (car pair))
          (emoji (cdr pair)))
      (setq text (replace-regexp-in-string
                  (format ">%s<" (regexp-quote keyword))
                  (format ">%s %s<" emoji keyword)
                  text t t)))))
(add-to-list 'org-export-filter-body-functions
             'my/org-export-prepend-todo-emoji)
(setq org-goto-interface 'outline-path-completionp)
(setq org-outline-path-complete-in-steps nil)
(setq org-imenu-depth 1)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'") #'nil)
  (define-key org-mode-map (kbd "C-,") #'nil))
(setq imenu-flatten t)

;;
;; -> org-agenda-core
;;
(with-eval-after-load 'org-agenda
  (setq org-agenda-include-diary t)
  (setq org-agenda-show-all-dates t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-agenda-custom-commands
        '(("j" "Month View" agenda ""
           ((org-agenda-start-day "today")
            (org-agenda-span 30)
            (org-agenda-time-grid nil)))))
  (defun display-year-agenda (&optional year)
    "Display an agenda entry for a whole year."
    (interactive (list (read-string "Enter the year: "
                                    (format-time-string "%Y" (current-time)))))
    (setq year (string-to-number year))
    (org-agenda-list)
    (org-agenda-year-view year)
    (setq this-year (string-to-number (format-time-string "%Y" (current-time))))
    (when (= year this-year)
      (org-agenda-goto-today)
      (recenter-top-bottom 10))))

(global-set-key (kbd "C-c y") 'display-year-agenda)

;;
;; -> scroll-core
;;
(setq scroll-margin 10)
(setq scroll-conservatively 10)
(setq scroll-preserve-screen-position t)

;;
;; -> dired-core
;;
(setq dired-dwim-target t)
(setq dired-listing-switches "-alGgh")
(setq dired-auto-revert-buffer t)
(setq dired-confirm-shell-command nil)
(setq dired-no-confirm t)
(setq dired-deletion-confirmer (lambda (_x) t))
(setq dired-recursive-deletes 'always)

;;
;; -> async-transfer-header-line
;;
(defvar my/async-transfer-rsync-jobs nil
  "Live rsync processes tracked for the async-transfer header-line.")

(defvar my/async-transfer-rsync-progress nil
  "Most recent parsed rsync progress fragment (e.g. \"[87% 12.3MB/s]\").")

(defvar my/async-transfer-timer nil
  "Repeating timer that refreshes the async-transfer header-line.")

(defun my/async-transfer-count ()
  "Return plist (:rsync N) of active transfer jobs."
  (setq my/async-transfer-rsync-jobs
        (seq-filter #'process-live-p my/async-transfer-rsync-jobs))
  (list :rsync (length my/async-transfer-rsync-jobs)))

(defun my/async-transfer-header-update ()
  "Refresh a global header-line describing active rsync jobs."
  (let* ((c (my/async-transfer-count))
         (r (plist-get c :rsync))
         (face 'mode-line-highlight))
    (setq-default header-line-format
                  (when (> r 0)
                    (propertize
                     (concat " ⟳"
                             (format " %d rsync%s" r
                                     (if my/async-transfer-rsync-progress
                                         (concat " "
                                                 (replace-regexp-in-string
                                                  "%" "%%"
                                                  my/async-transfer-rsync-progress))
                                       ""))
                             " running ")
                     'face face)))
    (force-mode-line-update t)
    (when (and my/async-transfer-timer (zerop r))
      (cancel-timer my/async-transfer-timer)
      (setq my/async-transfer-timer nil))))

(defun my/async-transfer-header-start (&rest _)
  "Start the 0.5 s refresh timer if not already running."
  (unless my/async-transfer-timer
    (setq my/async-transfer-timer
          (run-at-time 0 0.5 #'my/async-transfer-header-update))))

(defun my/async-transfer--filter (proc chunk)
  "Filter for rsync --info=progress2 output.
Captures latest progress into `my/async-transfer-rsync-progress'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert chunk)))
  (let ((pos 0) (last nil))
    (while (string-match
            "\\([0-9]+\\)%[ \t]+\\([0-9.]+[kKMmGgTt]?B/s\\)"
            chunk pos)
      (setq last (cons (match-string 1 chunk) (match-string 2 chunk))
            pos  (match-end 0)))
    (when last
      (setq my/async-transfer-rsync-progress
            (format "[%s%% %s]" (car last) (cdr last))))))

(require 'transient)

(defconst my/dired-compress-formats
  '((?z . ("xz"    ".xz"     "xz -9e"   "tar --force-local -cf - %i | xz -9e > %o"))
    (?g . ("gzip"  ".gz"     "gzip -9"   "tar --force-local -cf - %i | gzip -9 > %o"))
    (?t . ("tar.gz" ".tar.gz" nil        "tar --force-local -czf %o %i"))
    (?b . ("bzip2" ".bz2"    "bzip2 -9"  "tar --force-local -cf - %i | bzip2 -9 > %o"))
    (?s . ("zstd"  ".zst"    "zstd -19"  "tar --force-local -cf - %i | zstd -19 -o %o"))
    (?l . ("lzip"  ".lz"     "lzip -9"   "tar --force-local -cf - %i | lzip -9 > %o"))
    (?7 . ("7z"    ".7z"     nil         "7z a %o %i")))
  "Compression formats for `my/dired-do-compress'.
Each entry is (KEY . (NAME SUFFIX FILE-CMD ARCHIVE-CMD)).
FILE-CMD is the command to compress a single file (nil if not applicable).
ARCHIVE-CMD is the command pattern for directories, using %i/%o placeholders.
For tar.gz, FILE-CMD is nil because single files should use gzip instead.")

(transient-define-prefix my/dired-compress-transient ()
  "Compress or uncompress marked files in Dired with format selection."
  [:description
   (lambda () (format "Compress %s"
                      (if-let ((files (dired-get-marked-files nil nil nil nil t)))
                          (mapconcat #'file-name-nondirectory files ", ")
                        "nothing")))
   ("z" "xz     – best ratio, slow"     my/dired-compress-xz)
   ("g" "gzip   – fast, decent ratio"  my/dired-compress-gzip)
   ("t" "tar.gz – fast, decent ratio"   my/dired-compress-tar.gz)
   ("b" "bzip2  – good ratio, medium"   my/dired-compress-bzip2)
   ("s" "zstd   – fast, good ratio"     my/dired-compress-zstd)
   ("l" "lzip   – best ratio, slow"     my/dired-compress-lzip)
   ("7" "7z     – best ratio, slow"      my/dired-compress-7z)])

(defun my/dired-compress-with (key)
  "Compress marked files using format assocated with KEY from `my/dired-compress-formats'.
Already-compressed files are decompressed via `dired-compress'."
  (let* ((fmt (cdr (assq key my/dired-compress-formats)))
         (name (nth 0 fmt))
         (suffix (nth 1 fmt))
         (file-cmd (nth 2 fmt))
         (archive-cmd (nth 3 fmt))
         (suffixes dired-compress-file-suffixes)
         (files (dired-get-marked-files nil current-prefix-arg nil nil t)))
    (unless fmt
      (user-error "Unknown compression key: %c" key))
    (when files
      (with-current-buffer (current-buffer)
        (when my/dired--header-timer
          (cancel-timer my/dired--header-timer))
        (setq header-line-format
              (concat (propertize " ● " 'face 'warning)
                      (propertize (format "Compressing %s..." name) 'face 'warning)
                      (propertize " ●" 'face 'warning)))
        (force-mode-line-update t)))
    (if (> (length files) 1)
        (my/dired-compress-files files name suffix archive-cmd)
      (dolist (file files)
        (let ((handler (find-file-name-handler file 'dired-compress-file))
              (already-compressed nil))
          (dolist (entry suffixes)
            (when (and (not already-compressed)
                       (string-match-p (car entry) file))
              (setq already-compressed t)))
          (if (or already-compressed handler
                  (and (file-directory-p file)
                       (cl-find-if
                        (lambda (s) (string-match-p (car s) file))
                        suffixes)))
              (dired-compress file)
            (let ((newname (concat file suffix)))
              (condition-case err
                  (if (or (file-directory-p file) (not file-cmd))
                      (my/dired-compress-directory file newname name archive-cmd)
                    (my/dired-compress-file file newname name file-cmd))
                (error (error "Compression of %s failed: %s"
                              file (error-message-string err)))))))))
    (dired-post-do-command)))

(defun my/dired-compress-files (files name suffix archive-cmd)
  "Compress FILES into a single archive using NAME/SUFFIX/ARCHIVE-CMD.
FILES is a list of file paths.  Prompts for archive name,
defaulting to the current dired directory name."
  (unless archive-cmd
    (user-error "Multi-file %s compression not supported" name))
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name default-directory)))
         (default-name (concat dir-name suffix))
         (archive-name (read-string
                        (format "Archive name (%s): " default-name)
                        default-name))
         (full-out (expand-file-name archive-name))
         (files-arg (mapconcat
                     (lambda (f) (shell-quote-argument (file-local-name f)))
                     files " "))
         (cmd (format-spec archive-cmd
                          `((?o . ,(shell-quote-argument
                                    (file-local-name full-out)))
                            (?i . ,files-arg)))))
    (when (and (file-exists-p full-out)
               (not (y-or-n-p (format "%s exists, overwrite? "
                                      (abbreviate-file-name full-out)))))
      (user-error "Aborted"))
    (message "Compressing %d files as %s... (async)"
             (length files) archive-name)
    (let ((process (start-file-process
                    (format "compress-%s" archive-name)
                    (generate-new-buffer
                     (format " *compress-%s*" archive-name))
                    shell-file-name shell-command-switch cmd)))
      (process-put process 'my-dir default-directory)
      (process-put process 'my-cmd-name archive-name)
      (process-put process 'my-dired-buffer (current-buffer))
      (process-put process 'my-newname full-out)
      (set-process-sentinel process #'my/dired-compress-sentinel)
      (set-process-query-on-exit-flag process nil))))

(defvar-local my/dired--header-timer nil)

(defun my/dired-compress-sentinel (process _event)
  "Sentinel for async (de)compression processes.
Reverts the dired buffer on completion, updates header-line, and reports errors.
The process property `my-action' (\"Compress\" or \"Decompress\", default
\"Compress\") selects the verb used in messages."
  (when (memq (process-status process) '(exit signal))
    (let* ((cmd-name (process-get process 'my-cmd-name))
           (action (or (process-get process 'my-action) "Compress"))
           (exit-code (process-exit-status process))
           (dired-buf (process-get process 'my-dired-buffer))
           (newname (process-get process 'my-newname)))
      (if (zerop exit-code)
          (let* ((filesize (when (and newname (file-exists-p newname))
                              (file-size-human-readable
                               (file-attribute-size (file-attributes newname)))))
                 (msg (concat (propertize "✓" 'face 'success)
                              (format " %sed: %s" action cmd-name)
                              (when filesize
                                (format " (%s)" filesize)))))
            (message "%s" msg)
            (when (buffer-live-p dired-buf)
              (with-current-buffer dired-buf
                (revert-buffer nil t t)
                (when my/dired--header-timer
                  (cancel-timer my/dired--header-timer))
                (setq header-line-format
                      (concat (propertize " ● " 'face 'success)
                              msg
                              (propertize " ●" 'face 'success)))
                (force-mode-line-update t)
                (setq my/dired--header-timer
                      (run-with-timer
                       6 nil
                       (lambda (buf)
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (setq header-line-format nil
                                   my/dired--header-timer nil))))
                       (current-buffer))))))
        (message "%s failed for %s (exit %d)" action cmd-name exit-code)
        (display-buffer (process-buffer process))
        (when (buffer-live-p dired-buf)
          (with-current-buffer dired-buf
            (revert-buffer nil t t)
            (when my/dired--header-timer
              (cancel-timer my/dired--header-timer))
            (setq header-line-format nil)
            (force-mode-line-update t)))))))

(defun my/dired-compress-directory (dir newname name archive-cmd)
  "Compress DIR as a tar archive named NEWNAME using NAME/ARCHIVE-CMD, asynchronously."
  (let* ((default-directory (file-name-directory dir))
         (full-out (expand-file-name newname))
         (cmd (format-spec archive-cmd
                           `((?o . ,(shell-quote-argument (file-local-name full-out)))
                             (?i . ,(shell-quote-argument (file-local-name dir)))))))
    (when (and (file-exists-p full-out)
               (not (y-or-n-p (format "%s exists, overwrite? "
                                      (abbreviate-file-name full-out)))))
      (user-error "Aborted"))
    (message "Compressing %s with %s... (async)" (file-name-nondirectory dir) name)
    (let ((process (start-file-process
                    (format "compress-%s" (file-name-nondirectory dir))
                    (generate-new-buffer (format " *compress-%s*"
                                                 (file-name-nondirectory dir)))
                    shell-file-name shell-command-switch cmd)))
      (process-put process 'my-dir default-directory)
      (process-put process 'my-cmd-name (file-name-nondirectory dir))
      (process-put process 'my-dired-buffer (current-buffer))
      (process-put process 'my-newname full-out)
      (set-process-sentinel process #'my/dired-compress-sentinel)
      (set-process-query-on-exit-flag process nil))))

(defun my/dired-compress-file (file newname name file-cmd)
  "Compress FILE as NEWNAME using NAME/FILE-CMD, asynchronously."
  (unless file-cmd
    (error "Single-file %s compression not supported for %s" name file))
  (when (and (file-exists-p newname)
             (not (y-or-n-p (format "%s exists, overwrite? "
                                    (abbreviate-file-name newname)))))
    (user-error "Aborted"))
  (message "Compressing %s with %s... (async)" (file-name-nondirectory file) name)
  (let* ((default-directory (file-name-directory file))
         (process (start-file-process
                   (format "compress-%s" (file-name-nondirectory file))
                   (generate-new-buffer (format " *compress-%s*"
                                                (file-name-nondirectory file)))
                   shell-file-name shell-command-switch
                   (format "%s %s" file-cmd (shell-quote-argument (file-local-name file))))))
    (process-put process 'my-dir default-directory)
    (process-put process 'my-cmd-name (file-name-nondirectory file))
    (process-put process 'my-dired-buffer (current-buffer))
    (process-put process 'my-newname (expand-file-name newname))
    (set-process-sentinel process #'my/dired-compress-sentinel)
    (set-process-query-on-exit-flag process nil)))

(defmacro my/dired-define-compress-command (key name suffix file-cmd archive-cmd)
  "Generate a dired compress command for one format."
  `(defun ,(intern (format "my/dired-compress-%s" name)) (&optional arg)
     ,(format "Compress marked files (or next ARG) as %s." name)
     (interactive "P" dired-mode)
     (my/dired-compress-with ,key)))

(my/dired-define-compress-command ?z "xz"    ".xz"     "xz -9e"   "tar --force-local -cf - %i | xz -9e > %o")
(my/dired-define-compress-command ?g "gzip"   ".gz"     "gzip -9"   "tar --force-local -cf - %i | gzip -9 > %o")
(my/dired-define-compress-command ?t "tar.gz" ".tar.gz" nil         "tar --force-local -czf %o %i")
(my/dired-define-compress-command ?b "bzip2"  ".bz2"    "bzip2 -9"  "tar --force-local -cf - %i | bzip2 -9 > %o")
(my/dired-define-compress-command ?s "zstd"   ".zst"    "zstd -19"  "tar --force-local -cf - %i | zstd -19 -o %o")
(my/dired-define-compress-command ?l "lzip"   ".lz"     "lzip -9"   "tar --force-local -cf - %i | lzip -9 > %o")
(my/dired-define-compress-command ?7 "7z"     ".7z"     nil         "7z a %o %i")

(defconst my/dired-decompress-formats
  '(("\\.\\(?:tar\\.\\(?:gz\\|xz\\|bz2\\|zst\\|lz\\)\\|tgz\\|txz\\|tbz2?\\|tzst\\|tar\\)\\'"
     . ("tar"   "tar --force-local -xf %i"))
    ("\\.gz\\'"  . ("gzip"  "gzip -d %i"))
    ("\\.xz\\'"  . ("xz"    "xz -d %i"))
    ("\\.bz2\\'" . ("bzip2" "bzip2 -d %i"))
    ("\\.zst\\'" . ("zstd"  "zstd -d --rm %i"))
    ("\\.lz\\'"  . ("lzip"  "lzip -d %i"))
    ("\\.7z\\'"  . ("7z"    "7z x -y %i"))
    ("\\.zip\\'" . ("zip"   "unzip -o %i")))
  "Decompression formats for `my/dired-decompress'.
Each entry is (REGEXP . (NAME COMMAND)) and is tried in order, so archive
formats must precede their single-file suffixes (e.g. .tar.gz before .gz).
COMMAND is a shell command pattern using the %i placeholder for the
shell-quoted input file.  `tar -xf' auto-detects the compression filter.")

(defun my/dired-decompress-file (file)
  "Decompress FILE asynchronously, dispatching on its extension.
Reuses `my/dired-compress-sentinel' for buffer refresh and reporting."
  (let* ((entry (cl-find-if (lambda (e) (string-match-p (car e) file))
                            my/dired-decompress-formats))
         (fmt (cdr entry))
         (name (nth 0 fmt))
         (cmd-pattern (nth 1 fmt)))
    (unless entry
      (user-error "Don't know how to decompress %s"
                  (file-name-nondirectory file)))
    (let* ((default-directory (file-name-directory file))
           (basename (file-name-nondirectory file))
           (cmd (format-spec cmd-pattern
                             `((?i . ,(shell-quote-argument
                                       (file-local-name file))))))
           (process (start-file-process
                     (format "decompress-%s" basename)
                     (generate-new-buffer (format " *decompress-%s*" basename))
                     shell-file-name shell-command-switch cmd)))
      (message "Decompressing %s with %s... (async)" basename name)
      (process-put process 'my-dir default-directory)
      (process-put process 'my-cmd-name basename)
      (process-put process 'my-dired-buffer (current-buffer))
      (process-put process 'my-action "Decompress")
      (set-process-sentinel process #'my/dired-compress-sentinel)
      (set-process-query-on-exit-flag process nil))))

(defun my/dired-decompress (&optional arg)
  "Decompress marked (or next ARG) files asynchronously.
Each file is dispatched on its extension via `my/dired-decompress-formats'."
  (interactive "P" dired-mode)
  (let ((files (dired-get-marked-files nil arg nil nil t)))
    (when files
      (with-current-buffer (current-buffer)
        (when my/dired--header-timer
          (cancel-timer my/dired--header-timer))
        (setq header-line-format
              (concat (propertize " ● " 'face 'warning)
                      (propertize "Decompressing..." 'face 'warning)
                      (propertize " ●" 'face 'warning)))
        (force-mode-line-update t))
      (dolist (file files)
        (my/dired-decompress-file file)))
    (dired-post-do-command)))

(defun my/dired-do-compress (&optional arg)
  "Compress or uncompress marked (or next ARG) files.
If any marked files are already compressed, decompress them
asynchronously via `my/dired-decompress'.  Otherwise show the format
selection transient."
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files nil current-prefix-arg nil nil t))
         (suffixes dired-compress-file-suffixes)
         (has-compressed nil))
    (dolist (file files)
      (dolist (entry suffixes)
        (when (string-match-p (car entry) file)
          (setq has-compressed t))))
    (if has-compressed
        (my/dired-decompress arg)
      (my/dired-compress-transient))))

;;
;; -> dired-clipboard-core
;;
(defgroup my-dired-clipboard nil
  "Simple copy/cut/paste for Dired files."
  :group 'dired
  :prefix "my-dired-clipboard-")

(defcustom my-dired-clipboard-recursive-copies 'always
  "Value bound to `dired-recursive-copies' while pasting."
  :type '(choice (const :tag "Ask" top)
                 (const :tag "Always" always)
                 (const :tag "Never" nil))
  :group 'my-dired-clipboard)

(defcustom my-dired-clipboard-existing-file-policy 'rename
  "How paste handles existing destination files.
`rename' uses copy-style names like \"file copy.txt\" or \"file copy 2.txt\".
`error' signals an error when the destination already exists."
  :type '(choice (const :tag "Rename" rename)
                 (const :tag "Error" error))
  :group 'my-dired-clipboard)

(defcustom my-dired-clipboard-keep-marker nil
  "Marker for files created by paste.
nil means do not mark pasted files; t uses the current Dired marker;
a character uses that marker."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Current" t)
                 (character :tag "Char"))
  :group 'my-dired-clipboard)

(defvar my-dired-clipboard--state nil
  "Clipboard state plist: (:operation OPERATION :files FILES).
OPERATION is `copy' or `cut'; FILES is a list of absolute file paths.
nil means the clipboard is empty.")

(defun my/dired-clipboard--region-active-p ()
  "Return non-nil when a non-empty region is active."
  (and (bound-and-true-p mark-active)
       (mark t)
       (> (region-end) (region-beginning))))

(defun my/dired-clipboard--ensure-not-wdired ()
  "Signal an error when called from WDired."
  (when (derived-mode-p 'wdired-mode)
    (user-error "File clipboard disabled in WDired")))

(defun my/dired-clipboard--copy-name (file index)
  "Return a copy-style name for FILE at numeric INDEX."
  (let* ((directory-p (file-directory-p file))
         (name (file-name-nondirectory (directory-file-name file)))
         (root (if directory-p name (file-name-sans-extension name)))
         (extension (if directory-p "" (file-name-extension name t)))
         (suffix (if (= index 1) " copy" (format " copy %d" index))))
    (concat root suffix extension)))

(defun my/dired-clipboard--unique-destination (file directory)
  "Return a destination for FILE in DIRECTORY that does not exist."
  (let* ((target-directory (file-name-as-directory directory))
         (name (file-name-nondirectory (directory-file-name file)))
         (destination (expand-file-name name target-directory))
         (index 1))
    (while (file-exists-p destination)
      (setq destination
            (expand-file-name
             (my/dired-clipboard--copy-name file index)
             target-directory)
            index (1+ index)))
    destination))

(defun my/dired-clipboard--paste-destination (file directory)
  "Return the paste destination for FILE in DIRECTORY."
  (pcase my-dired-clipboard-existing-file-policy
    ('rename (my/dired-clipboard--unique-destination file directory))
    (_ (expand-file-name
        (file-name-nondirectory (directory-file-name file))
        directory))))

(defun my/dired-clipboard-copy (&optional arg)
  "Copy marked files, or the file at point, to the file clipboard.
With prefix ARG, cut (move) instead of copy."
  (interactive "P" dired-mode)
  (my/dired-clipboard--ensure-not-wdired)
  (when (my/dired-clipboard--region-active-p)
    (user-error "File copy disabled while region is active"))
  (let* ((files (dired-get-marked-files nil nil nil nil "No file at point"))
         (cut-p arg)
         (operation (if cut-p 'cut 'copy)))
    (setq my-dired-clipboard--state
          (list :operation operation :files files))
    (message "%d item%s %s to clipboard"
             (length files)
             (if (= (length files) 1) "" "s")
             (if cut-p "cut" "copied"))))

(defun my/dired-clipboard-cut ()
  "Cut marked files, or the file at point, to the file clipboard."
  (interactive nil dired-mode)
  (my/dired-clipboard--ensure-not-wdired)
  (when (my/dired-clipboard--region-active-p)
    (user-error "File cut disabled while region is active"))
  (let ((files (dired-get-marked-files nil nil nil nil "No file at point")))
    (setq my-dired-clipboard--state
          (list :operation 'cut :files files))
    (message "%d item%s cut to clipboard"
             (length files)
             (if (= (length files) 1) "" "s"))))

(defun my/dired-clipboard-paste ()
  "Paste files from the clipboard into the current Dired directory."
  (interactive nil dired-mode)
  (my/dired-clipboard--ensure-not-wdired)
  (unless my-dired-clipboard--state
    (user-error "File clipboard is empty"))
  (let* ((operation (plist-get my-dired-clipboard--state :operation))
         (files (plist-get my-dired-clipboard--state :files))
         (move-p (eq operation 'cut))
         (create-function (if move-p #'dired-rename-file #'dired-copy-file))
         (description (if move-p "Move" "Paste"))
         (target (file-name-as-directory (dired-current-directory)))
         (dired-recursive-copies my-dired-clipboard-recursive-copies))
    (dired-create-files
     create-function
     description
     files
     (lambda (from)
       (my/dired-clipboard--paste-destination from target))
     my-dired-clipboard-keep-marker)
    (when move-p
      (setq my-dired-clipboard--state nil))))

(defun my/dired-clipboard--copy-binding (&optional _)
  "Return the copy command for the current Dired state, or nil.
Return nil (fall through) in WDired or when the region is active."
  (if (or (derived-mode-p 'wdired-mode)
          (my/dired-clipboard--region-active-p))
      nil
    #'my/dired-clipboard-copy))

(defun my/dired-clipboard--cut-binding (&optional _)
  "Return the cut command for the current Dired state, or nil.
Return nil (fall through) in WDired or when the region is active."
  (if (or (derived-mode-p 'wdired-mode)
          (my/dired-clipboard--region-active-p))
      nil
    #'my/dired-clipboard-cut))

(defun my/dired-clipboard--paste-binding (&optional _)
  "Return the paste command for the current Dired state, or nil.
Return nil (fall through) in WDired."
  (if (derived-mode-p 'wdired-mode)
      nil
    #'my/dired-clipboard-paste))

(defvar my-dired-clipboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-w")
      `(menu-item "" nil :filter ,#'my/dired-clipboard--copy-binding))
    (define-key map (kbd "C-S-w")
      `(menu-item "" nil :filter ,#'my/dired-clipboard--cut-binding))
    (define-key map (kbd "C-y")
      `(menu-item "" nil :filter ,#'my/dired-clipboard--paste-binding))
    map)
  "Keymap for `my-dired-clipboard-mode'.")

(define-minor-mode my-dired-clipboard-mode
  "Copy, cut and paste files in Dired with M-w, C-S-w and C-y.
M-w copies marked files to the clipboard.
C-S-w cuts marked files to the clipboard.
C-y pastes files from the clipboard into the current directory.
All three fall back to their default bindings when the region is
active or in WDired."
  :lighter nil
  :keymap my-dired-clipboard-mode-map)

(defun my/dired-sort-by-size ()
  (interactive)
  (dired-sort-other "-alGghS"))

(defun my/dired-sort-by-date ()
  (interactive)
  (dired-sort-other "-alGght"))

(defun my/dired-sort-by-name ()
  (interactive)
  (dired-sort-other "-alGgh"))

(defun my/dired-sort-by-extension ()
  (interactive)
  (dired-sort-other "-alGghX"))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook (lambda () (my-dired-clipboard-mode 1)))
  (define-key dired-mode-map (kbd "D") #'my/dired-async-do-delete)
  (define-key dired-mode-map (kbd "C") 'dired-copy-file)
  (define-key dired-mode-map (kbd "C-c d") 'my/dired-duplicate-file)
  (define-key dired-mode-map (kbd "C-c u") 'my/dired-du)
  (define-key dired-mode-map (kbd "C-c U") 'my/disk-space-query)
  (define-key dired-mode-map (kbd "b") 'my/dired-file-to-org-link)
  (define-key dired-mode-map (kbd "_") #'dired-create-empty-file)
  (define-key dired-mode-map (kbd "z") #'my/dired-do-compress)
  (define-key dired-mode-map (kbd "j") #'dired-next-line)
  (define-key dired-mode-map (kbd "k") #'dired-previous-line)
  (define-key dired-mode-map (kbd "l") #'dired-find-file)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "3") #'my/dired-sort-by-size)
  (define-key dired-mode-map (kbd "4") #'my/dired-sort-by-date)
  (define-key dired-mode-map (kbd "5") #'my/dired-sort-by-name)
  (define-key dired-mode-map (kbd "6") #'my/dired-sort-by-extension))

(defun my-dired-switch-to-destination ()
  "Switch to the destination window after copying in Dired."
  (when-let ((dest-window
              (get-window-with-predicate
               (lambda (win)
                 (with-current-buffer (window-buffer win)
                   (and (derived-mode-p 'dired-mode)
                        (not (eq win (selected-window)))))))))
    (select-window dest-window)))

(defun my/dired-sort-move-to-first-file (&rest _)
  "Move point to the first file or directory after sorting, skipping . and .."
  (goto-char (point-min))
  (dired-next-line 2)
  (while (and (not (eobp))
              (looking-at-p ".*\\.\\.?$"))
    (dired-next-line 1)))

(advice-add 'dired-sort-toggle-or-edit :after #'my/dired-sort-move-to-first-file)

(advice-add 'dired-do-copy :after (lambda (&rest _) (my-dired-switch-to-destination)))
(advice-add 'dired-do-rename :after (lambda (&rest _) (my-dired-switch-to-destination)))

;; In dired, hitting RET while inside an isearch should both exit isearch
;; and open the file at point - matches the natural muscle memory.
(defun my/dired-isearch-find-file ()
  "Exit isearch and open the file at point in dired."
  (interactive)
  (isearch-exit)
  (when (derived-mode-p 'dired-mode)
    (dired-find-file)))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "<return>") #'my/dired-isearch-find-file)
  (define-key isearch-mode-map (kbd "C-m") #'my/dired-isearch-find-file)
  (define-key isearch-mode-map (kbd "C-j") #'my/dired-isearch-find-file))

;;
;; -> visuals-core
;;
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)
(setq window-divider-default-bottom-width 2)
(setq window-divider-default-right-width 2)
(setq window-divider-default-places t)
(window-divider-mode -1)
(defvar my/internal-border-width 0 "Default internal border width for toggling.")
(modify-all-frames-parameters `((internal-border-width . ,my/internal-border-width)))
(set-fringe-mode '(20 . 20))
(setq bookmark-set-fringe-mark nil)
(setq bookmark-fringe-mark nil)

(when (fboundp 'my/rainbow-mode)
  (add-hook 'prog-mode-hook #'my/rainbow-mode)
  (add-hook 'org-mode-hook #'my/rainbow-mode)
  (add-hook 'conf-space-mode-hook #'my/rainbow-mode))

;; Slight transparency on the frame background. Harmless in TTY/Wayland
;; environments that don't support it.
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;
;; -> imenu-core
;;
(defun my-imenu-create-index ()
  "Create an index using definitions starting with ';; ->'."
  (let ((index-alist '())
        (regex "^;;[[:space:]]->\\(.+\\)$"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((name (string-trim (match-string 1)))
              (pos (match-beginning 0)))
          (push (cons name (set-marker (make-marker) pos)) index-alist))))
    (setq imenu--index-alist (sort
                              index-alist
                              (lambda (a b)
                                (string< (car a) (car b)))))))
;;
;; (setq imenu-create-index-function #'my-imenu-create-index)
;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (setq imenu-sort-function 'imenu--sort-by-name)
            (setq imenu-generic-expression
                  '(
                    ;; Match comment-based section markers: ;; -> Section Name
                    (nil "^;;[[:space:]]+-> \\(.*\\)$" 1)
                    ;; Match function definitions
                    (nil "^\\s-*(defun\\s-+\\([^( \t\n]+\\)" 1)
                    ))
            (imenu-add-menubar-index)))

(add-hook 'conf-space-mode-hook
          (lambda ()
            (setq imenu-sort-function 'imenu--sort-by-name)
            (setq imenu-generic-expression
                  '((nil "^#[[:space:]]+-> \\(.*\\)$" 1)))
            (imenu-add-menubar-index)))

;;
;; -> recentf-core
;;
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)

(defun my/fido-recentf (arg)
  "Use fido to select from recently opened files.
With universal argument, use the traditional recentf-open-files interface."
  (interactive "P")
  (if arg
      (recentf-open-files)
    (find-file (completing-read "Recent file: " recentf-list nil t nil 'recentf-list))))

;;
;; -> grep-core
;;
(eval-after-load 'grep
  '(progn
     (dolist (dir '("nas" ".cache" "cache" "elpa" "chromium" ".local/share" "syncthing" ".mozilla" ".local/lib" "Games"))
       (push dir grep-find-ignored-directories))
     (dolist (file '(".cache" "*cache*" "*.iso" "*.xmp" "*.jpg" "*.mp4"))
       (push file grep-find-ignored-files))
     ))

;;
;; -> gdb-core
;;
(setq gdb-display-io-nopopup 1)
(setq gdb-many-windows t)
(global-set-key (kbd "<f9>") 'gud-break)
(global-set-key (kbd "<f10>") 'gud-next)
(global-set-key (kbd "<f11>") 'gud-step)

;;
;; -> compilation-core
;;
(setq compilation-always-kill t)
(setq compilation-context-lines 3)
(setq compilation-scroll-output t)
;; ignore warnings
(setq compilation-skip-threshold 2)
(global-set-key (kbd "<f5>") (if (fboundp 'project-compile) 'project-compile 'compile))

;;
;; -> diff-core
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-highlight-all-diffs t)
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-prepare-buffer-hook
          (if (fboundp 'outline-show-all) #'outline-show-all #'show-all))
(add-hook 'ediff-prepare-buffer-hook (lambda () (visual-line-mode -1)))

(defun my/ediff-dired-directories ()
  "Compare two directories using Ediff, seeded from visible Dired buffers.

Dir A defaults to the current Dired buffer; Dir B to another visible
Dired buffer.  Confirm or edit each at the prompt.

Passes a prefix argument to prompt for an optional file filter regexp."
  (interactive)
  (let* ((dirs (delq nil
                     (mapcar (lambda (w)
                               (with-selected-window w
                                 (when (eq major-mode 'dired-mode)
                                   (dired-current-directory))))
                             (window-list-1 nil 'visible t))))
         (dir-a (read-directory-name "Dir A: " (car dirs) (car dirs) t))
         (dir-b (read-directory-name "Dir B: " (cadr dirs) (cadr dirs) t))
         (regexp (when current-prefix-arg
                   (read-string "File filter regexp (empty for all): "))))
    (ediff-directories
     (file-name-as-directory dir-a)
     (file-name-as-directory dir-b)
     (and (stringp regexp) (not (string-blank-p regexp)) regexp))))

;;
;; -> project-core
;;
(require 'project)
(defun project-root-safe ()
  "Return the project root or nil if unavailable."
  (if (fboundp 'project-root)
      ;; Use project-root if available (Emacs 29+)
      (when-let ((project (project-current)))
        (project-root project))
    ;; Compatibility for Emacs < 29
    (when-let ((project (project-current)))
      (car (project-roots project)))))

(defun my/project-create-compilation-search-path ()
  "Populate the 'compilation-search-path' variable.
With directories under project root using find."
  (interactive)
  (let ((find-command
         (concat "find " (project-root-safe)
                 " \\( -path \\*/.local -o -path \\*/.config -o
 -path \\*/.svn -o -path \\*/.git -o -path \\*/nas \\) -prune -o
 -type d -print")))
    (setq compilation-search-path
          (split-string
           (shell-command-to-string find-command)
           "\n" t))))

(setq project-vc-extra-root-markers '(".top"))

;;
;; -> xref-core
;;
(require 'xref)

(defvar my/xref-selection-cache (make-hash-table :test 'equal)
  "Persistent cache of remembered xref choices keyed by project+identifier.
Values are plists of the form (:file F :line L :column C :summary S);
live xref-item objects would not round-trip through `savehist'.")

(defvar my/xref-current-identifier nil
  "Identifier currently being resolved by `xref--find-definitions'.")

(defun my/xref--cache-key (identifier)
  (cons (or (and (fboundp 'project-current)
                 (when-let* ((proj (project-current)))
                   (expand-file-name (project-root proj))))
            default-directory)
        identifier))

(defun my/xref--serialize (xref)
  "Convert XREF to a printable plist for persistence."
  (let ((loc (xref-item-location xref)))
    (list :file    (xref-location-group loc)
          :line    (ignore-errors (xref-location-line loc))
          :summary (xref-item-summary xref))))

(defun my/xref--matching-item (cached fresh)
  "Return the item in FRESH whose location matches CACHED plist, else nil."
  (when cached
    (let ((c-file (plist-get cached :file))
          (c-line (plist-get cached :line)))
      (seq-find (lambda (x)
                  (let ((loc (xref-item-location x)))
                    (and (equal c-file (xref-location-group loc))
                         (equal c-line (ignore-errors (xref-location-line loc))))))
                fresh))))

;; Live-preview state. Dynamically bound around completing-read so the
;; minibuffer post-command-hook can see them.
(defvar my/xref--preview-items nil
  "Alist of (display-string . xref-item) for the current preview session.")
(defvar my/xref--preview-origin nil
  "List (WINDOW BUFFER POINT WINDOW-START) captured before the prompt.")
(defvar my/xref--preview-last nil
  "Last xref previewed; used to skip redundant redraws.")
(defvar my/xref--preview-timer nil
  "Pending idle timer for the next preview update.")

(defcustom my/xref-preview-delay 0.25
  "Idle seconds before previewing the highlighted xref candidate.
Set to 0 for immediate preview on every keystroke."
  :type 'number
  :group 'xref)

(defun my/xref--build-items (xrefs)
  "Build a unique (display . xref) alist with project-relative paths."
  (let* ((proj-root (or (and (fboundp 'project-current)
                             (when-let* ((p (project-current)))
                               (expand-file-name (project-root p))))
                        default-directory))
         (relativize (lambda (path)
                       (if (and proj-root (file-name-absolute-p path))
                           (file-relative-name path proj-root)
                         path)))
         (entries (mapcar (lambda (x)
                            (let* ((loc (xref-item-location x))
                                   (group (funcall relativize
                                                   (xref-location-group loc)))
                                   (line (ignore-errors (xref-location-line loc))))
                              (cons (if line (format "%s:%d" group line) group)
                                    x)))
                          xrefs))
         (head-width (apply #'max 0
                            (mapcar (lambda (e) (length (car e))) entries)))
         (head-fmt (format "%%-%ds  %%s" (+ head-width 2)))
         (seen (make-hash-table :test 'equal))
         items)
    (dolist (e entries)
      (let* ((head (car e))
             (x (cdr e))
             (summary (xref-item-summary x))
             (label (format head-fmt head summary))
             (uniq label)
             (i 2))
        (while (gethash uniq seen)
          (setq uniq (format "%s (%d)" label i) i (1+ i)))
        (puthash uniq t seen)
        (push (cons uniq x) items)))
    (nreverse items)))

(defun my/xref--preview-update ()
  "Preview the currently-highlighted candidate in the origin window."
  (when (and (minibufferp) my/xref--preview-items my/xref--preview-origin)
    (let* ((cand (ignore-errors (car (completion-all-sorted-completions))))
           (xref (and cand (cdr (assoc cand my/xref--preview-items)))))
      (when (and xref (not (eq xref my/xref--preview-last))
                 (window-live-p (car my/xref--preview-origin)))
        (setq my/xref--preview-last xref)
        (let ((loc (xref-item-location xref)))
          (with-selected-window (car my/xref--preview-origin)
            (let ((marker (ignore-errors (xref-location-marker loc))))
              (when (markerp marker)
                 (switch-to-buffer (marker-buffer marker) t t)
                 (goto-char marker)
                 (recenter)))))))))

(defun my/xref--preview-schedule ()
  "Reschedule the preview to fire after `my/xref-preview-delay' idle seconds."
  (when (timerp my/xref--preview-timer)
    (cancel-timer my/xref--preview-timer))
  (setq my/xref--preview-timer
        (run-with-idle-timer my/xref-preview-delay nil
                             #'my/xref--preview-update)))

(defun my/xref--preview-cleanup ()
  "Cancel any pending preview and restore the origin window state."
  (when (timerp my/xref--preview-timer)
    (cancel-timer my/xref--preview-timer)
    (setq my/xref--preview-timer nil))
  (pcase my/xref--preview-origin
    (`(,win ,buf ,pt ,start)
     (when (and (window-live-p win) (buffer-live-p buf))
       (with-selected-window win
         (switch-to-buffer buf t t)
         (goto-char pt)
          (set-window-start win start))))))

(defun my/xref--completing-read-with-preview (xrefs)
  "Prompt for one of XREFS with live preview in the origin window.
Returns the chosen xref-item, or nil if cancelled."
  (let* ((items (my/xref--build-items xrefs))
         (win (selected-window))
         (my/xref--preview-items items)
         (my/xref--preview-origin
          (list win (window-buffer win) (window-point win) (window-start win)))
         (my/xref--preview-last nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook #'my/xref--preview-schedule nil t)
          (add-hook 'minibuffer-exit-hook #'my/xref--preview-cleanup nil t))
      (let ((sel (completing-read
                  "Definition: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata (category . xref-item))
                      (complete-with-action action items string pred)))
                  nil t)))
        (cdr (assoc sel items))))))

(defun my/xref-show-definitions-remember (fetcher alist)
  "Memoizing + previewing replacement for `xref-show-definitions-function'.
First pick for an identifier is prompted via completing-read with live
preview; subsequent calls for the same identifier+project jump straight
to the remembered xref. Choices persist via `savehist'. If the cached
location no longer appears in the fresh results (line drift, file moved)
we fall through to the prompt."
  (let* ((xrefs (funcall fetcher))
         (id my/xref-current-identifier)
         (key (and id (my/xref--cache-key id)))
         (cached (and key (gethash key my/xref-selection-cache)))
         (remembered (my/xref--matching-item cached xrefs))
         (display-action (assoc-default 'display-action alist)))
    (cond
     ;; Zero or one candidate - let the default handler do its thing.
     ((or (null xrefs) (null (cdr xrefs)))
      (xref-show-definitions-completing-read (lambda () xrefs) alist))
     ;; Cached pick still valid - jump directly, no prompt.
     (remembered
      (xref-pop-to-location remembered display-action))
     ;; Prompt with live preview, then record and jump.
     (t
      (let ((chosen (my/xref--completing-read-with-preview xrefs)))
        (when chosen
          (when key
            (puthash key (my/xref--serialize chosen) my/xref-selection-cache))
          (xref-pop-to-location chosen display-action)))))))

;; Capture the identifier so the show-function can key its cache on it.
;; `xref--find-definitions' is the common funnel for M-. and friends.
(defun my/xref--capture-identifier (orig-fn id &rest args)
  (let ((my/xref-current-identifier id))
    (apply orig-fn id args)))

(advice-add 'xref--find-definitions :around #'my/xref--capture-identifier)

(defun my/dumb-jump-forget (arg)
  "Forget the remembered xref for the symbol at point.
With a prefix argument, forget every remembered xref."
  (interactive "P")
  (if arg
      (progn (clrhash my/xref-selection-cache)
             (message "Cleared all remembered xref choices"))
    (let* ((id (or (thing-at-point 'symbol t)
                   (xref--read-identifier "Forget definition of: ")))
           (key (my/xref--cache-key id)))
      (if (remhash key my/xref-selection-cache)
          (message "Forgot remembered xref for %s" id)
        (message "No remembered xref for %s" id)))))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/xref-selection-cache))

(setq xref-show-definitions-function #'my/xref-show-definitions-remember
      xref-show-xrefs-function       #'xref-show-definitions-buffer)

;;
;; -> indentation-core
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun indent-whole-buffer ()
  "Indent the entire buffer without affecting point or mark."
  (interactive)
  (save-excursion
    (save-restriction
      (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c i") 'indent-whole-buffer)

;;
;; -> shell-core
;;
(defun my/shell-create (name)
  "Create a custom-named eshell buffer with NAME."
  (interactive "sName: ")
  (eshell 'new)
  (let ((new-buffer-name (concat "*eshell-" name "*")))
    (rename-buffer new-buffer-name t)))

(defun my/ansi-term-create (name)
  "Create a custom-named ansi-term buffer with NAME."
  (interactive "sName: ")
  (let* ((shell-prog (or explicit-shell-file-name shell-file-name "/bin/bash"))
         (temp-name (replace-regexp-in-string
                     "\\`\\*\\|\\*\\'" ""
                     (generate-new-buffer-name "*ansi-term-temp*")))
         (buf (ansi-term shell-prog temp-name)))
    (with-current-buffer buf
      (rename-buffer (generate-new-buffer-name (concat "*ansi-term-" name "*"))))))

(setq eshell-scroll-to-bottom-on-input t)
(add-hook 'eshell-mode-hook (lambda () (setq-local tab-always-indent 'complete)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(defvar-local my/ansi-term--last-mode nil
  "Cache of last known term mode to avoid redundant mode-line updates.")

(defun ansi-term-update-mode-line ()
  "Update the mode-line to show whether `ansi-term` is in character or line mode."
  (let ((current-mode (term-in-char-mode)))
    (unless (eq current-mode my/ansi-term--last-mode)
      (setq my/ansi-term--last-mode current-mode)
      (setq mode-name
            (if current-mode
                "Ansi-Term [Char]"
              "Ansi-Term [Line]"))
      (force-mode-line-update))))

(defun enable-ansi-term-mode-line-indicator ()
  "Enable dynamic mode-line indicator for `ansi-term` modes."
  (add-hook 'post-command-hook #'ansi-term-update-mode-line nil t))

(defun disable-ansi-term-mode-line-indicator ()
  "Disable dynamic mode-line indicator for `ansi-term` modes."
  (remove-hook 'post-command-hook #'ansi-term-update-mode-line t))

(add-hook 'term-mode-hook #'enable-ansi-term-mode-line-indicator)
(add-hook 'term-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook #'disable-ansi-term-mode-line-indicator nil t)))

(defun my/shell-menu ()
  "Menu for Shell commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "--- Shell Commands [q] Quit: ---
[e] eshell
[s] shell
[a] ansi-term"

               'face 'minibuffer-prompt))))
    (pcase key
      (?e (call-interactively 'my/shell-create))
      (?s (call-interactively 'shell))
      (?a (call-interactively 'my/ansi-term-create))
      ;; Quit
      (?q (message "Quit Shell menu."))
      (?\C-g (message "Quit Shell menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

;;
;; -> tab-bar-core
;;
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-new-tab-to 'rightmost)
(setq tab-bar-close-button-show nil)
(setq tab-bar-auto-width-max '((120) 20))

;;
;; -> windows-specific-core
;;
(when (eq system-type 'windows-nt)
  (let* ((bin (expand-file-name "bin" user-emacs-directory))
         (sys (getenv "SystemRoot"))
         (xPaths
          `(,bin
            ,(concat bin "/PortableGit/bin")
            ,(concat bin "/PortableGit/usr/bin")
            ,(concat bin "/netcoredbg")
            ,(concat bin "/csharp-ls/tools/net9.0/any")
            ,(concat bin "/hunspell/bin")
            ,(concat bin "/find")
            ,(concat bin "/clang/bin")
            ,(concat bin "/cmake/bin")
            ,(concat bin "/protoc")
            ,(concat bin "/ada_language_server/bin")
            ,(concat bin "/buf/bin")
            ,(concat bin "/kotlin-language-server/bin")
            ,(concat bin "/jdtls/bin")
            ,(concat bin "/ImageMagick-7.1.2-27-portable-Q16-x64")
            ,(concat bin "/ffmpeg-7.1.1-essentials_build/bin")
            ,(concat sys "/System32")
            ,(concat sys "/System32/Wbem")
            ,(concat sys "/System32/WindowsPowerShell/v1.0/")
            ,(concat sys "/System32/OpenSSH/")))
         (sysPath (getenv "PATH")))
    (setenv "PATH" (concat (mapconcat #'identity xPaths ";") ";" sysPath))
    (setq exec-path (append xPaths (split-string sysPath ";") (list "." exec-directory)))))

;;
;; -> programming-core
;;
;;

;; import shell variables
(defun import-shell-environment (env-var)
  "Import a specific ENV-VAR from the shell and set it inside Emacs."
  (let ((value (shell-command-to-string
                (concat "bash --login -c 'echo $" env-var "'"))))
    (when value
      (setenv env-var value)
      (when (string= env-var "PATH") ;; Special handling for PATH
        (setq exec-path (split-string value path-separator))))))

(import-shell-environment "PATH")

;; org babel
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

(defun my/eglot-dir-locals ()
  "Create .dir-locals.el file for eglot ada-mode using the selected DIRED path."
  (interactive)
  (add-dir-local-variable
   'ada-mode
   'eglot-workspace-configuration
   `((ada . (:projectFile ,(dired-get-filename))))))

(setq vc-handled-backends
      (append (and (executable-find "svn") '(SVN)) '(Git)))

(defun my/vc-svn-registered-silent (orig-fun file)
  "Silently return nil when `svn' command fails.
Prevents `vc-do-command' errors (e.g. status 53) from blocking
Dired or other VC operations on SVN working copies that svn
cannot handle."
  (condition-case nil
      (funcall orig-fun file)
    (error nil)))
(advice-add 'vc-svn-registered :around #'my/vc-svn-registered-silent)

(global-set-key (kbd "C-c l") 'my/selective-display-fold)

(defun my/selective-display-fold (&optional level)
  "Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(defun concatenate-project-files (root-dir output-file &optional exclude-patterns)
  "Traverse ROOT-DIR recursively and concatenate all text files into OUTPUT-FILE.
Each file is prefixed with a header showing its relative path.
EXCLUDE-PATTERNS is an optional list of regex patterns to exclude files/directories."
  (interactive "DRoot directory: \nFOutput file: ")
  (let ((default-exclude '("\\.git/" "node_modules/" "\\.DS_Store" "\\.pyc$" "\\.o$" "\\.so$" "\\.gitconfig"
                           "\\.jpg$" "\\.jpeg$" "\\.png$" "\\.gif$" "\\.pdf$" "\\.zip$" "\\.csproj" "\\.gitignore"
                           "\\.tar$" "\\.gz$" "\\.exe$" "\\.dll$" "\\.class$")))
    (setq exclude-patterns (append default-exclude exclude-patterns))
    
    (with-temp-buffer
      (insert (format "=== PROJECT CONCATENATION ===\n"))
      (insert (format "Root Directory: %s\n" root-dir))
      (insert (format "Generated: %s\n\n" (current-time-string)))
      
      (let ((file-count 0))
        (dolist (file (concatenate-project-files--get-text-files root-dir exclude-patterns))
          (let ((relative-path (file-relative-name file root-dir)))
            (insert (format "\n" ))
            (insert (format "=== FILE: %s ===\n" relative-path))
            (insert (format "\n"))
            
            (condition-case err
                (progn
                  (insert-file-contents file)
                  (goto-char (point-max))
                  (insert "\n")
                  (setq file-count (1+ file-count)))
              (error 
               (insert (format "ERROR: Could not read file - %s\n" (error-message-string err)))))))
        
        (insert (format "\n=== END OF CONCATENATION ===\n"))
        (insert (format "Total files processed: %d\n" file-count)))
      
      (write-region (point-min) (point-max) output-file)
      (message "Concatenated %d files from %s to %s" 
               (length (concatenate-project-files--get-text-files root-dir exclude-patterns))
               root-dir output-file))))

(defun concatenate-project-files--get-text-files (dir exclude-patterns)
  "Get all text files in DIR recursively, excluding files matching EXCLUDE-PATTERNS."
  (let ((files '()))
    (dolist (file (directory-files-recursively dir ".*" nil))
      (unless (concatenate-project-files--should-exclude-p file exclude-patterns)
        (when (concatenate-project-files--is-text-file-p file)
          (push file files))))
    (sort files 'string<)))

(defun concatenate-project-files--should-exclude-p (file exclude-patterns)
  "Check if FILE should be excluded based on EXCLUDE-PATTERNS."
  (catch 'excluded
    (dolist (pattern exclude-patterns)
      (when (string-match pattern file)
        (throw 'excluded t)))
    nil))

(defun concatenate-project-files--is-text-file-p (file)
  "Check if FILE is likely a text file by examining its content."
  (and (file-regular-p file)
       (file-readable-p file)
       (> (nth 7 (file-attributes file)) 0) ; file size > 0
       (condition-case nil
           (with-temp-buffer
             (insert-file-contents file nil 0 1024) ; read first 1KB
             (not (string-match-p "[\000-\010\016-\037]" (buffer-string))))
         (error nil))))

;; Convenience function for git projects
(defun concatenate-git-project (git-root output-file)
  "Concatenate all text files in a git project, respecting .gitignore patterns."
  (interactive "DGit project root: \nFOutput file: ")
  (let ((gitignore-patterns (concatenate-project-files--parse-gitignore 
                             (expand-file-name ".gitignore" git-root))))
    (concatenate-project-files git-root output-file gitignore-patterns)))

(defun concatenate-project-files--parse-gitignore (gitignore-file)
  "Parse .gitignore file and return list of regex patterns."
  (when (file-exists-p gitignore-file)
    (with-temp-buffer
      (insert-file-contents gitignore-file)
      (let ((patterns '()))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
            (when (and (> (length line) 0)
                       (not (string-prefix-p "#" line)))
              ;; Convert gitignore pattern to regex
              (let ((pattern (replace-regexp-in-string "\\*" ".*" line)))
                (push pattern patterns))))
          (forward-line 1))
        patterns))))

;;
;; -> flymake-core
;;
(setq flymake-show-diagnostics-at-end-of-line nil)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-N") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-P") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "<f8>") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "<S-f8>") #'flymake-goto-prev-error)

  (defun my/flymake--diag-buffer ()
    "Return the visible flymake diagnostics buffer, or nil."
    (seq-some (lambda (b)
                (and (with-current-buffer b
                       (derived-mode-p 'flymake-diagnostics-buffer-mode))
                     (get-buffer-window b)
                     b))
              (buffer-list)))

  (defvar my/flymake--sync-overlay nil
    "Overlay used to highlight the current entry in the diagnostics buffer.")

  (defun my/flymake-sync-diagnostics ()
    "Highlight the diagnostics buffer entry matching the error at point."
    (when-let* ((buf (my/flymake--diag-buffer))
                (win (get-buffer-window buf))
                (diag (or (car (flymake-diagnostics (point)))
                          (car (flymake-diagnostics (line-beginning-position)
                                                    (line-end-position))))))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (let ((id (tabulated-list-get-id)))
                (if (and (listp id) (eq (plist-get id :diagnostic) diag))
                    (setq found (point))
                  (forward-line 1))))
            (when found
              (unless (overlayp my/flymake--sync-overlay)
                (setq my/flymake--sync-overlay (make-overlay 1 1))
                (overlay-put my/flymake--sync-overlay 'face 'highlight)
                (overlay-put my/flymake--sync-overlay 'priority 100))
              (move-overlay my/flymake--sync-overlay
                            found
                            (min (point-max) (1+ (line-end-position)))
                            buf)
              (set-window-point win found)))))))

  (define-minor-mode my/flymake-follow-mode
    "Sync the diagnostics buffer to the error at point."
    :lighter nil
    (if my/flymake-follow-mode
        (add-hook 'post-command-hook #'my/flymake-sync-diagnostics nil t)
      (remove-hook 'post-command-hook #'my/flymake-sync-diagnostics t)))

  (add-hook 'flymake-mode-hook #'my/flymake-follow-mode)

  (defun my/flymake-toggle-diagnostics ()
    "Toggle the flymake diagnostics buffer."
    (interactive)
    (let ((buf (my/flymake--diag-buffer)))
      (if buf
          (quit-window nil (get-buffer-window buf))
        (flymake-show-buffer-diagnostics)
        (my/flymake-sync-diagnostics))))
  (define-key flymake-mode-map (kbd "M-M") #'my/flymake-toggle-diagnostics))

;;
;; -> simply-annotate-search-map
;;
(with-eval-after-load 'simply-annotate
  (set-keymap-parent simply-annotate-command-map search-map))

;;
;; -> development-core
;;
(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (abort-recursive-edit))))

(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)

(add-to-list 'display-buffer-alist
             '("\\*my-rg-results"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

(defun without-gc (&rest args)
  (let ((gc-cons-threshold most-positive-fixnum))
    (apply args)))

(setq ispell-personal-dictionary (concat user-emacs-directory "Emacs-vanilla/my-dictionary"))

(define-key help-map (kbd "=") #'describe-char)
(define-key help-map (kbd "j") #'describe-face)
(define-key help-map (kbd "-") #'describe-keymap)

(defun show-file-path ()
  "Show the full path of the current buffer's file in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key (kbd "C-c p") 'show-file-path)

;;
;; -> dwim
;;
(when (file-exists-p "/home/jdyer/bin/category-list-uniq.txt")
  (progn
    (defvar my/dwim-convert-commands
      '("ConvertNoSpace" "AudioConvert" "AudioInfo" "AudioNormalise"
        "AudioTrimSilence" "PictureAutoColour" "PictureConvert"
        "PictureCrush" "PictureFrompdf" "PictureInfo" "PictureMontage"
        "PictureOrganise" "PictureCrop" "PictureRotateFlip" "PictureEmail"
        "PictureUpdateFromCreateDate" "VideoTag2XMP"
        "PictureRotateLeft" "PictureRotateRight" "PictureScale"
        "PictureUpscale" "PictureGetText" "PictureOrientation"
        "PictureUpdateToCreateDate" "VideoConcat" "VideoConvert" "VideoConvertToGif"
        "VideoCut" "VideoDouble" "VideoExtractAudio" "VideoExtractFrames"
        "VideoFilter" "VideoFromFrames" "VideoInfo" "VideoRemoveAudio"
        "VideoReplaceVideoAudio" "VideoRescale" "VideoReverse"
        "VideoRotate" "VideoRotateLeft" "VideoRotateRight" "VideoShrink"
        "VideoSlowDown" "VideoSpeedUp" "VideoZoom" "WhatsAppConvert"
        "PictureCorrect" "Picture2pdf" "PictureTag" "PictureTagRename" "PictureTagAndRenameEmacs"
        "OtherTagDate" "VideoRemoveFlips" "PictureFixWhatsApp")
      "List of commands for dwim-convert.")

    (defvar my/org-dired-marked-files nil
      "Stores the current dired marked files.")

    (defun my/read-lines (file-path)
      "Return a list of lines of a file at FILE-PATH."
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t)))

    (defun my/dwim-convert-generic-menu (command)
      "Execute a dwim-shell-command-on-marked-files with the given COMMAND."
      (let* ((unique-text-file "/home/jdyer/bin/category-list-uniq.txt")
             (user-selection nil)
             (files my/org-dired-marked-files)
             (command-and-files (concat command " " (mapconcat 'identity files " "))))
        (prin1 files)
        (when (string= command "PictureTag")
          (setq user-selection (completing-read "Choose an option: "
                                                (my/read-lines unique-text-file)
                                                nil t)))
        (async-shell-command (if user-selection
                                 (concat command " " user-selection " " (mapconcat 'identity files " "))
                               (concat command " " (mapconcat 'identity files " ")))
                             "*convert*")))
    ;; (save-buffers-kill-terminal))

    (defun my/dwim-convert-with-selection-files-command (files-string chosen-command)
      "Prompt user to choose command and execute dwim-shell-command-on-marked-files."
      (interactive)
      (setq my/org-dired-marked-files (split-string files-string ";" t))
      (my/dwim-convert-generic-menu chosen-command))

    (defun my/dwim-convert-with-selection-files (files-string)
      "Prompt user to choose command and execute dwim-shell-command-on-marked-files."
      (interactive)
      (setq my/org-dired-marked-files (split-string files-string ";" t))
      (let ((chosen-command (completing-read "Choose command: "
                                             my/dwim-convert-commands)))
        (my/dwim-convert-generic-menu chosen-command)))
    
    (global-set-key (kbd "C-c v")
                    (lambda ()
                      (interactive)
                      (let ((files (my/get-files-from-context)))
                        (when files
                          (let ((files-string (mapconcat 'identity files ";")))
                            (my/dwim-convert-with-selection-files files-string))))))))

(defun my/image-dired-get-original-files ()
  "Get original file paths from image-dired marked thumbnails or current thumbnail."
  (when (fboundp 'dired-image-thumbnail-get-marked)
    (dired-image-thumbnail-get-marked)))

(defun my/get-files-from-context ()
  "Get files based on current context (dired, image-dired, etc.)"
  (cond
   ;; In image-dired thumbnail mode
   ((eq major-mode 'image-dired-thumbnail-mode)
    (my/image-dired-get-original-files))
   
   ;; In dired mode
   ((eq major-mode 'dired-mode)
    (dired-get-marked-files))
   
   ;; In image-dired display mode
   ((eq major-mode 'image-dired-display-image-mode)
    (list (image-dired-original-file-name)))
   
   ;; Default: try to get current file
   (t
    (when buffer-file-name
      (list buffer-file-name)))))

(defun my/universal-picture-tag ()
  "Tag pictures from any context (dired or image-dired)."
  (interactive)
  (let ((files (my/get-files-from-context)))
    (if files
        (let ((files-string (mapconcat 'identity files ";")))
          (my/dwim-convert-with-selection-files-command files-string "PictureTag"))
      (message "No files found to tag"))))

(defun my/universal-picture-tag-rename ()
  "Tag and rename pictures from any context (dired or image-dired)."
  (interactive)
  (let ((files (my/get-files-from-context)))
    (if files
        (let ((files-string (mapconcat 'identity files ";")))
          (my/dwim-convert-with-selection-files-command files-string "PictureTag")
          (my/dwim-convert-with-selection-files-command files-string "PictureTagRename"))
      (message "No files found to tag and rename"))))

(defun my/picture-tag-rename-and-update ()
  "Run PictureTag, PictureTagRename, and PictureUpdateFromCreateDate in sequence."
  (interactive)
  (let* ((files (my/get-files-from-context))
         (files-string (when files (mapconcat 'identity files ";"))))
    (if files-string
        (progn
          ;; First, tag the pictures
          (message "Step 1/3: Tagging pictures...")
          (my/dwim-convert-with-selection-files-command files-string "PictureTag")
          (sit-for 1)  ; Brief pause between operations
          
          ;; Then rename based on tags
          (message "Step 2/3: Renaming pictures...")
          (my/dwim-convert-with-selection-files-command files-string "PictureUpdateFromCreateDate")

          (sit-for 1)
          
          ;; Finally update dates
          (message "Step 3/3: Updating dates from CreateDate...")
          (my/dwim-convert-with-selection-files-command files-string "PictureTagRename")
          
          (message "All operations completed!"))
      (message "No files found to process"))))

(define-key my-overrides-mode-map (kbd "C-t t") 'my/picture-tag-rename-and-update)

;; Set up keybindings for both dired and image-dired
(defun my/setup-picture-keybindings ()
   "Set up consistent keybindings for picture operations."
   (define-key my-overrides-mode-map (kbd "C-t t") 'my/picture-tag-rename-and-update))


;; Apply to both modes
(add-hook 'dired-mode-hook 'my/setup-picture-keybindings)
(add-hook 'image-dired-thumbnail-mode-hook 'my/setup-picture-keybindings)

;; Optional: Add menu items
(defun my/add-picture-menu-items ()
  "Add picture tagging items to the menu."
  (when (featurep 'easymenu)
    (easy-menu-add-item
     nil '("Tools")
     ["Tag Pictures" my/universal-picture-tag
      :help "Tag selected pictures"]
     "Games")
    (easy-menu-add-item
     nil '("Tools")
     ["Tag and Rename Pictures" my/universal-picture-tag-rename
      :help "Tag and rename selected pictures"]
     "Games")))

(add-hook 'dired-mode-hook 'my/add-picture-menu-items)
(add-hook 'image-dired-thumbnail-mode-hook 'my/add-picture-menu-items)

;;
;; -> image-mode-dimensions
;;

(defun my/image-mode-show-dimensions ()
  "Display the open image's original pixel dimensions and file size in the header line."
  (when (and (derived-mode-p 'image-mode)
             buffer-file-name
             (file-exists-p buffer-file-name))
    (condition-case err
        (let* ((image (or (image-get-display-property)
                          (create-image buffer-file-name)))
               (display-size (image-size image t))
               (display-width (car display-size))
               (display-height (cdr display-size))
               (original-image (create-image buffer-file-name nil nil :scale 1))
               (original-size (image-size original-image t))
               (original-width (car original-size))
               (original-height (cdr original-size))
               (bytes (file-attribute-size
                       (file-attributes buffer-file-name))))
          (setq header-line-format
                (format " %d x %d px%s   %s"
                        original-width original-height
                        (if (or (/= original-width display-width)
                                (/= original-height display-height))
                            (format " (displayed %d x %d px)" display-width display-height)
                          "")
                        (file-size-human-readable bytes))))
      (error
       (setq header-line-format
             (format " image dimensions unavailable: %S" err))))))

(add-hook 'image-mode-hook #'my/image-mode-show-dimensions)
(add-hook 'image-mode-new-window-functions
          (lambda (&rest _) (my/image-mode-show-dimensions)))

;;
;; -> publishing-core
;;
(defun my/export-menu ()
  "Menu for Export/Publishing commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "--- Export Commands [q] Quit: ---
[w] Export to HTML (with table highlighting)
[d] Export to DOCX (via ODT)"
               'face 'minibuffer-prompt))))
    (pcase key
      (?w (progn
            (org-html-export-to-html)
            (my/html-promote-headers)
            (my/html-org-table-highlight)
            (my/html-flush-divs)))
      (?d (progn
            (org-odt-export-to-odt)
            (async-shell-command
             (concat "libreoffice --headless --convert-to docx "
                     (file-name-with-extension
                      (file-name-nondirectory (buffer-file-name))
                      "odt")) "*create-docs*")))
      ;; Quit
      (?q (message "Quit Export menu."))
      (?\C-g (message "Quit Export menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c e") 'my/export-menu)

;;
;; -> gh-release-core
;;
(defvar my/gh-release-buffer "*GitHub Releases*"
  "Buffer name for displaying GitHub releases.")

(defvar-local my/gh-release--header-timer nil)

(defun my/gh-release--set-header-line (buf msg face &optional clear-after)
  "Set BUF's header-line to colored MSG with FACE.
When CLEAR-AFTER (seconds) is given, clear header-line after that delay."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when my/gh-release--header-timer
        (cancel-timer my/gh-release--header-timer))
      (setq header-line-format
            (concat (propertize " ● " 'face face)
                    (propertize msg 'face face)
                    (propertize " ●" 'face face)))
      (force-mode-line-update t)
      (when clear-after
        (setq my/gh-release--header-timer
              (run-with-timer
               clear-after nil
               (lambda (b)
                 (when (buffer-live-p b)
                   (with-current-buffer b
                     (setq header-line-format nil
                           my/gh-release--header-timer nil))))
               buf))))))

(defun my/gh-release--ensure-gh ()
  "Check that gh CLI is available."
  (unless (executable-find "gh")
    (user-error "GitHub CLI (gh) not found in PATH")))

(defun my/gh-release--ensure-repo ()
  "Check that we are in a git repo."
  (unless (locate-dominating-file default-directory ".git")
    (user-error "Not in a git repository")))

(defun my/gh-release--tag-at-point ()
  "Extract the release tag from the current line.
The gh release list format is: TITLE\\tLATEST\\tTAG\\tDATE."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (let ((fields (split-string line "\t")))
      (when (>= (length fields) 3)
        (string-trim (nth 2 fields))))))

(defun my/gh-release-list ()
  "List GitHub releases for the current repository."
  (interactive)
  (my/gh-release--ensure-gh)
  (my/gh-release--ensure-repo)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (with-current-buffer (get-buffer-create my/gh-release-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "GitHub Releases" 'face 'bold) "\n")
        (insert (propertize (format "Repository: %s\n\n"
                                    (string-trim
                                     (shell-command-to-string
                                      "gh repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null || git remote get-url origin")))
                            'face 'font-lock-comment-face))
        (let ((output (shell-command-to-string
                       "gh release list --limit 30 2>&1")))
          (if (string-match-p "no releases" output)
              (insert "No releases found.\n")
            (let ((lines (split-string output "\n" t))
                  rows cols)
              (dolist (line lines)
                (push (split-string line "\t") rows))
              (setq rows (nreverse rows))
              (setq cols (apply #'max (mapcar #'length rows)))
              (let* ((widths (make-vector cols 0)))
                (dolist (row rows)
                  (dotimes (i (length row))
                    (aset widths i (max (aref widths i)
                                        (length (nth i row))))))
                (aset widths 1 0)
                (dolist (row rows)
                  (dotimes (i (length row))
                    (insert (propertize
                             (format (format "%%-%ds  " (aref widths i))
                                     (nth i row))
                             'face (if (= i 0) 'bold 'default))))
                   (insert "\n"))))))
        (goto-char (point-min)))
      (my/gh-release-mode)
      (pop-to-buffer (current-buffer)))))

(defun my/gh-release-browse ()
  "Open the release at point in a browser."
  (interactive)
  (let ((tag (my/gh-release--tag-at-point)))
    (if tag
        (shell-command (format "gh release view %s --web" (shell-quote-argument tag)))
      (user-error "No release tag found on this line"))))

(defun my/gh-release-view ()
  "View release details at point in a buffer."
  (interactive)
  (let ((tag (my/gh-release--tag-at-point)))
    (if tag
        (let* ((default-directory (locate-dominating-file default-directory ".git"))
               (output (shell-command-to-string
                        (format "gh release view %s 2>&1" (shell-quote-argument tag)))))
          (with-current-buffer (get-buffer-create "*GitHub Release View*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert output)
              (goto-char (point-min)))
            (special-mode)
            (pop-to-buffer (current-buffer))))
      (user-error "No release tag found on this line"))))

(defun my/gh-release-delete ()
  "Delete the release at point after confirmation."
  (interactive)
  (let ((tag (my/gh-release--tag-at-point)))
    (if tag
        (when (yes-or-no-p (format "Delete release %s? " tag))
          (message "Deleting release %s..." tag)
          (shell-command-to-string
           (format "gh release delete %s --yes --cleanup-tag 2>&1"
                   (shell-quote-argument tag)))
          (message "Deleted release %s" tag)
          (my/gh-release-list))
      (user-error "No release tag found on this line"))))

(defun my/gh-release-create (tag title notes files)
  "Create a new GitHub release.
TAG is the release tag, TITLE is the release title,
NOTES is the release notes text, and FILES is a list of file paths to attach."
  (interactive
   (let* ((default-file (when (derived-mode-p 'dired-mode)
                           (dired-get-filename nil t)))
          (tag (read-string "Release tag (e.g. v1.0): "))
          (title (read-string "Release title: " tag))
          (notes (read-string "Release notes: " ""))
          (files-str (read-string "Files to attach (space-separated, empty for none): "
                                  (when default-file
                                    (file-name-unquote default-file))))
          (files (if (string-empty-p files-str) nil
                   (mapcar #'expand-file-name (split-string files-str)))))
     (list tag title notes files)))
  (my/gh-release--ensure-gh)
  (my/gh-release--ensure-repo)
  (let* ((default-directory (locate-dominating-file default-directory ".git"))
         (git-root default-directory)
         (file-args (mapconcat (lambda (f) (shell-quote-argument (expand-file-name f)))
                                files " "))
         (cmd (format "gh release create %s --title %s --notes %s %s 2>&1"
                      (shell-quote-argument tag)
                      (shell-quote-argument title)
                      (shell-quote-argument notes)
                      file-args))
         (rel-buf (or (get-buffer my/gh-release-buffer)
                      (current-buffer)))
         (output-buffer "*gh-release-create*"))
    (my/gh-release--set-header-line rel-buf
      (format "Creating release %s..." tag) 'warning)
    (message "Creating release %s..." tag)
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer))
    (set-process-sentinel
     (start-process-shell-command "gh-release-create" output-buffer cmd)
     (lambda (proc event)
       (cond
        ((string-match "finished" event)
         (my/gh-release--set-header-line rel-buf
           (format "✓ Release %s created" tag) 'success 6)
         (message "Release %s created successfully" tag)
         (when (buffer-live-p rel-buf)
           (let ((default-directory git-root))
             (my/gh-release-list))))
        ((string-match "exited abnormally" event)
         (let* ((err-text (with-current-buffer output-buffer
                            (buffer-substring (point-min) (line-end-position))))
                (reason (if (string-match ": \\(.+\\)" err-text)
                            (match-string 1 err-text)
                          (string-trim (car (split-string err-text "\n"))))))
           (my/gh-release--set-header-line rel-buf
             (format "✗ %s: %s" tag (truncate-string-to-width reason 40 nil nil t))
             'error 8)
           (message "Release %s failed — %s" tag reason)
           (display-buffer output-buffer))))))))

(defun my/gh-release-download ()
  "Download assets from the release at point."
  (interactive)
  (let ((tag (my/gh-release--tag-at-point)))
    (if tag
        (let* ((default-directory (locate-dominating-file default-directory ".git"))
               (dir (read-directory-name "Download to: " default-directory))
               (rel-buf (or (get-buffer my/gh-release-buffer)
                            (current-buffer))))
          (my/gh-release--set-header-line rel-buf
            (format "Downloading assets for %s..." tag) 'warning)
          (message "Downloading assets for %s..." tag)
          (set-process-sentinel
           (start-process-shell-command
            "gh-release-download" "*gh-release-download*"
            (format "gh release download %s --dir %s 2>&1"
                    (shell-quote-argument tag)
                    (shell-quote-argument dir)))
           (lambda (proc event)
             (cond
              ((string-match "finished" event)
               (my/gh-release--set-header-line rel-buf
                 (format "✓ Downloaded assets for %s" tag) 'success 6)
               (message "Download complete for %s" tag))
              ((string-match "exited abnormally" event)
               (my/gh-release--set-header-line rel-buf
                 (format "✗ Download failed for %s" tag) 'error 6)
               (message "Download failed for %s" tag))))))
      (user-error "No release tag found on this line"))))

(defun my/gh-release-edit ()
  "Edit the release at point (title and notes)."
  (interactive)
  (let ((tag (my/gh-release--tag-at-point)))
    (if tag
        (let* ((default-directory (locate-dominating-file default-directory ".git"))
               (title (read-string "New title (empty to keep): "))
               (notes (read-string "New notes (empty to keep): "))
               (args (concat
                      (unless (string-empty-p title)
                        (format " --title %s" (shell-quote-argument title)))
                      (unless (string-empty-p notes)
                        (format " --notes %s" (shell-quote-argument notes))))))
          (when (string-empty-p args)
            (user-error "Nothing to change"))
          (shell-command-to-string
           (format "gh release edit %s%s 2>&1" (shell-quote-argument tag) args))
          (message "Updated release %s" tag)
          (when (get-buffer my/gh-release-buffer)
            (my/gh-release-list)))
      (user-error "No release tag found on this line"))))

(defun my/gh-release-menu ()
  "Menu for GitHub release commands."
  (interactive)
  (my/gh-release--ensure-gh)
  (my/gh-release--ensure-repo)
  (let ((key (read-key
              (propertize
               "--- GitHub Releases [q] Quit: ---
[l] List Releases    [c] Create Release
[v] View Details     [e] Edit Release
[b] Browse in Browser[d] Delete Release
[w] Download Assets"
               'face 'minibuffer-prompt))))
    (pcase key
      (?l (my/gh-release-list))
      (?c (call-interactively #'my/gh-release-create))
      (?v (my/gh-release-view))
      (?e (my/gh-release-edit))
      (?b (my/gh-release-browse))
      (?d (my/gh-release-delete))
      (?w (my/gh-release-download))
      (?q (message "Quit GitHub Releases menu."))
      (?\C-g (message "Quit GitHub Releases menu."))
      (_ (message "Invalid key: %c" key)))))

(defvar my/gh-release-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'my/gh-release-create)
    (define-key map (kbd "d") #'my/gh-release-delete)
    (define-key map (kbd "v") #'my/gh-release-view)
    (define-key map (kbd "e") #'my/gh-release-edit)
    (define-key map (kbd "w") #'my/gh-release-download)
    (define-key map (kbd "g") #'my/gh-release-list)
    (define-key map (kbd "b") #'my/gh-release-browse)
    (define-key map (kbd "RET") #'my/gh-release-browse)
    map))

(define-derived-mode my/gh-release-mode special-mode "GH-Releases"
  "Major mode for viewing GitHub releases.
\\{my/gh-release-mode-map}")

(global-set-key (kbd "C-c G") 'my/gh-release-menu)

;;
;; -> core-configuration
;;
(load-file "~/.emacs.d/Emacs-DIYer/init.el")

;;
;; -> dwim
;;
;;
;; (my/sync-ui-accent-color "coral")
