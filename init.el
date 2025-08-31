;; -*- lexical-binding: t; -*-

;;
;; -> requires-core
;;
(require 'org)
(require 'grep)
(require 'bookmark)
(require 'dired)

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

;;
;; -> modeline-completion-core
;;
;; (fido-mode 1)
(fido-vertical-mode 1)
(define-key icomplete-minibuffer-map (kbd "M-RET") 'my-icomplete-exit-minibuffer-with-input)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)

;;
;; -> keys-navigation-core
;;
(defvar my-jump-keymap (make-sparse-keymap))
(global-set-key (kbd "M-l") my-jump-keymap)
(define-key my-jump-keymap (kbd "=") #'tab-bar-new-tab)
(define-key my-jump-keymap (kbd "b") (lambda () (interactive) (find-file "~/bin")))
(define-key my-jump-keymap (kbd "c") (lambda () (interactive) (find-file "~/DCIM/content/aaa--calendar.org")))
(define-key my-jump-keymap (kbd "d") (lambda () (interactive) (find-file (expand-file-name diary-file))))
(define-key my-jump-keymap (kbd "e")
            (lambda ()
              (interactive)
              (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key my-jump-keymap (kbd "g") (lambda () (interactive) (find-file "~/.config")))
(define-key my-jump-keymap (kbd "h") (lambda () (interactive) (find-file "~")))
(define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
(define-key my-jump-keymap (kbd "k")
            (lambda () (interactive)
              (find-file (concat user-emacs-directory "emacs--core.org"))))
(define-key my-jump-keymap (kbd "l") #'my/load-theme)
(define-key my-jump-keymap (kbd "m") #'customize-themes)
(define-key my-jump-keymap (kbd "o") #'bookmark-jump)
(define-key my-jump-keymap (kbd "r") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(define-key my-jump-keymap (kbd "s") (lambda () (interactive) (find-file "~/source")))
(define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
(define-key my-jump-keymap (kbd "-") #'tab-close)
(global-set-key (kbd "M-;") #'my/quick-window-jump)

;;
;; -> keys-visual-core
;;
(add-hook 'text-mode-hook 'visual-line-mode)

;;
;; -> keys-visual-core
;;
(defvar my-win-keymap (make-sparse-keymap))
(global-set-key (kbd "C-q") my-win-keymap)
(define-key my-win-keymap (kbd "b") #'(lambda () (interactive)(tab-bar-mode 'toggle)))
(define-key my-win-keymap (kbd "c") #'display-fill-column-indicator-mode)
(define-key my-win-keymap (kbd "d") #'window-divider-mode)
(define-key my-win-keymap (kbd "e") #'whitespace-mode)
(define-key my-win-keymap (kbd "f") #'font-lock-mode)
(define-key my-win-keymap (kbd "g") #'global-hl-line-mode)
(define-key my-win-keymap (kbd "h") #'font-lock-update)
(define-key my-win-keymap (kbd "l") #'my/sync-ui-accent-color)
(define-key my-win-keymap (kbd "m") #'my/load-theme)
(define-key my-win-keymap (kbd "n") #'display-line-numbers-mode)
(define-key my-win-keymap (kbd "o") #'toggle-centered-buffer)
(define-key my-win-keymap (kbd "p") #'variable-pitch-mode)
(define-key my-win-keymap (kbd "q") #'toggle-menu-bar-mode-from-frame)
(define-key my-win-keymap (kbd "r") #'my/rainbow-mode)
(define-key my-win-keymap (kbd "s") #'my/toggle-internal-border-width)
(define-key my-win-keymap (kbd "u") #'set-cursor-color)
(define-key my-win-keymap (kbd "U") #'set-foreground-color)
(define-key my-win-keymap (kbd "B") #'set-background-color)
(define-key my-win-keymap (kbd "v") #'visual-line-mode)

;;
;; -> keys-other-core
;;
(global-set-key (kbd "M-s ,") #'my/mark-line)
(global-set-key (kbd "M-H") #'my/mark-line)
(global-set-key (kbd "M-s =") #'ediff-buffers)
(global-set-key (kbd "M-s +") #'ediff-regions-linewise)
(global-set-key (kbd "M-h") #'my/mark-block)
(global-set-key (kbd "M-s j") #'eval-defun)
(global-set-key (kbd "M-s x") #'diff-buffer-with-file)
(global-set-key (kbd "M-s ;") #'my/copy-buffer-to-kill-ring)
(global-set-key (kbd "M-s /") #'my/find-file)
(global-set-key (kbd "M-s '") #'my/grep)

;;
;; -> keybinding-core
;;
(global-set-key (kbd "C-M-l") (lambda () (interactive)
                                (my/adaptive-resize t -2)))
(global-set-key (kbd "C-M-h") (lambda () (interactive)
                                (my/adaptive-resize t 2)))
(global-set-key (kbd "C-M-j") (lambda () (interactive)
                                (my/adaptive-resize nil -1)))
(global-set-key (kbd "C-M-k") (lambda () (interactive)
                                (my/adaptive-resize nil 1)))
(global-set-key (kbd "C--") (lambda ()(interactive)(text-scale-adjust -1)))
(global-set-key (kbd "C-=") (lambda ()(interactive)(text-scale-adjust 1)))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "<f12>") #'(lambda ()(interactive)(async-shell-command "do_backup home" "*backup*")))
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "M-[") #'my/shell-menu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x [") #'beginning-of-buffer)
(global-set-key (kbd "C-x ]") #'end-of-buffer)
(global-set-key (kbd "C-x k") #'kill-buffer)
(global-set-key (kbd "C-c j") #'(lambda() (interactive)(tab-bar-history-back)(my/repeat-history)))
(global-set-key (kbd "C-c k") #'(lambda() (interactive)(tab-bar-history-forward)(my/repeat-history)))
(global-set-key (kbd "C-x l") #'scroll-lock-mode)
(global-set-key (kbd "C-x s") #'save-buffer)
(global-set-key (kbd "C-x v e") 'vc-ediff)
(global-set-key (kbd "C-x x g") #'revert-buffer)
(global-set-key (kbd "C-x x t") #'toggle-truncate-lines)
(global-set-key (kbd "C-z") #'save-buffer)
(global-set-key (kbd "C-;") #'my/comment-or-uncomment)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)
(global-set-key (kbd "M-2") #'split-window-vertically)
(global-set-key (kbd "M-3") #'split-window-horizontally)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)
(global-set-key (kbd "M-e") #'dired-jump)
(global-set-key (kbd "M-g i") #'imenu)
(global-set-key (kbd "M-g o") #'org-goto)
(global-set-key (kbd "M-i") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "M-j") #'(lambda ()(interactive)(scroll-up (/ (window-height) 4))))
(global-set-key (kbd "M-k") #'(lambda ()(interactive)(scroll-down (/ (window-height) 4))))
(global-set-key (kbd "M-o") #'bookmark-jump)
(global-set-key (kbd "M-m") #'my/fido-recentf)
(global-set-key (kbd "M-u") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c U") #'my/disk-space-query)
(global-set-key (kbd "M-z") #'visual-line-mode)
(global-set-key (kbd "M-#") #'my/sync-ui-accent-color)
(global-set-key (kbd "M-]") #'my/load-theme)
(global-unset-key (kbd "C-h h"))
(global-unset-key (kbd "C-t"))
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "e") #'vc-ediff))
(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "M-j") #'nil)
  (define-key diff-mode-map (kbd "M-k") #'nil))

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

;;
;; -> bell-core
;;
(setq visible-bell t)
(setq ring-bell-function 'ignore)

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

;;
;; -> confirm-core
;;
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(set-buffer-modified-p nil)

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
 '(mode-line ((t (:height 140 :underline nil :overline nil :box nil))))
 '(mode-line-inactive ((t (:height 140 :underline nil :overline nil :box nil))))
 '(org-level-1 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-2 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-3 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-4 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-5 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-6 ((t (:inherit default :weight light :height 1.0))))
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
(defun save-macro (name)
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
                                If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
        (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))

(defun my/mark-line ()
  "Mark the current line, handling Eshell prompt if in Eshell."
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (let ((prompt-end (marker-position eshell-last-output-end)))
        (goto-char prompt-end)
        (push-mark (point) nil t)
        (end-of-line))
    (beginning-of-line)
    (push-mark (point) nil t)
    (end-of-line)))

(defun my/mark-block ()
  "Marking a block of text surrounded by a newline."
  (interactive)
  (when (not (region-active-p))
    (backward-char))
  (skip-chars-forward " \n\t")
  (re-search-backward "^[ \t]*\n" nil 1)
  (skip-chars-forward " \n\t")
  (when (not (region-active-p))
    (push-mark))
  (re-search-forward "^[ \t]*\n" nil 1)
  (skip-chars-backward " \n\t")
  (setq mark-active t))
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

(defun my/dired-du ()
  "Run 'du -hc' and count the total number of files in the directory under
  the cursor in Dired, then display the output in a buffer named *dired-du*."
  (interactive)
  (let ((current-dir (dired-get-file-for-visit)))
    (if (file-directory-p current-dir)
        (let ((output-buffer-name "*dired-du*"))
          (with-current-buffer (get-buffer-create output-buffer-name)
            (erase-buffer)
            (let* ((command (format "du -hc --max-depth=1 %s && echo && echo 'File counts per subdirectory:' && find %s -maxdepth 2 -type d -exec sh -c 'echo -n \"{}: \"; find \"{}\" -type f | wc -l' \\;"
                                    (shell-quote-argument current-dir)
                                    (shell-quote-argument current-dir)))
                   (process (start-process-shell-command "dired-du" output-buffer-name command)))
              (set-process-sentinel process 
                                    (lambda (proc event)
                                      (when (string-match "finished" event)
                                        (with-current-buffer output-buffer-name
                                          (goto-char (point-min))))))
              (pop-to-buffer output-buffer-name))))
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
           (is-dark (not (string-greaterp bg "#888888")))
           (adjusted-bg (if is-dark
                            (adjust-color bg 20)
                          (adjust-color bg -5))))
      (custom-set-faces
       `(hl-line ((t (:background ,adjusted-bg))))))))

(defun my/load-theme ()
  "Prompt to select a theme from available themes and load the selected theme."
  (interactive)
  (let ((theme (completing-read "Choose theme: " (mapcar 'symbol-name (custom-available-themes)))))
    (dolist (item custom-enabled-themes)
      (disable-theme item))
    (load-theme (intern theme) t)))

(defvar highlight-rules
  '((th . (("TODO" . "#999")))
    (td . (("\\&gt" . "#bbb")
           ("-\\&gt" . "#ccc")
           ("- " . "#ddd")
           ("- - - - " . "#eee")
           ("- - - - - - - - " . "#fff")
           ("TODO" . "#fdd")
           ("DOING" . "#ddf")
           ("DONE" . "#dfd"))))
  "Alist of elements ('th or 'td) and associated keywords/colors for row highlighting.")

(defun apply-row-style (row-start row-attributes color)
  "Apply a background COLOR to the row starting at ROW-START with ROW-ATTRIBUTES."
  (goto-char row-start)
  (kill-line)
  (insert (format "<tr%s style=\"background: %s\">\n" row-attributes color)))

(defun highlight-row-by-rules (row-start row-end row-attributes element)
  "Highlight a row based on ELEMENT ('th or 'td) keyword rules within ROW-START to ROW-END."
  (let ((rules (cdr (assoc element highlight-rules))))
    (dolist (rule rules)
      (let ((keyword (car rule))
            (color (cdr rule)))
        (when (save-excursion
                (and (re-search-forward (format "<%s.*>%s.*</%s>" element keyword element) row-end t)
                     (goto-char row-start)))
          (apply-row-style row-start row-attributes color))))))

(defun my/html-org-table-highlight ()
  "Open the exported HTML file, find tables with specific classes,
                                                        and add background styles to rows containing keywords in <td> or <th> elements."
  (interactive)
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
                  (highlight-row-by-rules row-start row-end row-attributes 'th)
                  (highlight-row-by-rules row-start row-end row-attributes 'td)))))))
      (write-region (point-min) (point-max) html-file))))

(defun my/format-to-table (&optional match properties-to-display)
  "Format Org headings into a structured alist, optionally filtered by MATCH
  and displaying only specified PROPERTIES-TO-DISPLAY (e.g., '(\"ID\" \"PRIORITY\"))."
  (interactive)
  (let ((rows '())
        (header '("TODO" "Tags" "Title" "Comments")) ;; Start without "Properties"
        (max-level 0))
    (save-excursion
      (goto-char (point-min))
      (when match (re-search-forward (format "\\*%s\\*$" (regexp-quote match)) nil t))
      ;; Add property names to the header dynamically
      (setq header (append header properties-to-display))
      (org-map-entries
       (lambda ()
         (let* ((entry (org-element-at-point))
                (heading (org-get-heading t t t t))
                (level (org-outline-level))
                (tags (remove "noexport" (org-get-tags)))
                (todo (org-get-todo-state))
                (vis-indent "- ")
                (contents "")
                (all-properties (org-entry-properties))
                (filtered-properties
                 (mapcar (lambda (prop)
                           (if (cdr (assoc prop all-properties))
                               (cdr (assoc prop all-properties))
                             ""))
                         properties-to-display)))
           (prin1 properties-to-display)
           (prin1 all-properties)
           (prin1 filtered-properties)
           (org-end-of-meta-data nil)
           (skip-chars-forward " \n\t")
           (when (eq (org-element-type (org-element-at-point)) 'paragraph)
             (let ((start (point)))
               (org-next-visible-heading 1)
               (setq contents (buffer-substring-no-properties start (point)))
               (dolist (pattern '("^#\\+begin.*" "^#\\+end.*" "\n+"))
                 (setq contents (replace-regexp-in-string pattern
                                                          (if (string= pattern "\n+") " " "")
                                                          (string-trim contents))))))
           (setq max-level (max max-level level))
           (push (append
                  (list
                   (or todo "")
                   (string-join tags ":")
                   (cond ((= level 1)
                          (concat "> " heading))
                         ((= level 2)
                          (concat "> " heading))
                         ((= level 3)
                          (concat "*> " heading "*"))
                         ((= level 4)
                          (concat "*" heading "*"))
                         (t
                          (concat "/"
                                  (mapconcat (lambda (_) vis-indent)
                                             (make-list (* (- level 4) 1) "") "") heading "/")))
                   (or contents ""))
                  filtered-properties)
                 rows)))
       nil (when match 'tree)))
    (setq rows (reverse rows))
    (push 'hline rows)
    (cons header rows)))

(defun my/html-flush-divs ()
  "Flush the divs in export to improve on Confluence import"
  (interactive)
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
  (let* ((org-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (let ((header-levels '("h1" "h2" "h3" "h4" "h5" "h6")))
        (dolist (level header-levels)
          (let* ((current-level (string-to-number (substring level 1)))
                 (new-level (max 1 (1- current-level)))  ;; Promote but don't go below h1
                 (open-tag-regex (format "<%s\\([^>]*\\)>" level))  ;; Regex for opening tag with attributes
                 (close-tag-regex (format "</%s>" level))  ;; Regex for closing tag
                 (new-open-tag (format "<h%d\\1>" new-level))  ;; Replacement for opening tag, preserving attributes
                 (new-close-tag (format "</h%d>" new-level)))  ;; Replacement for closing tag
            ;; Replace opening tags
            (goto-char (point-min))
            (while (re-search-forward open-tag-regex nil t)
              (replace-match new-open-tag))
            ;; Replace closing tags
            (goto-char (point-min))
            (while (re-search-forward close-tag-regex nil t)
              (replace-match new-close-tag)))))
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
             (setq rows (append rows (list (list (or todo "") title (or parent ""))))))))
       nil 'file))
    (setq rows (reverse rows))
    (push 'hline rows)
    (cons header rows)))

(defun my/kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end))
(global-set-key [remap kill-ring-save] 'my/kill-ring-save)

(defun my/disk-space-query ()
  "Run 'df -h' and display the output in a new buffer."
  (interactive)
  (let ((output-buffer-name "*Disk Space*"))
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

;;
;; -> window-positioning-core
;;
(add-to-list 'display-buffer-alist
             '("\\*\\(.*shell\\|.*term.*\\|eldoc.*\\*\\|Flymake.*\\)"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height . 0.3)))
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
(setq org-cycle-separator-lines 0)
(setq org-edit-src-content-indentation 0)
(setq org-tags-sort-function 'org-string-collate-greaterp)
(setq org-startup-indented t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(g)" "ORDR(o)" "SENT(s)" "|" "DONE(n)" "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(("TODO" . "#ee6273")
        ("DOING" . "#6e8baa")
        ("ORDR" . "#c96eee")
        ("SENT" . "#c86bee")
        ("DONE" . "#77aa66")
        ("CANCELLED" . "#426b3e")))
(setq org-goto-interface 'outline-path-completionp)
(setq org-outline-path-complete-in-steps nil)
(setq org-imenu-depth 1)

;;
;; -> org-agenda-core
;;
(with-eval-after-load 'org-agenda
  (setq org-agenda-include-diary t)
  (setq org-agenda-show-all-dates t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-agenda-custom-commands
        '(("m" "Month View" agenda ""
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
(setq dired-deletion-confirmer '(lambda (x) t))
(setq dired-recursive-deletes 'always)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C") 'dired-copy-file)
  (define-key dired-mode-map (kbd "C-c d") 'my/dired-duplicate-file)
  (define-key dired-mode-map (kbd "C-c u") 'my/dired-du)
  (define-key dired-mode-map (kbd "C-c U") 'my/disk-space-query)
  (define-key dired-mode-map (kbd "C-c i") 'my/image-dired-sort)
  (define-key dired-mode-map (kbd "b") 'my/dired-file-to-org-link)
  (define-key dired-mode-map (kbd "_") #'dired-create-empty-file))

(defun my-dired-switch-to-destination ()
  "Switch to the destination window after copying in Dired."
  (when-let ((dest-window
              (get-window-with-predicate
               (lambda (win)
                 (with-current-buffer (window-buffer win)
                   (and (derived-mode-p 'dired-mode)
                        (not (eq win (selected-window)))))))))
    (select-window dest-window)))

(defadvice dired-sort-toggle-or-edit (after dired-sort-move-to-first-file activate)
  "Move point to the first file or directory after sorting, skipping . and .."
  (goto-char (point-min))
  (dired-next-line 2)  ;; Skip past header and move to first entry
  (while (and (not (eobp))
              (looking-at-p ".*\\.\\.?$"))  ;; Check if line is . or ..
    (dired-next-line 1)))

(advice-add 'dired-do-copy :after (lambda (&rest _) (my-dired-switch-to-destination)))
(advice-add 'dired-do-rename :after (lambda (&rest _) (my-dired-switch-to-destination)))

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

(add-hook 'prog-mode-hook #'my/rainbow-mode)
(add-hook 'org-mode-hook #'my/rainbow-mode)
(add-hook 'conf-space-mode-hook #'my/rainbow-mode)

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
        (let ((name (s-trim (match-string 1)))
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
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)

(defun my/fido-recentf (arg)
  "Use fido to select from recently opened files.
With universal argument, use the traditional recentf-open-files interface."
  (interactive "P")
  (if arg
      (recentf-open-files)
    (find-file (completing-read "Recent file: " recentf-list nil t nil 'recentf-list))))

;;
;; -> isearch-core
;;
(defvar flex-isearch-group-size 2
  "Number of initial characters to group together for more accurate flex searching.")

(defun flex-isearch-regexp-compile (string &optional _lax)
  "Convert a search STRING to a flexible regexp.
The first `flex-isearch-group-size` chars of each word are matched literally,
and the following chars are matched flexibly.
Only add a word boundary if the string starts with a word character."
  (let* ((parts (split-string string " " t))
         (compile-part
          (lambda (part)
            (let* ((len (length part))
                   (group-size (min flex-isearch-group-size len))
                   (grouped (substring part 0 group-size))
                   (rest (substring part group-size)))
              (concat
               (regexp-quote grouped)
               (mapconcat (lambda (char)
                            (format "[^%s\n]*%s"
                                    (regexp-quote (char-to-string char))
                                    (regexp-quote (char-to-string char))))
                          rest
                          "")
               "[^-_[:alnum:]\n]*"))))
         (regexp-body (mapconcat compile-part parts "[^-_[:alnum:]\n]+")))
    ;; Only add word boundary if the first char is a word character
    (if (string-match-p "\\`[[:alnum:]_]" string)
        (concat "\\b" regexp-body)
      regexp-body)))

(defun flex-isearch-search-fun ()
  "Return the appropriate search function for flex searching."
  (if isearch-forward 'flex-isearch-forward 'flex-isearch-backward))

(defun flex-isearch-forward (string &optional bound noerror count)
  "Flex search forward for STRING."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-forward regexp bound noerror count)))

(defun flex-isearch-backward (string &optional bound noerror count)
  "Flex search backward for STRING."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-backward regexp bound noerror count)))

;; Set the search functions for isearch
;; (setq isearch-search-fun-function #'flex-isearch-search-fun)

;; Important: Set this function so isearch-occur and related commands will work
;; (setq search-default-mode #'flex-isearch-regexp-compile)

(defadvice isearch-exit (after dired-enter-directory-or-file activate)
  "In dired mode, enter directory or open file after isearch."
  (when (eq major-mode 'dired-mode)
    (let ((file (dired-get-file-for-visit)))
      (when file
        (dired-find-file)))))

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
(global-set-key (kbd "<f5>") 'my/project-compile)

;;
;; -> diff-core
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-highlight-all-diffs t)
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)
(add-hook 'ediff-prepare-buffer-hook (lambda () (visual-line-mode -1)))

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
      (cdr (project-roots project)))))

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

(setq project-vc-extra-root-markers '(".project"))

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

(setq eshell-scroll-to-bottom-on-input t)
(setq-local tab-always-indent 'complete)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(defun ansi-term-update-mode-line ()
  "Update the mode-line to show whether `ansi-term` is in character or line mode."
  (setq mode-name
        (if (term-in-char-mode)
            "Ansi-Term [Char]"
          "Ansi-Term [Line]"))
  (force-mode-line-update))

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
      (?a (call-interactively 'ansi-term))
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

;;
;; -> windows-specific-core
;;
(when (eq system-type 'windows-nt)
  (setq home-dir "c:/users/jimbo")
  (let ((xPaths
         `(,(expand-file-name "~/bin")
           ,(expand-file-name "~/bin/PortableGit/bin")
           ,(expand-file-name "~/bin/PortableGit/usr/bin")
           ,(expand-file-name "~/bin/Apache-Subversion/bin/")
           ,(expand-file-name "~/bin/svn2git-2.4.0/bin")
           ,(expand-file-name "~/bin/clang/bin")
           ,(expand-file-name "~/bin/find")
           ,(expand-file-name "~/bin/omnisharp-win-x64")
           "c:/GnuWin32/bin"
           "c:/GNAT/2021/bin")))
    (setenv "PATH" (mapconcat 'identity xPaths ";"))
    (setq exec-path (append xPaths (list "." exec-directory))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Consolas" :height 110 :weight normal))))
   '(fixed-pitch ((t ( :family "Consolas" :height 110)))))

  (setq font-general "Consolas 11")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general)))

(setq tab-bar-show 1)

;;
;; -> linux-specific-core
;;
(when (eq system-type 'gnu/linux)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "DejaVu Sans" :height 120 :weight normal))))
   '(fixed-pitch ((t ( :family "Source Code Pro" :height 110)))))
  (setq font-general "Source Code Pro 12")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general)))

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

(setq vc-handled-backends '(SVN Git))

;;
;; all-purpose build menu
;;
(defvar cmake-preset
  "build/linux/debug"
  "cmake-preset")

(defun change-directory-and-run (dir command bufname)
  "Change to DIR and run the COMMAND."
  (let ((default-directory dir))
    (async-shell-command command bufname)
    (message "Running command: %s:%s" dir command)))

(defun run-exe-command (dir exe bufname)
  "Run EXE from a specified DIR."
  (message "run-exe-command: %s:%s:%s" dir exe bufname)
  (change-directory-and-run dir exe bufname))

(defun run-cmake-command (command)
  "Run COMMAND from the top level of the project."
  (message command)
  (change-directory-and-run (project-root (project-current t)) command "*cmake*"))

(defun run-cmake-compile-command (command)
  "Run compile COMMAND from the top level of the project."
  (message command)
  (let ((default-directory (project-root (project-current t))))
    (compile command)
    (message "Running command: %s:%s" dir command)))

(defun kill-async-buffer (buffer-name)
  "Kill the async buffer with BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer)
      (message "Killed buffer: %s" buffer-name))))

(defun list-cmake-presets ()
  "List available CMake presets using `cmake --list-presets=configure`."
  (let ((output (shell-command-to-string "cmake --list-presets=configure")))
    (delq nil
          (mapcar (lambda (line)
                    (if (string-match "^\\s-+\"\\([^\"]+\\)\"\\s-*$" line)
                        (match-string 1 line)))
                  (split-string output "\n")))))

(defun transient-select-cmake-preset ()
  "Function to select a CMake preset."
  (interactive)
  (let* ((presets (list-cmake-presets))
         (preset (completing-read "Select CMake preset: " presets nil t)))
    (setq cmake-preset preset)
    (message "Selected CMake preset: %s" preset)))

(defun build-menu ()
  "Menu for Build and Diagnostic commands (Horizontal Layout)."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- Build and Diagnostic Commands [q] Quit: -------
CMake   [p] Set Preset      [c] Configure          [R] Build [i] Install
        [g] Refresh         [x] Clean              [s] List Presets
Actions [f] Toggle Flycheck [d] Show Diagnostics
Coding  [e] Eglot & Flymake [u] Undo Eglot/Flymake [h] Stop Eglot
Run     [r] All             [1] CigiDummyIG        [2] CigiMiniHost       [3] CigiMiniHostCSharp
Kill    [5] CigiDummyIG     [6] CigiMiniHost       [7] CigiMiniHostCSharp [k] All"
               'face 'minibuffer-prompt))))
    (pcase key
      ;; CMake Commands
      (?p (call-interactively 'transient-select-cmake-preset))
      (?c (run-cmake-command (format "cmake --preset %s" cmake-preset)))
      (?\r (run-cmake-compile-command (format "cmake --build --preset %s" cmake-preset)))
      (?i (run-cmake-command (format "cmake --install %s" cmake-preset)))
      (?g (run-cmake-command (format "cmake --preset %s --fresh" cmake-preset)))
      (?x (when (y-or-n-p "Are you sure you want to proceed? ")
            (run-cmake-command "rm -rf build")))
      (?s (run-cmake-command "cmake --list-presets=configure"))
      ;; Actions
      (?f (flymake-mode))
      (?d (flymake-show-buffer-diagnostics))
      ;; Coding
      (?e (progn (call-interactively 'eglot) (flymake-mode 1)))
      (?u (progn (eglot-shutdown-all) (flymake-mode -1)))
      (?h (eglot-shutdown-all))
      ;; Run Commands
      (?r (progn
            (run-exe-command
             (concat (project-root (project-current t))
                     "build/windows/debug/bin/Debug")
             "CigiDummyIG.exe" "*Running CigiDummyIG.exe*")
            (run-exe-command
             (concat (project-root (project-current t))
                     "build/windows/debug/bin/Debug")
             "CigiMiniHostCSharp.exe" "*Running CigiMiniHostCSharp.exe*")))
      (?1 (run-exe-command
           (concat (project-root (project-current t))
                   "build/windows/debug/bin/Debug")
           "CigiDummyIG.exe"
           "*Running CigiDummyIG.exe*"))
      (?2 (run-exe-command
           (concat (project-root (project-current t))
                   "build/windows/debug/bin/Debug")
           "CigiMiniHost.exe"
           "*Running CigiMiniHost.exe*"))
      (?3 (run-exe-command
           (concat (project-root (project-current t))
                   "build/windows/debug/bin/Debug")
           "CigiMiniHostCSharp.exe"
           "*Running CigiMiniHostCSharp.exe*"))
      ;; Kill Commands
      (?5 (kill-async-buffer "*Running CigiDummyIG.exe*"))
      (?6 (kill-async-buffer "*Running CigiMiniHost.exe*"))
      (?7 (kill-async-buffer "*Running CigiMiniHostCSharp.exe*"))
      (?k (progn
            (kill-async-buffer "*Running CigiDummyIG.exe*")
            (kill-async-buffer "*Running CigiMiniHost.exe*")
            (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))
      ;; Quit
      (?q (message "Quit Build menu."))
      (?\C-g (message "Quit Build menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

;;
;; coding menu
;;
(defun dotnet-run-command (command buffer-name)
  "Run a dotnet COMMAND in a new async shell BUFFER-NAME."
  (let ((default-directory (project-root (project-current t))))
    (async-shell-command command buffer-name)))

(defun dotnet-project-menu ()
  "Interactive menu for building and managing any .NET project in the current directory."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- .NET Project Menu [q] Quit -------
Build    [b] Project [0] Rebuild
Clean    [c] Clean Project
Restore  [R] Restore Dependencies
Run      [m] Run Project"
               'face 'minibuffer-prompt))))
    (pcase key
      (?b (dotnet-run-command "dotnet build" "*Dotnet Build*"))
      (?0 (dotnet-run-command "dotnet build --no-incremental" "*Dotnet Rebuild*"))
      (?c (dotnet-run-command "dotnet clean" "*Dotnet Clean*"))
      (?R (dotnet-run-command "dotnet restore" "*Dotnet Restore*"))
      (?m (dotnet-run-command "dotnet run" "*Dotnet Run*"))
      (?q (message "Quit .NET Project Menu."))
      (?\C-g (message "Quit .NET Project Menu."))
      (_ (message "Invalid key: %c" key)))))

(defun code-menu ()
  "Menu format code ."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- Coding [q] Quit: -------
Coding  [L] Enable [l] Disable
Flymake [F] Toggle [d] Diagnostics [S] Show
Eldoc   [E] Toggle [e] Buffer
Ada     [o] Other"
               'face 'minibuffer-prompt))))
    (pcase key
      ;; Actions
      (?F (call-interactively 'flymake-mode))
      (?d (flymake-show-buffer-diagnostics))
      (?S (progn
            (if flymake-show-diagnostics-at-end-of-line
                (setq flymake-show-diagnostics-at-end-of-line nil)
              (setq flymake-show-diagnostics-at-end-of-line t))))
      ;; Eglot
      (?L (progn (call-interactively 'eglot) (flymake-mode 1)))
      (?l (progn (eglot-shutdown-all) (flymake-mode -1)))
      ;; Eldoc
      (?E (global-eldoc-mode 'toggle))
      (?e (call-interactively 'eldoc-doc-buffer))
      ;; Ada
      (?o (ada-light-other-file))
      ;; Quit
      (?q (message "Quit Build menu."))
      (?\C-g (message "Quit Build menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

(defun top-code-menu ()
  "Menu format code ."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- Coding [q] Quit: -------
[a] code [s] build [d] dotnet [f] custom"
               'face 'minibuffer-prompt))))
    (pcase key
      (?a (code-menu))
      (?s (build-menu))
      (?d (dotnet-project-menu))
      (?f (build-menu))
      (?q (message "Quit Build menu."))
      (?\C-g (message "Quit Build menu."))
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "M-RET") #'top-code-menu)

(global-set-key (kbd "C-c f") 'my/selective-display-fold)

(defun my/selective-display-fold (&optional level)
  "Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

;;
;; -> ada-core
;;
(defvar ada-light-mode-keywords
  ;; https://www.adaic.org/resources/add_content/standards/05rm/html/RM-2-9.html
  '("abort" "else" "new" "return" "abs" "elsif" "not" "reverse" "abstract" "end"
    "null" "accept" "entry" "select" "access" "exception" "of" "separate"
    "aliased" "exit" "or" "subtype" "all" "others" "synchronized" "and" "for"
    "out" "array" "function" "overriding" "tagged" "at" "task" "generic"
    "package" "terminate" "begin" "goto" "pragma" "then" "body" "private" "type"
    "if" "procedure" "case" "in" "protected" "until" "constant" "interface"
    "use" "is" "raise" "declare" "range" "when" "delay" "limited" "record"
    "while" "delta" "loop" "rem" "with" "digits" "renames" "do" "mod" "requeue"
    "xor")
  "Keywords of the Ada 2012 language.")
;;
(defvar ada-light-mode--font-lock-rules
  (list (regexp-opt ada-light-mode-keywords 'symbols))
  "Rules for search-based fontification in `ada-light-mode'.
The format is appropriate for `font-lock-keywords'.")
;;
(defvar ada-light-mode-syntax-table     ; used automatically by define-derived-mode
  (let ((table (make-syntax-table)))
    ;; Comments start with "--".
    (modify-syntax-entry ?- ". 12" table)
    ;; Newlines end comments.
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    ;; Backslash is a regular symbol, not an escape character.
    (modify-syntax-entry ?\\ "_" table)
    table)
  "Syntax table used in `ada-light-mode'.")
;;
(defvar ada-light-mode-other-file-alist
  '(("\\.ads\\'" (".adb"))
    ("\\.adb\\'" (".ads")))
  "Value for `ff-other-file-alist' in `ada-light-mode'.")
;;
(defun ada-light-mode--syntax-propertize (start end)
  "Apply syntax properties to the region from START to END."
  ;; Ada delimits character literals with single quotes, but also uses the
  ;; single quote for other purposes. Since character literals are always
  ;; exactly one character long (i.e., there are no escape sequences), we can
  ;; easily find them with a regular expression and change the syntax class of
  ;; the enclosing single quotes to "generic string". This also nicely handles
  ;; the case of '"': generic string delimiters only match other generic string
  ;; delimiters, but not ordinary quote characters (i.e., the double quote).
  (goto-char start)
  (while-let ((pos (re-search-forward "'.'" end t)))
    (put-text-property (- pos 3) (- pos 2) 'syntax-table '(15))
    (put-text-property (- pos 1) pos 'syntax-table '(15))))
;;
(defvar ada-light-mode--imenu-rules
  `(("Functions"
     ,(rx bol
          (* space)
          (? (? "not" (* space)) "overriding" (* space))
          "function"
          (+ space)
          (group (+ (or word (syntax symbol)))))
     1)
    ("Procedures"
     ,(rx bol
          (* space)
          (? (? "not" (* space)) "overriding" (* space))
          "procedure"
          (+ space)
          (group (+ (or word (syntax symbol)))))
     1)
    ("Types"
     ,(rx bol
          (* space)
          (? "sub")
          "type"
          (+ space)
          (group (+ (or word (syntax symbol)))))
     1)
    ("Packages"
     ,(rx bol
          (* space)
          "package"
          (+ space)
          (group (+ (or word (syntax symbol))))
          (+ space)
          "is")
     1))
  "Imenu configuration for `ada-light-mode'.
The format is appropriate for `imenu-generic-expression'.")
;;
(defun ada-light-mode--indent-line ()
  "Indent a single line of Ada code."
  ;; This is a really dumb implementation which just indents to the most recent
  ;; non-empty line's indentation. It's better than the default though because
  ;; it stops there, so that users who want completion on TAB can get it after
  ;; indenting. (The default behavior is to insert TAB characters indefinitely.)
  (let ((indent (save-excursion
                  (beginning-of-line)
                  (if (re-search-backward "^[^\n]" nil t) ; non-empty line
                      (current-indentation)
                    0))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (when (< (current-indentation) indent)
        (save-excursion (indent-line-to indent))))))
;;
;;;###autoload
(define-derived-mode ada-light-base-mode prog-mode "AdaLBase"
  "Base mode for `ada-light-mode' and `gpr-light-mode'."
  ;; Set up commenting; Ada uses "--" followed by two spaces.
  (setq-local comment-use-syntax t
              comment-start "--"
              comment-padding 2)
  ;; Set up fontification.
  (setq-local font-lock-defaults '(ada-light-mode--font-lock-rules nil t)
              syntax-propertize-function #'ada-light-mode--syntax-propertize)
  ;; And finally, configure indentation. Since our indentation function isn't
  ;; particularly good, don't force it upon the user.
  (setq-local standard-indent 3
              tab-width 3               ; used by eglot for range formatting
              indent-line-function 'ada-light-mode--indent-line
              electric-indent-inhibit t))
;;
;;;###autoload
(define-derived-mode ada-light-mode ada-light-base-mode "AdaL"
  "Major mode for the Ada programming language.
It doesn't define any keybindings. In comparison with `ada-mode',
`ada-light-mode' is faster but less accurate."
  (setq-local ff-other-file-alist ada-light-mode-other-file-alist
              imenu-generic-expression ada-light-mode--imenu-rules))
;;
;;;###autoload
(define-derived-mode gpr-light-mode ada-light-base-mode "GPRL"
  "Major mode for GPR project files."
  :syntax-table ada-light-mode-syntax-table)
;;
;; Register the mode for Ada code following GNAT naming conventions.
;;;###autoload
(progn (add-to-list 'auto-mode-alist '("\\.ad[abcs]\\'" . ada-light-mode))
       (add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-light-mode)))
;;
;; Configure eglot if available.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((ada-light-mode :language-id "ada")
                                        "ada_language_server"))
  ;; The Ada Language Server doesn't support formatting .gpr files, but it
  ;; provides completion and detects syntax errors.
  (add-to-list 'eglot-server-programs '((gpr-light-mode :language-id "ada")
                                        "ada_language_server" "--language-gpr"))
  (defun ada-light-other-file ()
    "Jump from spec to body or vice versa using the Ada Language Server."
    (interactive)
    (if-let ((server (eglot-current-server)))
        (eglot-execute-command server
                               "als-other-file"
                               (vector (eglot--TextDocumentIdentifier)))
      (message "%s" "Not connected to the Ada Language Server")))
  ;; The "als-other-file" command used by `ada-light-other-file' requires
  ;; support for the "window/showDocument" server request in eglot; add it if
  ;; necessary.
  (unless (cl-find-method 'eglot-handle-request nil '(t (eql window/showDocument)))
    (cl-defmethod eglot-handle-request
      (_server (_method (eql window/showDocument)) &key uri &allow-other-keys)
      (find-file (eglot--uri-to-path uri))
      (list :success t)))
  ;;
  (defun ada-light-mode--current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at-p (rx (* space) eol))))
  ;;
  (defun ada-light-mode--indent-line-eglot ()
    "Indent the current line using the Ada Language Server."
    (interactive)
    (if (ada-light-mode--current-line-empty-p)
        ;; Let's not "indent" empty lines with the language server, it would
        ;; just delete them. Instead, take a guess at the required indentation
        ;; based on the most recent non-empty line.
        (indent-relative t t)
      (condition-case err
          (eglot-format (line-beginning-position) (line-end-position))
        ;; When `eglot-format' fails due to a server issue it signals the
        ;; underlying `jsonrpc-error'. In this case, let's return normally to
        ;; give completion a chance.
        (jsonrpc-error
         (when-let ((msg (alist-get 'jsonrpc-error-message (cdr err))))
           (message "Language server error: %s" msg))
         nil))))
  ;;
  (defun ada-light-mode--eglot-setup ()
    "Set up `eglot' integration for `ada-light-mode'."
    (when (eq major-mode 'ada-light-mode)
      (if (eglot-managed-p)
          (setq-local indent-line-function 'ada-light-mode--indent-line-eglot
                      electric-indent-inhibit nil)
        (setq-local indent-line-function 'ada-light-mode--indent-line
                    electric-indent-inhibit t))))
  ;;
  (add-hook 'eglot-managed-mode-hook #'ada-light-mode--eglot-setup))
;;
(provide 'ada-light-mode)

;;
;; -> development-core
;;
(global-set-key (kbd "C-c t") 'toggle-centered-buffer)
(global-set-key (kbd "M-s i") #'my/convert-markdown-clipboard-to-org)
(global-set-key (kbd "M-s u") #'my/org-promote-all-headings)

(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (abort-recursive-edit))))

(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
    The generic `keyboard-quit' does not do the expected thing when
    the minibuffer is open.  Whereas we want it to close the
    minibuffer, even without explicitly focusing it.
    The DWIM behaviour of this command is as follows:
    - When the region is active, disable it.
    - When a minibuffer is open, but not focused, close the minibuffer.
    - When the Completions buffer is selected, close it.
    - In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

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

;;
;; -> image-dired
;;
(require 'image-mode)
(require 'image-dired)

(add-to-list 'display-buffer-alist
             '("\\*image-dired\\*"
               display-buffer-in-direction
               (direction . left)
               (window . root)
               (window-width . 0.5)))

(add-to-list 'display-buffer-alist
             '("\\*image-dired-display-image\\*"
               display-buffer-in-direction
               (direction . right)
               (window . root)
               (window-width . 0.5)))

(defun my/image-dired-sort (arg)
  "Sort images in various ways given ARG."
  (interactive "P")
  ;; Use `let` to temporarily set `dired-actual-switches`
  (let ((dired-actual-switches
         (cond
          ((equal arg nil)            ; no C-u
           "-lGghat --ignore=*.xmp")
          ((equal arg '(4))           ; C-u
           "-lGgha --ignore=*.xmp")
          ((equal arg 1)              ; C-u 1
           "-lGgha --ignore=*.xmp"))))
    (let ((w (selected-window)))
      (delete-other-windows)
      (revert-buffer)
      (image-dired default-directory)
      (let ((idw (selected-window)))
        (select-window w)
        (dired-unmark-all-marks)
        (select-window idw)
        (image-dired-display-this)
        (image-dired-line-up-dynamic)))))

(setq image-use-external-converter t)
(setq image-dired-external-viewer "/usr/bin/gthumb")
(setq image-dired-show-all-from-dir-max-files 999)
(setq image-dired-thumbs-per-row 999)
(setq image-dired-thumb-relief 0)
(setq image-dired-thumb-margin 5)
(setq image-dired-thumb-size 150)

(defun my/image-save-as ()
  "Save the current image buffer as a new file."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (initial_mode major-mode)
         (counter 1)
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (write-region (point-min) (point-max) new-file nil 'no-message)
    (revert-buffer nil t nil)
    ;; (delete-file file t)
    (if (equal initial_mode 'image-dired-image-mode)
        (progn
          (image-dired ".")
          (image-dired-display-this))
      (find-file new-file t))))

(defun my/delete-current-image-and-move-to-next ()
  "Delete the current image file and move to the next image in the directory."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      (image-next-file 1)
      (delete-file current-file)
      (message "Deleted %s" current-file))))

(defun my/delete-current-image-thumbnails ()
  "Delete the current image file and move to the next image in the directory."
  (interactive)
  (let ((file-name (image-dired-original-file-name)))
    (delete-file file-name)
    (image-dired-delete-char)
    (image-dired-display-this)))

(eval-after-load 'image-mode
  '(progn
     (define-key image-mode-map (kbd "C-d") 'my/delete-current-image-and-move-to-next)
     (define-key image-mode-map (kbd "C-x C-s") 'my/image-save-as)))

(eval-after-load 'image-dired
  '(progn
     (define-key image-dired-thumbnail-mode-map (kbd "C-d") 'my/delete-current-image-thumbnails)
     (define-key image-dired-thumbnail-mode-map (kbd "n")
                 (lambda ()(interactive)(image-dired-forward-image)(image-dired-display-this)))
     (define-key image-dired-thumbnail-mode-map (kbd "p")
                 (lambda ()(interactive)(image-dired-backward-image)(image-dired-display-this)))
     ))

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
        "PictureCorrect" "Picture2pdf" "PictureTag" "PictureTagRename"
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
  (let ((files '())
        (continue t))
    (save-excursion
      (goto-char (point-min))
      ;; Move to first image if not already on one
      (condition-case nil
          (unless (image-dired-image-at-point-p)
            (image-dired-forward-image))
        (error nil))
      
      ;; Collect all marked files
      (while (and continue
                  (condition-case nil
                      (image-dired-image-at-point-p)
                    (error nil)))
        (when (condition-case nil
                  (image-dired-thumb-file-marked-p)
                (error nil))
          (let ((orig-file (condition-case nil
                               (image-dired-original-file-name)
                             (error nil))))
            (when orig-file
              (push orig-file files))))
        ;; Move to next image, stop if we can't move forward
        (let ((current-pos (point)))
          (condition-case nil
              (image-dired-forward-image)
            (error nil))
          ;; If we didn't move, we're at the end
          (when (= current-pos (point))
            (setq continue nil)))))
    
    ;; If no marked files found, get current file
    (when (and (null files) 
               (condition-case nil
                   (image-dired-image-at-point-p)
                 (error nil)))
      (let ((orig-file (condition-case nil
                           (image-dired-original-file-name)
                         (error nil))))
        (when orig-file
          (push orig-file files))))
    
    (nreverse files)))

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

;; Set up keybindings for both dired and image-dired
(defun my/setup-picture-keybindings ()
  "Set up consistent keybindings for picture operations."
  (local-set-key (kbd "C-c t") 'my/universal-picture-tag)
  (local-set-key (kbd "C-c r") 'my/universal-picture-tag-rename))

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
;; -> core-configuration
;;
(load-file "~/.emacs.d/Emacs-DIYer/init.el")

;;
;; -> dwim
;;
;;
(my/sync-ui-accent-color "coral")
