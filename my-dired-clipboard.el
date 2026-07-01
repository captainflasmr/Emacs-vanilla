;;; my-dired-clipboard.el --- Simple Dired file clipboard -*- lexical-binding: t; -*-

(require 'dired)
(require 'dired-aux)

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

(defun my/dired-clipboard-copy ()
  "Copy marked files, or the file at point, to the file clipboard."
  (interactive nil dired-mode)
  (my/dired-clipboard--ensure-not-wdired)
  (when (my/dired-clipboard--region-active-p)
    (user-error "File copy disabled while region is active"))
  (let ((files (dired-get-marked-files nil nil nil nil "No file at point")))
    (setq my-dired-clipboard--state
          (list :operation 'copy :files files))
    (message "%d item%s copied to clipboard"
             (length files)
             (if (= (length files) 1) "" "s"))))

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

(provide 'my-dired-clipboard)
;;; my-dired-clipboard.el ends here
