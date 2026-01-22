;; -*- lexical-binding: t; -*-
;; Author: Raoul Comninos
;; my-dired.el --- My Dired settings

;;;;;;;;;;;;;;;; 

(require 'dired)
(require 'dired-x)

(defun my-dired-setup ()
  "Customize dired settings."
  (dired-hide-details-mode 1)  ; Show only filenames, hide permissions/dates/sizes
  (dired-omit-mode 1))          ; Enable hiding of unwanted files
(add-hook 'dired-mode-hook #'my-dired-setup)
(setq dired-omit-files "^\\..*$") ;; Hide dotfiles (files starting with .)


;; Navigation Enhancements

(defun dired-back-to-top ()
	(interactive)
	(goto-char (point-min))
	(dired-next-line 1))

; Replace normal "go to top" with "go to first file" (skip header line)
(define-key dired-mode-map
	(vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
	(interactive)
	(goto-char (point-max))
	(dired-next-line -1))

; Replace normal "go to bottom" with "go to last file" 
(define-key dired-mode-map
	    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(setq dired-dwim-target 'dired-dwim-target-next)  ; Copy/move to other dired window automatically

(setq dired-listing-switches "-alth --group-directories-first")  ; Show all files, long format, by time, human sizes, folders first


;; External Tool Integration

; Load dired-narrow if available and bind to C-x / for filtering files by name
(when (require 'dired-narrow nil 'noerror)
  (global-set-key (kbd "C-x /") 'dired-narrow))


;; Custom File Opening

(defun my-dired-find-file ()
  "Open file or directory and show its canonical path."
  (interactive)
  (let ((original (dired-get-file-for-visit)))
    (if (file-directory-p original)
        (find-alternate-file (file-truename original))  ; For folders: resolve symlinks and replace current buffer
      (find-file original))))                          ; For files: open normally

; Make Enter key use our custom file opening behavior
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'my-dired-find-file))


;; ;; Mouse Click Behavior

;; (defun my-dired-mouse-visit (event)
;;   "Visit file or directory at mouse EVENT in the same window."
;;   (interactive "e")
;;   (let ((window (posn-window (event-start event)))
;;         (pos (posn-point (event-start event))))
;;     (when (windowp window)
;;       (select-window window)
;;       (goto-char pos)
;;       (my-dired-find-file))))

;; ; Bind mouse clicks in dired and disable link-following behavior
;; (with-eval-after-load 'dired
;;   ;; Disable mouse-1-click-follows-link in dired to prevent mouse-1 -> mouse-2 translation
;;   (add-hook 'dired-mode-hook
;;             (lambda ()
;;               (setq-local mouse-1-click-follows-link nil)))
;;   ;; Bind both mouse-1 and mouse-2 (single and double) to open in-place
;;   (define-key dired-mode-map [mouse-1] 'my-dired-mouse-visit)
;;   (define-key dired-mode-map [mouse-2] 'my-dired-mouse-visit)
;;   (define-key dired-mode-map [double-mouse-1] 'my-dired-mouse-visit)
;;   (define-key dired-mode-map [double-mouse-2] 'my-dired-mouse-visit))



;; Directory Navigation

(defun my-dired-up-directory ()
  "Go up one directory in Dired mode."
  (interactive)
  (dired-up-directory))

; Make 'q' key go up one folder instead of quitting dired
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "q") 'my-dired-up-directory)))



;; WDired Visual Feedback

; Setup visual feedback when editing filenames directly in dired (wdired mode)
(defvar my/wdired-before-finish-editing-hook nil)

;; Function to run the above hook
(defun my/wdired-before-finish-editing-run-hook (&rest _)
  (run-hooks 'my/wdired-before-finish-editing-hook))

;; Add advice to run the hook before finishing edits or aborting changes in `wdired`
(with-eval-after-load 'wdired
  (advice-add #'wdired-finish-edit :before #'my/wdired-before-finish-editing-run-hook)
  (advice-add #'wdired-abort-changes :before #'my/wdired-before-finish-editing-run-hook))

;; Enable `highlight-changes-mode` when entering `wdired` mode
(add-hook 'wdired-mode-hook (defun my/lambda-1693716265 ()
                              (highlight-changes-mode 1)))  ; Highlight what you're changing

;; Disable `highlight-changes-mode` using our custom hook when exiting `wdired`
(add-hook 'my/wdired-before-finish-editing-hook (defun my/lambda-1693716255 ()
                                                  (highlight-changes-mode -1)))  ; Turn off highlighting when done



;; File Concatenation

;; Concatenate marked files in Emacs Dired into a single file
(defun append-marked-files ()
  "Concatenate marked files in Dired into \"result.txt\" in the same directory."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (error "Not in Dired mode"))
  (let* ((files (dired-get-marked-files))
         (default-directory (file-name-directory (car files)))
         contents)
    (dolist (file files)
      (when (file-regular-p file)                ; Only process actual files, not directories
        (push (with-temp-buffer
                (insert-file-contents file)      ; Read entire file into memory
                (buffer-string))
              contents)))
    (write-region (mapconcat #'identity (nreverse contents) "\n")  ; Join all content with newlines
                  nil (concat default-directory "result.txt"))
    (message "Concatenated %d files into %sresult.txt"
             (length files) default-directory)))
(defalias 'cat 'append-marked-files)  ; Allow calling this function as 'cat'



;; Directory Cleanup

(defun delete-empty-dirs (root-dir &optional dry-run)
  "Find and delete empty directories under ROOT-DIR.
If DRY-RUN is non-nil, only print which directories would be deleted."
  (interactive "DSelect root directory: \nP")
  (let ((count 0)
        (root-dir (file-name-as-directory (expand-file-name root-dir)))
        (should-continue t))
    ;; Confirm with user
    (unless dry-run
      (unless (y-or-n-p (format "Delete all empty directories under %s? " root-dir))
        (user-error "Operation cancelled")))
    
    ;; Repeatedly scan until no more empty directories are found
    (while should-continue
      (let ((deleted-count 0))
        ;; Walk directories from deepest to shallowest (so we can delete parent dirs after children)
        (dolist (dir (reverse (directory-files-recursively root-dir "^" t)))
          (when (and (file-directory-p dir)
                     (directory-empty-p dir))
            (setq deleted-count (1+ deleted-count))
            (setq count (1+ count))
            (if dry-run
                (message "Would delete: %s" dir)
              (message "Deleting: %s" dir)
              (delete-directory dir))))
        ;; Stop if no directories were deleted in this pass
        (setq should-continue (> deleted-count 0))))
    
    (message "%s %d empty %s"
             (if dry-run "Found" "Deleted")
             count
             (if (= count 1) "directory" "directories"))))

(defun directory-empty-p (dir)
  "Return t if DIR is an empty directory."
  (and (file-directory-p dir)
       (null (directory-files dir nil "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))))  ; Check if no files (excluding . and ..)



;; File Renaming

; Rename files/folders: replace spaces with dashes and make lowercase
(defun dired-rename-spaces-to-dashes-lowercase ()
  "Rename marked files/folders, replacing spaces with dashes.
Converts to lowercase. Only renames folders, not their contents."
  (interactive)
  (let ((items (dired-get-marked-files t))
        (rename-count 0))
    (if (null items)
        (message "No items marked for renaming")
      (dolist (item items)
        (when (and item (stringp item) (file-exists-p item))
          (let* ((full-path (file-truename (expand-file-name item)))  ; Resolve symlinks to get real path
                 (is-dir (file-directory-p full-path))
                 (old-name (file-name-nondirectory 
                            (if is-dir (directory-file-name full-path) full-path)))
                 (directory (file-name-directory 
                             (if is-dir (directory-file-name full-path) full-path)))
                 (new-name (let ((temp (downcase (replace-regexp-in-string " +" "-" old-name))))
                             (replace-regexp-in-string "-+" "-" temp)))  ; Convert spaces to dashes, collapse multiple dashes, make lowercase
                 (new-full-path (expand-file-name new-name directory)))
            (message "Debug: Item: %s (dir? %s)" old-name is-dir)
            (message "Debug: From: %s" full-path)
            (message "Debug: To: %s" new-full-path)
            (when (and (not (string= old-name new-name))      ; Only rename if name actually changes
                       (not (file-exists-p new-full-path)))   ; Don't overwrite existing files
              (condition-case err
                  (progn
                    (if is-dir
                        (dired-rename-file full-path new-full-path t)  ; Use Dired's rename for dirs
                      (rename-file full-path new-full-path t))
                    (setq rename-count (+ 1 rename-count))
                    (dired-relist-file new-full-path)             ; Update dired display
                    (message "Debug: Renamed '%s' to '%s', count: %d" 
                             old-name new-name rename-count))
                (error (message "Error renaming '%s' to '%s': %s" 
                                full-path new-full-path err)))))))
      (revert-buffer)                                          ; Refresh the dired buffer
      (message "Final count: %d item(s) renamed" rename-count))))

; Bind the rename function to "N" key in Dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-rename-spaces-to-dashes-lowercase))

; Rename marked files to random names, preserving extensions
(defun dired-rename-to-random ()
  "Rename marked files to random names (10 chars: lowercase + digits).
Preserves file extensions."
  (interactive)
  (let ((items (dired-get-marked-files t))
        (rename-count 0))
    (if (null items)
        (message "No items marked for renaming")
      (dolist (item items)
        (when (and item (stringp item) (file-exists-p item))
          (let* ((full-path (file-truename (expand-file-name item)))
                 (is-dir (file-directory-p full-path))
                 (old-name (file-name-nondirectory
                            (if is-dir (directory-file-name full-path) full-path)))
                 (directory (file-name-directory
                             (if is-dir (directory-file-name full-path) full-path)))
                 (extension (if is-dir "" (file-name-extension old-name t)))  ; Get extension with dot
                 (new-name (concat (my-random-string 10) extension))
                 (new-full-path (expand-file-name new-name directory)))
            (message "Debug: Renaming %s to %s" old-name new-name)
            ;; Keep trying until we find a unique name
            (while (file-exists-p new-full-path)
              (setq new-name (concat (my-random-string 10) extension))
              (setq new-full-path (expand-file-name new-name directory)))
            (condition-case err
                (progn
                  (if is-dir
                      (dired-rename-file full-path new-full-path t)
                    (rename-file full-path new-full-path t))
                  (setq rename-count (+ 1 rename-count))
                  (dired-relist-file new-full-path)
                  (message "Debug: Renamed '%s' to '%s', count: %d"
                           old-name new-name rename-count))
              (error (message "Error renaming '%s' to '%s': %s"
                              full-path new-full-path err))))))
      (revert-buffer)
      (message "Final count: %d item(s) renamed to random names" rename-count))))

(defun my-random-string (length)
  "Generate a random string of LENGTH characters (lowercase letters + digits)."
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789")
        (result ""))
    (dotimes (_ length)
      (setq result (concat result (string (elt chars (random (length chars)))))))
    result))

;; Bind the random rename function to "V" key in Dired mode (replaces dired-do-run-mail)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") 'dired-rename-to-random))


;; File Type Colors

;; Customize dired faces to show different colors for file types
(custom-set-faces
 '(dired-symlink ((t (:foreground "#3b00a0")))))      ; Symlinks: purple #3b00a0

;; Highlight executable files in orange using overlays
;; This approach works with dired-hide-details-mode
(defface dired-executable
  '((t (:foreground "#b33376")))
  "Face for executable files in dired."
  :group 'dired-faces)

(defun dired-highlight-executables ()
  "Highlight executable files in green in dired using overlays."
  (when (eq major-mode 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case nil
            (let* ((file (dired-get-filename nil t)))
              (when (and file
                         (file-exists-p file)
                         (file-regular-p file)
                         (not (file-symlink-p file))
                         (file-executable-p file))
                ;; Create an overlay for this line
                (let ((ov (make-overlay (line-beginning-position)
                                        (line-end-position))))
                  (overlay-put ov 'face 'dired-executable)
                  (overlay-put ov 'evaporate t))))  ; Auto-remove when text is deleted
          (error nil))
        (forward-line 1)))))

;; Apply executable highlighting after dired loads
(add-hook 'dired-after-readin-hook #'dired-highlight-executables)


;; Mark Files Instead of Flagging for Deletion

;; Rebind ~ and # to mark files (not flag for deletion)
;; These mimic what % m ~ and % m # do - mark files matching pattern
(defun my-dired-mark-backup-files ()
  "Mark files containing ~ in their names."
  (interactive)
  (dired-mark-files-regexp "~"))

(defun my-dired-mark-autosave-files ()
  "Mark files containing # in their names."
  (interactive)
  (dired-mark-files-regexp "#"))

;; Override the default keybindings immediately (dired is already loaded above)
(define-key dired-mode-map "~" 'my-dired-mark-backup-files)
(define-key dired-mode-map "#" 'my-dired-mark-autosave-files)

(provide 'my-dired)

;; finis
