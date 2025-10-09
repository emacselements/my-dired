;; -*- lexical-binding: t; -*-
;; Author: Raoul Comninos
;; my-dired.el --- My Dired settings

(require 'dired)
(require 'dired-x)

(defun my-dired-setup ()
  "Customize dired settings."
  (dired-hide-details-mode 1)  ; Show only filenames, hide permissions/dates/sizes
  (dired-omit-mode 1))          ; Enable hiding of unwanted files
(add-hook 'dired-mode-hook #'my-dired-setup)
(setq dired-omit-files "^\\..*$") ;; Hide dotfiles (files starting with .)

;;;;;;;;;;;;;;;;;;;

(defun dired-back-to-top ()
	(interactive)
	(beginning-of-buffer)
	(dired-next-line 1))

; Replace normal "go to top" with "go to first file" (skip header line)
(define-key dired-mode-map
	(vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
	(interactive)
	(end-of-buffer)
	(dired-next-line -1))

; Replace normal "go to bottom" with "go to last file" 
(define-key dired-mode-map
	    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(setq dired-dwim-target 'dired-dwim-target-next)  ; Copy/move to other dired window automatically

(setq dired-listing-switches "-alth --group-directories-first")  ; Show all files, long format, by time, human sizes, folders first

;;;;;;;;;;;;;;;;;;;

; Load dired-narrow if available and bind to C-x / for filtering files by name
(when (require 'dired-narrow nil 'noerror)
  (global-set-key (kbd "C-x /") 'dired-narrow))

;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;

(defun my-dired-up-directory ()
  "Go up one directory in Dired mode."
  (interactive)
  (dired-up-directory))

; Make 'q' key go up one folder instead of quitting dired
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "q") 'my-dired-up-directory)))

;;;;;;;;;;;;;;;;;;;

; Setup visual feedback when editing filenames directly in dired (wdired mode)
(defvar my/wdired-before-finish-editing-hook nil)

;; Function to run the above hook
(defun my/wdired-before-finish-editing-run-hook (&rest _)
  (run-hooks 'my/wdired-before-finish-editing-hook))

;; Add advice to run the hook before finishing edits or aborting changes in `wdired`
(advice-add #'wdired-finish-edit :before #'my/wdired-before-finish-editing-run-hook)
(advice-add #'wdired-abort-changes :before #'my/wdired-before-finish-editing-run-hook)

;; Enable `highlight-changes-mode` when entering `wdired` mode
(add-hook 'wdired-mode-hook (defun my/lambda-1693716265 ()
                              (highlight-changes-mode 1)))  ; Highlight what you're changing

;; Disable `highlight-changes-mode` using our custom hook when exiting `wdired`
(add-hook 'my/wdired-before-finish-editing-hook (defun my/lambda-1693716255 ()
                                                  (highlight-changes-mode -1)))  ; Turn off highlighting when done

;;;;;;;;;;;;;;;;;;;

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
      (when (file-regular-p file)
        (push (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))
              contents)))
    (write-region (mapconcat #'identity (nreverse contents) "\n")
                  nil (concat default-directory "result.txt"))
    (message "Concatenated %d files into %sresult.txt"
             (length files) default-directory)))
(defalias 'cat 'append-marked-files)

;;;;;;;;;;;;;;;; 

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
        ;; Walk directories from deepest to shallowest
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
       (null (directory-files dir nil "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))))

;;;;;;;;;;;;;;;; 

(defun dired-rename-spaces-to-dashes-lowercase ()
  "Rename marked files or folders in Dired, replacing spaces with dashes and converting to lowercase.
When a folder is selected, only renames the folder itself, not its contents.
Reports number of items renamed."
  (interactive)
  (let ((items (dired-get-marked-files t))
        (rename-count 0))
    (if (null items)
        (message "No items marked for renaming")
      (dolist (item items)
        (when (and item (stringp item) (file-exists-p item))
          (let* ((full-path (file-truename (expand-file-name item)))  ; Resolve symlinks
                 (is-dir (file-directory-p full-path))
                 (old-name (file-name-nondirectory 
                            (if is-dir (directory-file-name full-path) full-path)))
                 (directory (file-name-directory 
                             (if is-dir (directory-file-name full-path) full-path)))
                 (new-name (downcase (replace-regexp-in-string " " "-" old-name)))
                 (new-full-path (expand-file-name new-name directory)))
            (message "Debug: Item: %s (dir? %s)" old-name is-dir)
            (message "Debug: From: %s" full-path)
            (message "Debug: To: %s" new-full-path)
            (when (and (not (string= old-name new-name))
                       (not (file-exists-p new-full-path)))
              (condition-case err
                  (progn
                    (if is-dir
                        (dired-rename-file full-path new-full-path t)  ; Use Dired's rename for dirs
                      (rename-file full-path new-full-path t))
                    (setq rename-count (+ 1 rename-count))
                    (dired-relist-file new-full-path)
                    (message "Debug: Renamed '%s' to '%s', count: %d" 
                             old-name new-name rename-count))
                (error (message "Error renaming '%s' to '%s': %s" 
                                full-path new-full-path err)))))))
      (revert-buffer)
      (message "Final count: %d item(s) renamed" rename-count))))

; Bind to "N" in Dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-rename-spaces-to-dashes-lowercase))

;;;;;;;;;;;;;;;; 

(provide 'my-dired)
