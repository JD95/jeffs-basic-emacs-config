;;; editor.el --- Utilities for editing in all modes

;;; Commentary:
;; 

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(smooth-scrolling-mode)
(elmacro-mode)

;; Themes and Colors ------------------------------------
(load-theme 'gotham t)

;; Style Enforcement
(setq whitespace-style '(face
			 empty
			 lines-tail
			 trailing
			 indentation
			 ))


;; WINDOW CONTROLS --------------------------------------
(require 'evil)
(evil-mode 1)

;; Allows for shift+arrow to move between frames

;; Shrink or grow windows
(global-set-key (kbd "C-,") #'shrink-window-horizontally)
(global-set-key (kbd "C-.") #'enlarge-window-horizontally)
(global-set-key (kbd "C-{") #'shrink-window)
(global-set-key (kbd "C-}") #'enlarge-window)

(global-set-key (kbd "C-M-;") 'elmacro-show-last-macro)

;; Move using avy search
;;(global-set-key (kbd "M-s") 'avy-goto-word-1)

(bind-keys*
 ("M-s" . avy-goto-word-1)
 ("C-h" . windmove-left)
 ("C-l" . windmove-right)
 ("C-k" . windmove-up)
 ("C-j" . windmove-down)
)

;; Completion
;;(require 'helm-config)
;;(global-set-key (kbd "M-x") nil)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(projectile-mode)

;; Swap horizontal and vertical splits
(defun toggle-window-split ()
  "Change the window split from horizontal to vertical."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; For creating new projects

(setq current-projects-directory "E:/Current_Project/")

(setq default-org-header "#+STARTUP: hidestars\n")

(defun new-project ()
  "Create a new org mode project."
  (interactive)
  (let* ((project-name (read-string "project name:"))
	(project-folder (concat current-projects-directory project-name))
	(project-overview (concat project-folder "/overview.org"))
	)
    (make-directory project-folder)
    (append-to-file default-org-header nil project-overview)
    (find-file project-overview)
  )
)

;; Utilities

 (defun insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))
;; File Encryption

(require 'epa-file)
    (epa-file-enable)

(defun epg--list-keys-1 (context name mode)
  "A fix for the epa bug.
Argument CONTEXT Not sure.
Argument NAME Not sure.
Argument MODE Not sure."
  (let ((args (append (if (epg-context-home-directory context)
			  (list "--homedir"
				(epg-context-home-directory context)))
		      '("--with-colons" "--no-greeting" "--batch"
			"--with-fingerprint" "--with-fingerprint")
		      (unless (eq (epg-context-protocol context) 'CMS)
			'("--fixed-list-mode"))))
	(list-keys-option (if (memq mode '(t secret))
			      "--list-secret-keys"
			    (if (memq mode '(nil public))
				"--list-keys"
			      "--list-sigs")))
	(coding-system-for-read 'binary)
	keys string field index)
    (if name
	(progn
	  (unless (listp name)
	    (setq name (list name)))
	  (while name
	    (setq args (append args (list list-keys-option (car name)))
		  name (cdr name))))
      (setq args (append args (list list-keys-option))))
    (with-temp-buffer
      (apply #'call-process
	     (epg-context-program context)
	     nil (list t nil) nil args)
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
	(setq keys (cons (make-vector 15 nil) keys)
	      string (match-string 0)
	      index 0
	      field 0)
	(while (and (< field (length (car keys)))
		    (eq index
			(string-match "\\([^:]+\\)?:" string index)))
	  (setq index (match-end 0))
	  (aset (car keys) field (match-string 1 string))
	  (setq field (1+ field))))
      (nreverse keys))))

;; Buffer Menu items
(global-set-key "\C-x\C-b" 'buffer-menu)

(defun general-workspace ()
  "Set up a general split screen workspace."
  (interactive)
  (delete-other-windows nil)
  (split-window-below nil)
  (split-window-right nil)
  (other-window 1)
  (other-window 1)
  (eshell nil))

(defun single-window-workspace ()
  "Change to a single window workspace."
  (interactive)
  (delete-other-windows nil)
  (neotree-hide))

(provide 'editor)

;;; editor.el ends here
