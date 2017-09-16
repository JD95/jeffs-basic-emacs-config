;;; lisp.el --- Utilities for Emacs Lisp Mode

;;; Commentary:
;; 

;;; Code:

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)
	    (paredit-mode)
	    (define-key emacs-lisp-mode-map
	      "M-s" 'avy-goto-word-1)
	    ))

(global-set-key (kbd "C-c C-l") 'eval-last-sexp)

(provide 'lisp)

;;; lisp.el ends here
