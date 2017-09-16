;;; utilities.el --- elisp utilities
;; Utility functions

;;; Commentary:
;; 

;;; Code:

(defun system-switch (options)
  (cond ((eq system-type 'windows-nt) (nth 0 options))
	((eq system-type 'linux) (nth 1 options))
	(t (nth 2 options))))

(provide 'utilities)

;;; utilities.el ends here
