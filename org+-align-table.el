  (require 'cl-lib)
  (require 'org-table)
  (defun org+-search-forward-table (&optional limit noerror table-type)
    "Search for the next org table go to its end and return point.
  Set the boundaries of group 0 of the match data to the
  beginning and the end of the table.
  LIMIT and NOERROR are like for `search-forward'.
  TABLE-TYPE is the same argument as for `org-at-table-p'."
    (interactive)
    (let ((b (org-at-table-p))
	  (pt (point)))
      (unless b
	(while (and
		(setq b (re-search-forward org-table-any-line-regexp limit noerror))
		(null (org-at-table-p table-type))))
	(when
	    (and (null b) (eq noerror t))
	  (goto-char pt)))
      (when b
	(setq b (org-table-begin))
	(let ((e (org-table-end table-type)))
	(set-match-data (list b e (current-buffer)))
	(goto-char e)))))

  (defun org+-font-lock-align-table-row ()
    "Align the table column separators to the actual character columns."
    (save-excursion
      (when (org-at-table-p)
	(beginning-of-line)
	(skip-chars-forward "[[:space:]]")
	(forward-char) ;; behind leading ?|
	(let ((e (line-end-position)))
	  (while (re-search-forward "[ \t]|" e t)
	    (put-text-property (match-beginning 0) (1- (match-end 0))
			       'display
			       (list 'space :align-to (1- (current-column))))
	    )))))

  (defun org+-font-lock-align-tables (limit)
    "Align one full table."
    (when (org+-search-forward-table limit 'noerror)
      (save-match-data
	(let ((e (match-end 0)))
	  (goto-char (match-beginning 0))
	  (while (< (point) e)
	    (org+-font-lock-align-table-row)
	    (forward-line))
	  ))
      (point)))

  (defvar org+-align-table-mode-keywords nil)
  (setq org+-align-table-mode-keywords '((org+-font-lock-align-tables)))

  (define-minor-mode org+-align-table-mode
    "Set up the alignment of table columns."
    t nil nil
    (cl-pushnew 'display font-lock-extra-managed-props)
    (cl-assert (derived-mode-p 'org-mode) "`org+-align-table-mode' is for org-mode only ")
    (if org+-align-table-mode
	(font-lock-add-keywords
	 nil
	 org+-align-table-mode-keywords
	 t)
      (font-lock-remove-keywords
       nil
       org+-align-table-mode-keywords))
    (font-lock-flush)
    (font-lock-ensure))
