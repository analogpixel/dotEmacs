;;
;; Move to the begining of the word and switch the case
;;
(defun toggleCaseWord ()
  (interactive)
  (setq currentPoint (point))
  (forward-word -1)
  (setq a (char-to-string (char-after (point))))
  (delete-region (+ (point) 1)  (point))
  (if (string= (upcase a) a) (insert (downcase a)) (insert (upcase a)))
  (goto-char currentPoint)
)

;;
;; Toggle the case of the current letter
;;
(defun toggleCase ()
  (interactive)
  (setq a (char-to-string (char-after (point))))
  (delete-region (+ (point) 1)  (point))
  (if (string= (upcase a) a) (insert (downcase a)) (insert (upcase a)))
	(forward-char -1)
	)
