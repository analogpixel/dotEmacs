(defun lineup (c)
	(interactive "sChar to Align to:")
	(setq max 0)
	(setq end   (line-number-at-pos (region-end) ))
	(setq start (line-number-at-pos (region-beginning) ))

	(goto-line start)
	(while (< (line-number-at-pos) end)
		(if (search-forward c (line-end-position) t nil)
				(if (< max (current-column)) (setq max (current-column))))
		(forward-line 1)
	)

	(goto-line start)
	(message (format "%d" max))
	(setq max (- max 1))

	(while (< (line-number-at-pos) end)
		(if (search-forward c (line-end-position) t nil)
				(progn
					(forward-char -1)
					(setq pad (- max (current-column)))
					(while (> pad 0)
						(insert " ")
						(setq pad (- pad 1))
						)
					)
			)
		(forward-line 1)
  )
)