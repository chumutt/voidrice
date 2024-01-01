;;; round-number.el --- Description -*- lexical-binding: t; -*-
;; This file is not part of GNU Emacs.

(defun get-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789.-")
  (or (looking-at "[0123456789.-]+")
      (error "No number at point"))
  (string-to-number (match-string 0)))

(defun round-number-at-point-to-decimals (decimal-count)
  (interactive "NDecimal count: ")
  (let ((mult (expt 10 decimal-count)))
    (replace-match
     (number-to-string
      (/ (fround (* mult (get-number-at-point)))
         mult)))))

(provide 'get-number-at-point)
(provide 'round-number-at-point-to-decimals)
;;; round-number.el ends here
