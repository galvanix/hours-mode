;; Copyright 2003 Jim Radford

(setq hours-time  "\\<\\([0-9][0-9]?:[0-9][0-9]\\(?:am\\|pm\\)\\)\\>"
      hours-date  "\\<\\(\\(20[0-9][0-9]\\)[-/]\\([0-9][0-9]\\)?[-/]\\([0-9][0-9]?\\)\\)\\>"
      hours-day   "\\<\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)\\>"
      hours-hours "\\<\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\>"
      hours-tags  ":\\([^:]+\\):")
(setq hours-separator " - ")
(setq hours-separator-re " +- +")
(setq hours-partial-interval
      (concat hours-time hours-separator-re hours-time))
(setq hours-interval
      (concat hours-partial-interval " +" hours-hours))
(setq hours-date-day
      (concat hours-date " +" hours-day))
(setq hours-outline-regexp
      (concat "\\(^\\(?:Invoice +\\)?" hours-date "\\)"))
(setq hours-prefix "")
(setq hours-invoice-prefix "")
(setq hours-round-minutes 15)
(setq hours-possible-tags
      (concat "\\(?: +" hours-tags "\\)?"))

(defun hours-check-date (date)
  (hours-date-is-legal-p (hours-date-from-string date)))
(defun hours-check-day (date day)
  (setq date (hours-date-from-string date))
  (and (hours-date-is-legal-p date)
       (do-unless (equal day (calendar-day-name date 3))
         (message "%s should be %s." day (calendar-day-name date 3)))))
(defun hours-get-interval (time1 time2)
    (let* ((interval (- (hours-time-in-minutes time2) (hours-time-in-minutes time1))))
      (if (<= interval 0) (setq interval (+ interval (* 24 60))))
	  interval))

(defun hours-check-interval (time1 time2 hours)
    (let* ((minutes (* (string-to-number hours) 60))
           (interval (hours-get-interval time1 time2)))
      (do-unless (= interval minutes)
        (message "%s should be %s." (/ (float minutes) 60) (/ (float interval) 60)))))

(setq hours-font-lock-keywords
      `(("^\\S *#.*$" . font-lock-comment-face)
        (,hours-date-day
         (1 (if (save-match-data
                  (hours-check-date (match-string 1)))
                hours-interval-face
              font-lock-warning-face))
         (5 (if (hours-check-day (match-string 1) (match-string 5))
                hours-interval-face
              font-lock-warning-face)))
        (,hours-interval
         (1 hours-interval-face)
         (2 hours-interval-face)
         (3 (if (hours-check-interval (match-string 1) (match-string 2) (match-string 3))
                hours-interval-face
              font-lock-warning-face)))
        (,(concat "^\\(Invoice\\>\\).*\\(\\<Hours\\).*\\(\\<Days\\)")
         (1 hours-invoice-face)
         (2 hours-invoice-face)
         (3 hours-invoice-face))
        (,hours-date
         (1 (if (hours-check-date (match-string 0))
                hours-date-face
              font-lock-warning-face)))
        (,hours-day     . hours-day-face)
        (,hours-time    . hours-time-face)
        (,(concat "\\(?:" hours-interval "\\|" "^Invoice.* +Hours" "\\)" " +\\(:\\)")
         (4 font-lock-keyword-face)
         ("\\([^:,]+\\)\\([,:]\\)" nil nil (1 font-lock-string-face)
                                           (2 font-lock-keyword-face)))))

(setq hours-font-lock-defaults
      '(hours-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(easy-mmode-defmap hours-mode-map
  `(("\C-cd" . hours-insert-current-date)
	("\C-ct" . hours-insert-current-date-time)
	("\C-c\C-c" . (lambda () (interactive) (hours-end-day) (hours-compute)))
	("\C-c\C-t" . (lambda () (interactive) (hours-end-day) (hours-compute)))
	("\C-c=" . hours-compute)
    ("\C-ci" . hours-invoice)
    ("\M-\r" . hours-toggle-entry)
    ("\C-c`" . hours-next-error))
  "Keymap for `hours-mode'.")

(define-derived-mode hours-mode text-mode "Hours"
  "Major mode for viewing/editing hours files."
  (set (make-local-variable 'font-lock-defaults) hours-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) hours-outline-regexp))

;; (define-minor-mode hours-minor-mode
;;   "Hours mode for viewing/editing time cards in hours format.
;; \\{hours-minor-mode-map}"
;;   nil " Hours" nil)

(defun plist-keys (plist)
  "Return the keys of PLIST that have non-null values, in order."
  (assert (zerop (mod (length plist) 2)) t)
  (loop for (key value . rest) on plist by #'cddr
        unless (or (null value) (memq key accu)) collect key into accu
        finally (return accu)))

(defun hours-invoice () (interactive)
  (let (total error-location (bound (point)))
    (save-excursion
      (while  ; a line that doesn't start with a space should be an entry
          (and (or (eq total nil)
                   (memq 'open (mapcar (lambda (e) (or (stringp e) (car e))) total)))
               (re-search-backward "^[^# \t\n]" nil 'noerror)
               (do-unless
                   (and
                    (goto-char (match-beginning 0))
                    (cond ((looking-at (concat "^Invoice\\>.*\\<Hours" hours-possible-tags))
                           (let ((tags (split-string (or (match-string 1) "") ",")))
                             (dolist (tag tags t)
                               (if (not (eq 'closed (car (lax-plist-get total tag))))
                                   (setq total (lax-plist-put total tag (list 'closed
                                                                              (cadr (lax-plist-get total tag))
                                                                              (caddr (lax-plist-get total tag)))))))))
                          ((looking-at (concat hours-date-day " +" hours-interval hours-possible-tags))
                           (save-match-data (hours-check-day (match-string 1) (match-string 5)))
                           (save-match-data (hours-check-interval (match-string 6) (match-string 7) (match-string 8)))
                           (let ((tags (save-match-data (split-string (or (match-string 9) "") ","))))
                             (dolist (tag tags t)
                               (if (not (eq 'closed (car (lax-plist-get total tag))))
                                   (setq total (lax-plist-put total tag (list 'open
                                                                              (+ (or (cadr (lax-plist-get total tag)) 0)
                                                                                 (/ (float (string-to-number (match-string 8)))
                                                                                    (length tags)))
                                                                              (+ (or (caddr (lax-plist-get total tag)) 0)
                                                                                 (/ 1.0
                                                                                    (length tags)))
                                                                              ))))))
                           )))
                 (setq error-location (point))))))
    (when error-location
      (goto-char error-location)
      (error "Illegal entry found" ))
    (dolist (tag (plist-keys total))
      (do-unless (eq nil (cadr (lax-plist-get total tag)))
        (insert hours-invoice-prefix "Invoice ") (hours-insert-current-date) (insert (format " %s Hours on %d Days"
                                                                                             (cadr (lax-plist-get total tag))
                                                                                             (caddr (lax-plist-get total tag)))
                                                                                     (if (eq tag "")
                                                                                         ""
                                                                                       (concat " :" tag ":")) "\n")))))

(defun hours-end-day () (interactive)
  (let (error-location (bound (point)))
    (save-excursion
	  (do-unless
		  (and (re-search-backward "^[^# \t\n]")
			   (goto-char (match-beginning 0))
			   (looking-at (concat hours-date-day " +" (concat hours-time hours-separator-re)))
			   (goto-char (match-end 0))
			   (or (hours-insert-current-time) t))
		(setq error-location (point))))
	(when error-location
	  (goto-char error-location)
	  (error "Illegal entry found" ))))

(defun hours-compute () (interactive)
  (let (error-location (bound (point)))
    (save-excursion
	  (do-unless
		  (and (re-search-backward "^[^# \t\n]")
			   (goto-char (match-beginning 0))
			   (looking-at (concat hours-date-day " +" hours-partial-interval))
			   (save-match-data (hours-check-day (match-string 1) (match-string 5)))
			   (goto-char (match-end 0))
			   (save-match-data (or (insert (format " %s" (/ (float (hours-get-interval (match-string 6) (match-string 7))) 60))) t)))
		(setq error-location (point))))
	(when error-location
	  (goto-char error-location)
	  (error "Illegal entry found" ))))

(defun hours-date-is-legal-p (date)
  (and date (calendar-date-is-valid-p date)))

(require 'diary-lib)
(defun hours-time-in-minutes (string)
  (let* ((military (diary-entry-time string))
         (hours (/ military 100))
         (mins  (% military 100)))
    (+ (* hours 60) mins)))

(defmacro do-unless (cond &rest body)
  "Eval BODY forms if COND is nil and return cond" 
  `(let ((p ,cond))
    (unless p 
      ,@body)
    p))
(put 'do-unless 'lisp-indent-function 1)

(require 'calendar)

;; (defun hours-date-at-point ()
;;   (if (looking-at hours-date)
;;       (list (string-to-int (match-string 3))
;;             (string-to-int (match-string 4))
;;            (string-to-int (match-string 2)))))

(defun hours-date-from-string (string)
  (if (string-match hours-date string)
      (list (string-to-int (match-string 3 string))
            (string-to-int (match-string 4 string))
            (string-to-int (match-string 2 string)))))

(defun hours-format-date (date)
  (format "%d/%02d/%02d"
          (nth 2 date) (nth 0 date) (nth 1 date)))

;;(calendar-current-date)

(defun hours-insert-current-date () (interactive)
  (insert (format-time-string "%Y/%m/%d %a ")))

(defun hours-insert-current-time () (interactive)
  (let* ((mil (diary-entry-time (format-time-string "%H:%M")))
		 (m (% mil 100))
		 (r (* (floor (/ (+ (float m) (/ (float hours-round-minutes) 2)) hours-round-minutes)) hours-round-minutes))
		 (h (% (+ (/ mil 100) (if (>= r 60) 1 0)) 24))
		 (M (if (>= r 60) (- r 60) r))
		 (P (if (>= h 12) "pm" "am"))
		 (H (if (>  h 12) (- h 12) (if (= h 0) 12 h))))
	(insert (format "%2d:%02d%s" H M P))))

;(defun hours-insert-current-time () (interactive)
;  (insert (format-time-string "%l:%M%#p")))

(defun hours-insert-current-date-time () (interactive)
  (insert hours-prefix)
  (hours-insert-current-date)
  (hours-insert-current-time)
  (insert hours-separator))

(defun hours-next-date () 
  (interactive) 
  (if (save-excursion
        (beginning-of-line)
             (next (calendar-gregorian-from-absolute (1+ (calendar-absolute-from-gregorian date)))))
      (end-of-line)
    (insert-string (format "\n%s %s "
                           (hours-format-date next) (calendar-day-name next 3)))))

(defface hours-interval-face
  '((t (:foreground "blue" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-interval-face 'hours-interval-face)

(defface hours-invoice-face
  '((t (:foreground "green" :bold t)))
  "`hours-mode' face used to highlight invoice entries."
  :group 'hours-mode)
(defvar hours-invoice-face 'hours-invoice-face)

(defface hours-date-face
  '((t (:foreground "magenta" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-date-face 'hours-date-face)

(defface hours-day-face
  '((t (:foreground "yellow" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-day-face 'hours-day-face)

(defface hours-time-face
  '((t (:foreground "brown" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-time-face 'hours-time-face)

;;;; Hours Folding...

(defun hours-fold-entry () (interactive)
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (while (and (looking-at "\\s ") (eq (forward-line -1) 0)))
      (end-of-line)
      (setq beg (point))
      (forward-line 1)
      (while (and (looking-at "\\s \\|$") (eq (forward-line 1) 0)))
      (backward-char)
      (setq end (point)))
    (if (< beg end)
      (hours-fold-region beg end))))

(defun hours-fold-region (beg end) (interactive "r")
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'hours-fold t)
    (overlay-put overlay 'before-string "...")))

(defun hours-unfold-region (beg end) (interactive "r")
  (let ((overlays (overlays-in beg end)) deleted)
    (while overlays
      (let ((ovl (car overlays)))
        (when (overlay-get ovl 'hours-fold)
          (delete-overlay (setq deleted ovl))))
      (setq overlays (cdr overlays)))
    deleted))

(defun hours-unfold-entry () (interactive)
  (hours-unfold-region (line-beginning-position) (min (1+ (line-end-position)) (point-max))))

(defun hours-toggle-entry () (interactive)
  (or (hours-unfold-entry) (hours-fold-entry)))

(defun hours-unfold-all () (interactive)
  (hours-unfold-region (point-min) (point-max)))

(defun hours-fold-all () (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^\\s \n]")
      (hours-fold-entry))))

(provide 'hours)
