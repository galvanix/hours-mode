(setq hours-time  "\\([0-9][0-9]?:[0-9][0-9]\\(?:am\\|pm\\)\\)"
      hours-date  "\\(20[0-9][0-9]/[0-9][0-9]?/[0-9][0-9]?\\)"
      hours-day   "\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)"
      hours-hours "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)")
(setq hours-entry
      (concat "^" hours-date " +" hours-day " +" hours-time " +- +" hours-time " +" hours-hours "$"))
(setq hours-invoice
      (concat "^Invoice +" hours-date " +" hours-day " +" hours-hours " +Hours"))
(setq hours-outline-regexp
      (concat "\\(" hours-invoice "\\|" hours-entry "\\)"))

(setq hours-test "^jim")
(setq hours-font-lock-keywords
      `(("hello" . font-lock-warning-face)
        (,hours-entry   . (let ((date (hours-date-at-point))
                                (day (string-match 2))
                                (time1 (string-match 3))
                                (time2 (string-match 4))
                                (hours (string-match 5)))
                            (if (and
                                 (calendar-date-is-legal-p date)
                                 (eq (calendar-day-name date 3) day)
                                 (= (- time2 time1) hours))
                                hours-entry-face
                              hours-entry-face)))
        (,hours-invoice . hours-invoice-face)
        (,hours-day     . hours-day-face)
        (,hours-time    . hours-time-face)))

;; use font-lock-warning-face for bad dates

(easy-mmode-defmap hours-mode-map
  `(("\C-cd" . hours-next-date)
    ("\C-c\C-c" . hours-something))
  "Keymap for `hours-mode'.")

(define-derived-mode hours-mode text-mode "Hours"
  "Major mode for viewing/editing hours files."
  (set (make-local-variable 'font-lock-defaults) hours-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) hours-outline-regexp))


(define-minor-mode hours-minor-mode
  "Hours mode for viewing/editing time cards in hours format.
\\{hours-minor-mode-map}"
  nil " Hours" nil)


(require 'calendar)

(defun hours-date-at-point ()
  (if (looking-at "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)")
      (list (string-to-int (match-string 2))
            (string-to-int (match-string 3))
            (string-to-int (match-string 1)))))

(defun hours-format-date (date)
  (format "%d/%02d/%02d"
          (nth 2 date) (nth 0 date) (nth 1 date)))

;;(calendar-date-is-legal-p '(10 10 2003))
;;(calendar-current-date)

(defun hours-next-date () 
  (interactive) 
  (if (save-excursion
        (beginning-of-line)
             (next (calendar-gregorian-from-absolute (1+ (calendar-absolute-from-gregorian date)))))
      (end-of-line)
    (insert-string (format "\n%s %s "
                           (hours-format-date next) (calendar-day-name next 3)))))

(defface hours-entry-face
  '((t (:foreground "blue" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-entry-face 'hours-entry-face)

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
  '((t (:foreground "magenta" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-day-face 'hours-day-face)

(defface hours-time-face
  '((t (:foreground "magenta" :bold t)))
  "`hours-mode' face used to highlight complete entries."
  :group 'hours-mode)
(defvar hours-time-face 'hours-time-face)

(setq hours-font-lock-defaults
      '(hours-font-lock-keywords t nil nil nil (font-lock-multiline)))
