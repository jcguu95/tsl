;;;; This file is under development.

(require 'tsl)

;;; Let user select timestring from a given string.

(defun my/tss-in-string (str)
  "Return the list of substrings of the string STR in the format
  my/*ts-format*."
  (-sort #'string<
         (-flatten (s-match-strings-all *ts-format* str))))

(defvar my/*ts-format*
  (rx (repeat 8 digit)
      "-"
      (repeat 6 digit))
  "Time string format: dddddddd-dddddd, where d denotes any
  digit.")

(defun my/select-ts-from-string (str)
  "Let user select a timestring from the given string STR."
  (let* ((tss (my/tss-in-string str))
         (len (length tss)))
    (cond ((= len 0) (message "No timestring to select."))
          ((= len 1) (car tss))
          (t (ivy-read "Select: " tss)))))

(defun my/find-note-from-ts (str)
  (let ((files (tsl:find (my/select-ts-from-string str))))
    (when files (dired-other-window files))))

;; Example
(my/find-note-from-ts
 "20210428-130000-4.jpeg 20210429-101000 2021501-065000
 20210502-060000 20210504-060500.jpeg 20210429-111500.jpeg
 20210502-060000.jpeg 20190808-000100")

(provide 'ranger-find-note)
