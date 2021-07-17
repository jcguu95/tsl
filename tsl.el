;; tsl -- timestamped links handler

;; Disclaimer: this is a personal script.

(require 'cl)
(require 'rx)
(require 'f)
(require 'dired)

;;; ;;; ;;; ;;; ;;; ;;; ;;
;;; user customization ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;

;; TODO change name - org-link-ts?
;;
(defvar tsl:lib nil
  "The variable that stores the list of directories to search.")
(defvar tsl:*search-history* nil
  "The variable that stores all search histories that returned
  non-NIL. It does not persist beyond different sessions.")

(loop for dir in
      '("~/data/storage/recordings"
        "~/data/storage/memories"
        "~/data/storage/+org/wiki/fleeting"
        "~/data/storage/+org/store")
      do (add-to-list 'tsl:lib dir))

(loop for dir in
      (f-directories "~/data/storage/memories")
      do (add-to-list 'tsl:lib dir))

;;; ;;; ;;; ;; ;;
;;; utilities ;;;
;;; ;;; ;;; ;; ;;

(defun tsl:regex<-query (query)
  "Expect query to be a string in the following formats, in
  examples:

   2020
   202007
   20200725
   20200725-10
   20200725-1030
   20200725-103057

This function will complete the query into a full query. For
example,

   202007          => 202007dd-dddddd
   20200725        => 20200725-dddddd
   20200725-1030   => 20200725-1030dd
   20200725-103057 => 20200725-103057"
  ;; TODO rewrite it to support queries like ????0325-1534??
  (let ((len (length query)))
    (if (<= len 8)
        (rx-to-string `(seq ,query
                            (repeat ,(- 8 len) digit)
                            "-"
                            (repeat 6 digit)))
      (rx-to-string `(seq ,query
                          (repeat ,(- 15 len) digit))))))

(defun tsl:find (query)
  (let* ((targets (-flatten
                   (loop for dir in tsl:lib
                         collect
                         (loop for file in (cddr (directory-files dir))
                               collect (concat dir "/" file)))))
         (result (loop for file in targets
                       if (string-match (tsl:regex<-query query) (f-base file))
                       collect file)))
    (or result
        (progn
          (run-with-timer 1 nil
                          (lambda (query)
                            (message "No results found for query: %s." query))
                          query)
          nil))))

;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
;;; org link of customized type ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;

(org-link-set-parameters
 "tsl"
 :follow #'org-tsl-open
 :store #'org-tsl-store-link)

(defun org-tsl-open (str arg)
  ;; TODO arg is universal arg? learn how to use.
  ;;
  ;; TODO more
  ;; options to be added.
  ;;
  ;; TODO image => sxiv? images => dired? sxiv loop?
  ;; audio(s)/video(s) => emm?
  ;;
  ;; Use dired by default.. really open the files if universal
  ;; argument is provided. in the later case, if there are
  ;; multiple files, ask the user by using ivy what to do next:
  ;; either ivy, force-open, or select one (or multiple?) to
  ;; force-open.
  ;;
  ;; DONE- allow link format
  ;; [[tsl:20191026::(:name "screen" :hash "^3e4d")]]
  ;; [[tsl:20191026::(:name "screen" :hash "3e4d")]]
  ;; [[tsl:20191026::(:hash "3e4d" :name "screen")]]
  ;; [[tsl:20191026::(:hash "3e4d" :name ("screen" "jpeg"))]]
  ;;
  ;; TODO 自動偵測 filetype 並給予建議
  (let* ((query (nth 0 (split-string str "::")))
         (result (tsl:find query))

         (lisp (nth 1 (split-string str "::")))
         (lst (when lisp (read lisp))))

    (cl-labels
        ((md5sum (file)
                 (with-demoted-errors "Error: %S"
                   (and (file-exists-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (md5 (buffer-string))))))
         (filter-with (result lst)
                      ;; main function to filter using lisp control string.
                      (let ((hash (plist-get lst :hash))
                            (name (plist-get lst :name)))
                        ;; Filter based on name(s).
                        (when name
                          (when (atom name) (setf name (list name)))
                          ;; Allow those whose absolute paths
                          ;; match all regex given in the list
                          ;; NAME.
                          (loop for n in name
                                do (setf result
                                         (-filter (lambda (file)
                                                    (string-match n (f-full file)))
                                                  result))))
                        ;; TODO Add checks for ":size" and ":type".
                        ;; Filter based on hash.
                        (when hash
                          (progn
                            (message "Computing hash might take a while..")
                            (setf result
                                  (-filter (lambda (file)
                                             (if (f-directory-p file)
                                                 t ;; pass all directories on hashes
                                               (string-match hash (md5sum file))))
                                           result))))
                        result)))

      ;; Do filter the results with lisp control string.
      ;; FIXME should put this into tsl:find
      (setf result (filter-with result lst))
      (if result
          (progn
            (add-to-list 'tsl:*search-history* (cons str result))
            (dired (cons "" result))
            (message "%s match(es) found for query -- %s. Full result: %s"
                     (length result) str result))
        (message "No matches found! Sorry!")))))

(defun org-tsl-store-link ()
  "Store a link to a timestamped link."
  ;; It only works for org-store-link ..
  ;; FIXME This interferes with org-capture even without the context of TSL.
  ;; org-capture calls org-store-link, which calls org-store-link-functions..
  ;; so I shouldn't use read-from-minibuffer.
  (let* (;; (ts (read-from-minibuffer "Time: "))
         ;; (lisp (read-from-minibuffer "Lisp: "))
         ;; (link (concat "tsl" ":" ts "::" lisp))
         ;; (description (read-from-minibuffer "Description: "))
         ts lisp link description
         )
    (org-link-store-props
     :type "tsl"
     :link link
     :description description)))

(defun tsl:go-back-history ()
  ;; TODO merge this with usual search
  (interactive)
  (dired
   (alist-get
    (ivy-read "Query string: "
              (mapcar #'car tsl:*search-history*))
    tsl:*search-history*
    nil nil #'string=)))


;; experiment
;;
;;
(defun tsl:check-ts-format (str)
  "Check if the input string is an expected time string.

Mechanism: first check if it's of length 4, 6, 8, 11, 13, or 15.
If not, return nil. If yes, complete the string canonically so
that it's of length 15.

Next, break them into tokens, and check if they are as expected."
  ;; if length 4, can be any number
  ;; if length 6, the last two should be 01~12
  ;; if length 8, after translating to a date, should be a legit date
  ;; if length 11, first 8 should give a legit date, 9th should be a -, 10+11 should be between 00~23
  ;; if length 13, ..etc + the last two should be 00~59
  ;; if length 15, ..etc + the last two should be 00~59
  ;;
  ;; ;; test cases
  ;; (mapcar #'tsp:check-ts-format '("2020" "20210731-150156")) ;; all t
  ;; (mapcar #'tsp:check-ts-format '("20200731-" "20210731-150161")) ;; all nil
  (let ((len (length str)))
    (when
        ;; First sanity check.
        (cl-case len
          (4 (setf str (concat str "0101-000000")))
          (6 (setf str (concat str "01-000000")))
          (8 (setf str (concat str "-000000")))
          (11 (setf str (concat str "0000")))
          (13 (setf str (concat str "00")))
          (15 str))
      ;; Break them into tokens, and check if as expected.
      (ignore-errors
        (let ((year (read (substring str 0 4)))
              (month (read (substring str 4 6)))
              (day (read (substring str 6 8)))
              (dash (read (substring str 8 9)))
              (hour (read (substring str 9 11)))
              (minute (read (substring str 11 13)))
              (sec (read (substring str 13 15))))
          (and (and (integerp year) (> year 0))
               (and (integerp month) (<= 1 month 12))
               (and (integerp day) (<= 1 day 31))
               (eq '- dash)
               (and (integerp hour) (<= 0 hour 23))
               (and (integerp minute) (<= 0 minute 59))
               (and (integerp sec) (<= 0 sec 59))))))))

(defun tsl:extract-ts-from-string (str)
  "The core utility that extracts time stamps from any given
string."
  ;; ;; test cases
  ;; (mapcar #'tsp:extract-ts-from-string
  ;;         (list
  ;;          "This is a long--message- asd -s--20210107 -12--sd ok"
  ;;          "This is a long--message- asd -s--20210107-12--sd ok"
  ;;          "This is a long--message- asd -s--20210131-085932--sd ok"
  ;;          "This is a long--message- asd -s--20210170--sd ok"
  ;;          "This is a long--message- asd -s--202101--sd ok"))
  (let ((result (-uniq
                 (-filter (lambda (x) (tsl:check-ts-format x))
                          (-flatten
                           (s-match-strings-all

                            (rx word-start
                                (** 4 15 (any digit "-"))
                                word-end)

                            str)

                           )))))

    ;; This is just a weird hack to prevent "legal" timestrings
    ;; like "101010" .. and it would still have some edge cases.
    (-filter (lambda (x) (>= (length x) 8)) result)))

(defun tsl:find-at-point ()
  ;; Only support "full" time string at this moment.
  (interactive)
  (let* ((str (thing-at-point 'sentence 'no-property))
         (ts (tsl:extract-ts-from-string str)))
    (dired-other-window (-flatten (-uniq (mapcar #'tsl:find ts))))))

;; TODO use the following snippet to let user select which window to open dired in
;; (defun my/dired-ace-selected-window (dirname &optional switches)
;;   (interactive (dired-read-dir-and-switches "in other window "))
;;   (let ((wnd (aw-select "Modeline?")))
;;     (aw-switch-to-window wnd)
;;     (switch-to-buffer (dired-noselect dirname switches))))

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;


;;; Let user select timestring from a given string.
;;;
;;; TODO Integrate this part of code with the above.

(defvar my/*ts-format*
  (rx (repeat 8 digit)
      "-"
      (repeat 6 digit))
  "Time string format: dddddddd-dddddd, where d denotes any
  digit.")

(defun my/tss-in-string (str)
  "Return the list of substrings of the string STR in the format
  my/*ts-format*."
  (-sort #'string<
         (-flatten (s-match-strings-all my/*ts-format* str))))

(defun my/select-ts-from-string (str)
  "Let user select a timestring from the given string STR."
  (let* ((tss (my/tss-in-string str))
         (len (length tss)))
    (cond ((= len 0) (message "No timestring to select."))
          ((= len 1) (car tss))
          (t (ivy-read "Select timestamp: " tss)))))

(defun my/find-ts-note (str)
  "Extract timestamps from STR, let user choose one if multiple,
and find all files in TSL:LIB that has the selected timestamp."
  (let ((files (tsl:find (my/select-ts-from-string str))))
    (when files (dired-other-window files))))

;; Example
;; (my/find-ts-note
;;  "20210428-130000-4.jpeg 20210429-101000 2021501-065000
;;   20210502-060000 20210504-060500.jpeg 20210429-111500.jpeg
;;   20210502-060000.jpeg 20190808-000100 20210701
;;   20210714-133511---first-paper-submitted-to-arxiv")

(provide 'tsl)
