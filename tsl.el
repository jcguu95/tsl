;; tsl -- timestamped links handler

(defvar tsl:lib)

(loop for dir in
      '("~/data/storage/recordings"
        "~/data/storage/memories"
        "~/data/storage/+org/wiki/fleeting")
      do (add-to-list 'tsl:lib dir))

(loop for dir in (f-directories "~/data/storage/memories")
      do (add-to-list 'tsl:lib dir))

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
  (let ((len (length query)))
    (if (<= len 8)
        (rx-to-string `(seq ,query
                            (repeat ,(- 8 len) digit)
                            "-"
                            (repeat 6 digit)))
      (rx-to-string `(seq ,query
                          (repeat ,(- 15 len) digit))))))

(defun tsl:find (query)
  (loop for file in (-flatten (loop for dir in tsl:lib
                                    collect (f-files dir)))
        if (string-match (tsl:regex<-query query) (f-base file))
        collect file))

(org-link-set-parameters "tsl"
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
                        ;; Filter based on hash.
                        (when hash
                          (progn
                            (message "Computing hash might take a while..")
                            (setf result
                                  (-filter (lambda (file)
                                             (string-match hash (md5sum file)))
                                           result))))
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
                        result)))

      ;; Do filter the results with lisp control string.
      ;; FIXME should put this into tsl:find
      (setf result (filter-with result lst))
      (if result
          (progn
            (dired (cons "" result))
            (message "%s match(es) found for query -- %s. Full result: %s"
                     (length result) str result))
        (message "No matches found! Sorry!")))))

(defun org-tsl-store-link ()
  "Store a link to a timestamped link."
  ;; It only works for org-store-link ..
  (let* ((ts (read-from-minibuffer "Time: "))
         (lisp (read-from-minibuffer "Lisp: "))
         (link (concat "tsl" ":" ts "::" lisp))
         (description (read-from-minibuffer "Description: ")))
    (org-link-store-props
     :type "tsl"
     :link link
     :description description)))
