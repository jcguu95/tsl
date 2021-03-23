;; tsl -- timestamped links handler

(setf tsl:lib '("~/data/storage/recordings"
                "~/data/storage/memories/2011"))

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

(tsl:find "201106")

(dired (tsl:find "20111029-08"))

(org-open-file (car (tsl:find "201110")))

;; TODO - I want to add an org link type like this.
;;   [[tsl:201106]]

(org-link-set-parameters "tsl"
                         :follow #'org-tsl-open
                         :store #'org-tsl-store-link)

(defun org-tsl-open (query arg)
  ;; arg is universal arg? learn how to use.
  (let* ((result (tsl:find query))
         (len (length result)))
    (dired result)))
