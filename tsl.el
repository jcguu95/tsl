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
  ;; TODO arg is universal arg? learn how to use. TODO more
  ;; options to be added. TODO image => sxiv? TODO images =>
  ;; dired? sxiv loop? TODO audio(s)/video(s) => emm? Use dired
  ;; by default.. really open the files if universal argument is
  ;; provided. in the later case, if there are multiple files,
  ;; ask the user by using ivy what to do next: either ivy,
  ;; force-open, or select one (or multiple?) to force-open.
  (let* ((result (tsl:find query))
         (len (length result)))
    (dired result)))

(defun org-tsl-store-link ()
  ;; TODO
  )
