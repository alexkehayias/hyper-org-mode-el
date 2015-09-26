(require 'request)

;; Override this in your init.el to set the directory location of
;; hyper org files
(defvar hyper-org-dir "/.hyper-org")

;; Override this in your init.el to point it at the url of the hyper
;; org server
(defvar hyper-org-url "http://127.0.0.1:1986")

;; Override this to decide which files to sync
(defvar hyper-org-files (list "todo.org"))

;; Override this to set how often to sync with server
(defvar hyper-org-sync-freq-sec 5)

(defun overwrite-file (file-name content)
  (with-current-buffer (find-file-noselect file-name t)
    (message "Overwriting file %S" file-name)
    (toggle-read-only -1)
    (erase-buffer)
    (insert content)
    (save-buffer)))

(defun sync-file (file-name content)
  "Save the contents to file if there is any change.
   Destructively overwrites the file with content and stores an
   additional copy of the file at <file-name>.prev which is used
   for conflict resolution.

   Example:
   (string-to-file \"/path/to/file.org\" \"content here\")"
  (save-excursion
    (unwind-protect
        (let* ((working-buf (find-file-noselect file-name t))
               (working-content (with-current-buffer working-buf
                                 (buffer-substring-no-properties (point-min)
                                                                 (point-max))))
              (working-modified (with-current-buffer working-buf
                                  (buffer-modified-p working-buf)))
              (prev-file-name (concat file-name ".prev"))
              (prev-buf (find-file-noselect prev-file-name t))
              (prev-content (with-current-buffer prev-buf
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
          ;; Only sync the file if it has not been modified
          (if working-modified
              (message "Working modified, not syncing")
            ;; If the buffer is different AND the buffer is
            ;; different from the previous sync'd file then
            ;; overwrite the file otherwise we can accidentally blow
            ;; away changes from the working file
            (if (and (not (equal working-content content))
                     (equal working-content prev-content))
                (progn (overwrite-file file-name content)
                       (overwrite-file prev-file-name content))
              (message
               (format "Not syncing %s. Local same as remote? %s Dirty local? %s"
                       file-name
                       (equal working-content content)
                       (not (equal working-content prev-content))))))))))

(defun handle-conflict ())

(defun hyper-org-pull (file-name)
  "Pull the file-name from the server and save it locally.

   Example:
   (hyper-org-pull \"todo.org\")"
  (message "Syncing %S" (concat hyper-org-url "/api/v1/pull/" file-name))
  (lexical-let ((file-name file-name))
    (request (concat hyper-org-url "/api/v1/pull/" file-name)
             :type "GET"
             :parser (lambda () (buffer-string))
             :success (function*
                       (lambda (&key data &allow-other-keys)
                         ;; TODO save it to the local file and to the
                         ;; "previous" version of the file
                         (when data
                           (sync-file (concat hyper-org-dir "/" file-name) data))))
             :error (function*
                     (lambda (&key data &allow-other-keys)
                       (message "ERROR: %S" (assoc-default 'files data))))
             :complete (function*
                        (lambda (&key data &allow-other-keys)
                          (message "Finished syncing %S" file-name))))))

(defun hyper-org-push (file-path proposed-file-name previous-file-name)
  "Push changes from local file to server.
   Example:
   (hyper-org-push \"/my/file/path\" \"todo.org\" \"previous-todo.org\")"
  (message "Pushing" (concat file-path "/" proposed-file-name))
  (let ((proposed-path (format "%s/%s" file-path proposed-file-name))
        (previous-path (format "%s/%s" file-path previous-file-name)))
    (lexical-let ((file-path file-path))
      (request (concat hyper-org-url "/api/v1/push/" file-path)
               :type "POST"
               :files `(("proposed" . ,proposed-path)
                        ("previous" . ,previous-path))
               :parser 'json-read
               ;; If the push is successful, synchronize with the server
               :success (function*
                         (lambda (&key data &allow-other-keys)
                           (hyper-org-pull file-path)))
               ;; TODO on error open up ediff
               :error (function*
                       (lambda (&key data &allow-other-keys)
                         (message "ERROR: %S" (assoc-default 'files data))))
               :complete (function*
                        (lambda (&key data &allow-other-keys)
                          (message "Finished pushing %S" proposed-file-name)))))))

;; Macro for running a function repeatedly in the back ground
;; https://github.com/punchagan/dot-emacs/blob/master/punchagan.org
(defmacro run-with-timer-when-idle (secs idle-time repeat-time function &rest args)
  "Run a function on timer, but only when idle."
  `(run-with-timer
    ,secs
    ,repeat-time
    (lambda () (run-with-idle-timer ,idle-time nil ,function ,@args))))

(defun hyper-org-post-save ()
  ;; On post save, check if the saved file is in the list of files
  ;; that we want to sync. If it is push it to the server.
  ;; TODO prevent pushing right after syncing somehow
  (message "POST SAVED"))

;; Run the synchronization with the server in the background
(defun hyper-org-set-timers ()
  (dolist (el hyper-org-files)
    (lexical-let ((f el))
      (message "Initializing hyper-org sync for file %S" f)
      (run-with-timer-when-idle 0 hyper-org-sync-freq-sec hyper-org-sync-freq-sec 'hyper-org-pull f))))

(hyper-org-set-timers)


;; TODO post save hook for org mode, if it has a file name that is in
;; the var "synced-files" then push the file to the server
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'hyper-org-post-save)))
