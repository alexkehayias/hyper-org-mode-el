(require 'request)
(require 'url)

;; Override this in your init.el to set the directory location of
;; hyper org files
(defvar hyper-org-dir "/.hyper-org")
;; Override this in your init.el to point it at the url of the hyper
;; org server
(defvar hyper-org-url "http://127.0.0.1:1986")

(defun string-to-file (file-name content)
  "Save the contents to file.
   Example:
   (string-to-file \"/path/to/file.org\" \"content here\""
  (save-excursion
    (let (buf)
      (unwind-protect
          (progn
            (setq buf (find-file-noselect file-name t))
            (when buf
              (message "Saving %S" file-name)
              (set-buffer buf)
              (toggle-read-only -1)
              (erase-buffer)
              (insert content)
              (save-buffer)))))))

;; (string-to-file "/Users/alexkehayias/Desktop/test.txt" "yo")

(defun handle-conflict ())

(defun hyper-org-pull (file-name)
  "Pull the file-name from the server and save it locally

   Example:
   (hyper-org-pull \"todo.org\")"
  (message "Getting %S" (concat hyper-org-url "/api/v1/pull/" file-name))
  (lexical-let ((file-name file-name))
    (request (concat hyper-org-url "/api/v1/pull/" file-name)
             :type "GET"
             :parser (lambda () (buffer-string))
             :success (function*
                       (lambda (&key data &allow-other-keys)
                         (when data
                           (string-to-file
                            (concat hyper-org-dir "/" file-name) data))))
             ;; TODO on error open up ediff
             :error (function*
                     (lambda (&key data &allow-other-keys)
                       (message "ERROR: %S" (assoc-default 'files data))))
             :complete (function*
                        (lambda (&key data &allow-other-keys)
                          (message "Finished syncing"))))))

(defun hyper-org-push (file-path proposed-file-name previous-file-name)
  "Push changes from local file to server.
   Example:
   (hyper-org-push \"/my/file/path\" \"todo.org\" \"previous.org\")"
  (prin1 (concat file-path "/" proposed-file-name))
  (let ((proposed-path (format "%s/%s" file-path proposed-file-name))
        (previous-path (format "%s/%s" file-path previous-file-name)))
    (request (concat hyper-org-url "/api/v1/push/" file-path)
             :type "POST"
             :files `(("proposed" . ,proposed-path)
                      ("previous" . ,previous-path))
             :parser 'json-read
             :success (function*
                       (lambda (&key data &allow-other-keys)
                         (message "SUCCESS: %S" (assoc-default 'files data))))
             ;; TODO on error open up ediff
             :error (function*
                     (lambda (&key data &allow-other-keys)
                       (message "ERROR: %S" (assoc-default 'files data)))))))

;; Macro for running a function repeatedly in the back ground
;; https://github.com/punchagan/dot-emacs/blob/master/punchagan.org
(defmacro run-with-timer-when-idle (secs idle-time repeat-time function &rest args)
  "Run a function on timer, but only when idle."
  `(run-with-timer
    ,secs
    ,repeat-time
    (lambda () (run-with-idle-timer ,idle-time nil ,function ,@args))))

;; Run the synchronization with the server in the background
;; repeatedly every 5 seconds
;; TODO extend this to synchronize multiple files
(run-with-timer-when-idle 5 5 5 'hyper-org-pull "todo.org")
