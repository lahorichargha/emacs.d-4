(require 'dss-paths)
(require 'ediff)
(require 'ediff-vers)
(require 'vc)
(require 'magit)

(setq magit-remote-ref-format 'remote-slash-branch)

;;; the builtin versions are buggy when the remote is anything other than origin/..
;;; http://stackoverflow.com/questions/2016901/viewing-unpushed-git-commits
(magit-define-inserter unpulled-commits (remote branch)
  (when remote
    (apply #'magit-git-section
           'unpulled "Unpulled commits:" 'magit-wash-log "log"
           (append magit-git-log-options
                   (list "..@{u}")))))

(magit-define-inserter unpushed-commits (remote branch)
  (when remote
    (apply #'magit-git-section
           'unpushed "Unpushed commits:" 'magit-wash-log "log"
           (append magit-git-log-options
                   (list "@{u}..")))))

(setq dvc-tips-enabled nil)
(setq vc-follow-symlinks t)
(define-key magit-mode-map (kbd "<") 'magit-pull)

(defun vc-ediff ()
  (interactive)
  (vc-buffer-sync)
  (ediff-load-version-control)
  (setq ediff-split-window-function 'split-window-horizontally)
  (ediff-vc-internal "" ""))

(defun dss/vc-state-refresh-open-buffers ()
  (interactive)
  (message "updating buffer VC status ...")
  (mapc (lambda (b)
          (dss/vc-state-refresh (buffer-file-name b)))
        (buffer-list))
  (message ""))

(defun dss/vc-state-refresh (file &optional backend)
  (interactive)
  (when (> (length file) 0)
    (setq backend (or backend (vc-backend file)))
    (when backend
      (vc-state-refresh file backend))))

;;;
(defun dss/git-rehitch ()
  (interactive)
  (let ((hitch-status
         (apply #'format
                (cons "git hitch: %s - %s"
                      (mapcar
                       (lambda (var)
                         (setenv var "")
                         (let ((val (dss/local-shell-command-to-string
                                     (concat "echo -n $" var))))
                           (setenv var val)
                           val))
                       '("GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL"))))))
    (message hitch-status)
    hitch-status))

(defun dss/magit-status-hook ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (insert (dss/git-rehitch))
    (insert "\n")))

(add-hook 'magit-refresh-status-hook 'dss/magit-status-hook)

(provide 'dss-vc)
