(require 'multi-term)
(require 'comint)

(setq shell-command-switch "-lc")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; multi-term
(autoload 'multi-term "multi-term")
(setq multi-term-program "/bin/bash")
(defun dss/cd-multi-term (dir &optional command switch)
  (let (tmp-buffer term-buffer)
    ;; with-temp-buffer gets in the way here
    (set-buffer (setq tmp-buffer (get-buffer-create "*multi-term-launcher*")))

    (setq default-directory dir)

    ;;(setq term-buffer (multi-term))
    (setq term-buffer (multi-term-get-buffer current-prefix-arg))
    (set-buffer term-buffer)
    (multi-term-internal)

    (kill-buffer tmp-buffer)

    (if command
        (term-send-raw-string command))
    (unless (and (not (eq switch nil))
                 (< switch 0))
        (switch-to-buffer term-buffer))
    term-buffer))

(defun dss/remote-term (host &optional command)
  (interactive)
  (let (term-buffer
        (index 1)
        term-name)
    (while (buffer-live-p
            (get-buffer (format "*%s<%s>*" host index)))
      (setq index (1+ index)))
    (setq term-name (format "%s<%s>" host index))
    (setq term-buffer
          (make-term term-name "/usr/bin/ssh" nil host))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer)
    (dss/term-setup-tramp)
    (if command
        (term-send-raw-string command))))

(defun dss/multi-term ()
  (interactive)
  (if (string-match-p "^/scp:" default-directory)
      (let ((host (second (split-string default-directory ":")))
            (dir (third (split-string default-directory ":"))))
        (dss/remote-term
         host
         (concat "cd " dir "; clear\n")))
    (multi-term)))

(defun dss/term-toggle-mode ()
  "Toggle between term-char-mode and term-line-mode."
  (interactive)
  (if (term-in-line-mode)
      (dss/term-char-mode)
    (term-line-mode)))

(defun dss/term-char-mode ()
  (interactive)
  (term-char-mode)
  (comint-goto-process-mark))

(defun dss/term-dabbrev ()
  (interactive)
  (let ((beg (point)))
    (dabbrev-expand nil)
    (kill-region beg (point)))
  (term-send-raw-string (substring-no-properties (current-kill 0))))

(defun dss/term-backward-kill-word ()
  (interactive)
  (if (term-in-line-mode)
      (backward-kill-word 1)
    (term-send-backward-kill-word)))

(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-escape-map "/"
               'dss/term-dabbrev)
             (define-key term-mode-map (kbd "C-c C-j")
               'dss/term-toggle-mode)
             (define-key term-mode-map (kbd "M-DEL")
               'dss/term-backward-kill-word)
             (define-key term-mode-map (kbd "M-g")
               'dss/term-toggle-mode)
             (linum-mode -1)))

(defun dss/term-yank ()
  (interactive)
  (if (term-in-line-mode)
      (yank)
    (term-paste)))

(defun dss/term-reverse-search ()
  (interactive)
  (if (term-in-line-mode)
      (isearch-backward)
    (term-send-reverse-search-history)))

;(defun term-forward-search ()
;  (interactive)
;  (if (term-in-line-mode)
;      (isearch-forward)
;    (term-send-forward-search-history)))


;;; derived from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(defun dss/term-setup-tramp ()
  "Setup ansi-term/tramp remote directory tracking"
  (interactive)
  (term-send-raw-string "
# Emacs ansi-term/tramp remote directory tracking
if [ $TERM = eterm-color ]; then
    function eterm-set-cwd {
        $@
        echo -e \"\\033AnSiTc\" $(pwd)
    }

    # set hostname, user, and cwd
    function eterm-reset {
        echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)}
        echo -e \"\\033AnSiTc\" $(pwd)
        echo -e \"\\033AnSiTh\" ${TRAMP_HOSTNAME-$(hostname)}
    }

    for temp in cd pushd popd; do
        alias $temp=\"eterm-set-cwd $temp\"
    done

    # set hostname, user, and cwd now
    eterm-reset
fi
"))

(setq term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-x C-x" . term-send-raw)
     ;("C-p" . term-send-raw);previous-line)
     ;("C-n" . term-send-raw);next-line)
     ("C-s" . isearch-forward)
     ("C-r" . dss/term-reverse-search)
     ("C-m" . term-send-raw)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-N" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input)
     ("M-." . comint-dynamic-complete)
     ("Od" . term-send-backward-word)
     ("Oc" . term-send-forward-word)
     ("M-d" . term-send-forward-kill-word)
     ("M-g" . dss/term-toggle-mode)
     ("C-y" . dss/term-yank)))

;; also see http://dea.googlecode.com/svn/trunk/my-lisps/multi-term-settings.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-term)
