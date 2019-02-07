;;; Eshell
;; eshell alias
(setq eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -lh")
        (list "la" "ls -a")
        (list "emacs" "find-file $1")
        (list "m" "find-file $1")
        (list "mc" "find-file $1")
        (list "d" "dired .")
        (list "l" "eshell/less $1 $2"))))

;; written by Stefan Reichoer <reichoer@web.de>
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (interactive)
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))

;; make-new-shell
(defun eshell/make-new-eshell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

;; shell-toggle
(require 'shell-toggle)
(setq shell-toggle-launch-shell 'shell-toggle-eshell)
(setq shell-toggle-full-screen-window-only t)

;; shell-pop
(custom-set-variables
 '(shell-pop-shell-type (quote ("eshell" "*eshell*"
                                (lambda nil (eshell shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-c t")
 '(shell-pop-window-height 50)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom"))
