;;; org-mode
(require 'org)

;; 画像をインラインで表示
(setq org-startup-with-inline-images t)

;; インデントモードにする
(setq org-startup-indented t)

;; インデントの幅を設定
(setq org-indent-indentation-per-level 1)

;; org-mode での強調表示を可能に
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; .org ファイルは自動的に org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-default-notes-file のディレクトリ
(setq org-directory "//Mac/Dropbox/Emacs/org/")

;; org-default-notes-file のファイル名
(setq org-default-notes-file "//Mac/Dropbox/Emacs/org/default.org")

;; TODO 状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)" "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))

;; LOGBOOK drawer に時間を格納
(setq org-clock-into-drawer t)

;; DONE の時刻を記録
(setq org-log-done 'time)

;; 文字化け対策
(setq system-time-locale "C")

;; アンダースコア後下付き文字になるのを回避
(setq org-export-with-sub-superscripts nil)


;;; org-capture-template
(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "//Mac/Dropbox/Emacs/org/task.org" "Task")
         "* TODO %?\n%U" :empty-lines 1)
        ("T" "Task with Clipboard" entry
         (file+headline "//Mac/Dropbox/Emacs/org/task.org" "Task")
         "* TODO %?\n%U\n%c" :empty-lines 1)
        ("n" "Note" entry
         (file+headline "//Mac/Dropbox/Emacs/org/note.org" "Note")
         "* %?\n%U" :empty-lines 1)
        ("N" "Note with Clipboard" entry
         (file+headline "//Mac/Dropbox/Emacs/org/note.org" "Note")
         "* %?\n%U\n%c" :empty-lines 1)))

;; カーソル位置に Task entry を追加
(defun my/org-capture ()
  "Insert an org-capture Task entry at point."
  (interactive)
  (org-capture 0 "t"))


;;; org-agenda
;; 対象ファイル
(setq org-agenda-files (list org-directory))

;; 始めから Log mode
(setq org-agenda-start-with-log-mode '(closed clock))


;;; open-junk-file
(require 'open-junk-file)
(setq open-junk-file-format "//Mac/Dropbox/Emacs/junk/%Y-%m-%d-%H%M%S.")


;;; org-clock
;; Emacs 終了時に org-clock-out
(defun my/org-clock-out-and-save ()
  "Save buffers and stop clocking when kill emacs."
  (when (org-clock-is-active)
    (org-clock-out)
    (save-some-buffers t)))
(add-hook 'kill-emacs-hook #'my/org-clock-out-and-save)

;; 1分未満を記録しない
(setq org-clock-out-remove-zero-time-clocks t)

;; タスク名をタイトルバーに表示
(setq org-clock-clocked-in-display 'frame-title)


;;; Archive ファイルを datetree で管理
(setq org-archive-location "//Mac/Dropbox/Emacs/org/archive.org::datetree/")
