;;; Helm
(use-package helm :ensure
  :bind
  (("M-x"           . helm-M-x)
   ("C-x C-f"       . helm-find-files)
   ("C-x C-x"       . helm-mini)
   ("C-x C-z"       . helm-resume)
   ("C-c i"         . helm-semantic-or-imenu)
   ("C-c w"         . helm-google-suggest)
   ("C-c C-SPC"     . helm-all-mark-rings)
   ("C-c <f1>"      . helm-info)
   ("C-M-y"         . helm-show-kill-ring)
   :map helm-map
   ("TAB"           . helm-execute-persistent-action)
   ("C-z"           . helm-select-action)
   ("M-b"           . my/helm-ff-run-browse-project)
   ("<f1>"          . helm-help))
  :custom
  (helm-mini-default-sources            ; helm-mini に表示するソース
   '(helm-source-buffers-list
     helm-source-recentf
     helm-source-files-in-current-dir))
  (helm-candidate-number-limit 100)     ; 表示する最大候補数
  (helm-autoresize-max-height 0)        ; Helm バッファのサイズ
  (helm-autoresize-min-height 40)
  (helm-default-display-buffer-functions '(display-buffer-in-side-window))
                                        ; Helm バッファが常にウィンドウの下側に来るように設定
  (helm-ff-skip-boring-files t)         ; 次のファイルは非表示
  (helm-boring-file-regexp-list (quote ("Icon.$")))
  (helm-scroll-amount 8)                ; その他の設定
  (helm-split-window-inside-p t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  :config
  (bind-key* "M-m"  'helm-migemo-mode helm-map)
  (helm-autoresize-mode t)
  (helm-mode t)
  (helm-migemo-mode t)
  ;; helm-find-file から browse-project を呼び出す
  (defun my/helm-ff-run-browse-project ()
    "Call helm-ff-run-browse-project with C-u."
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'helm-ff-run-browse-project))
  ;; helm-gtags が UNC path 環境下で動作しない問題を回避
  (advice-add 'select-window
              :around (lambda (orig-fun &rest args)
                        (when (nth 0 args)(apply orig-fun args)))))


;;; helm-elscreen
(use-package helm-elscreen :ensure
  :after (helm elscreen)
  :bind ("C-x C-l"  . helm-elscreen))


;;; helm-ag(ripgrep)
(use-package helm-ag :ensure
  :after helm
  :bind ("C-c g"    . helm-do-ag)
  :custom
  (helm-ag-base-command "rg --vimgrep --no-heading --smart-case"))


;;; helm-swoop
(use-package helm-swoop :ensure
  :after helm
  :bind
  (("M-s"           . helm-swoop)
   :map helm-swoop-map
   ("C-s"           . helm-next-line)
   ("C-r"           . helm-previous-line))
  :custom (helm-swoop-move-to-line-cycle nil) ; リストを循環しない
  )


;;; helm-descbinds
(use-package helm-descbinds :ensure
  :after helm
  :bind ("C-c k"    . helm-descbinds)
  )


;;; Yasnippet
(use-package yasnippet
  :custom
  (yas-snippet-dirs
   '("//Mac/Dropbox/Emacs/snippets/mysnippets" ; 自作スニペット
     "//Mac/Dropbox/Emacs/snippets/yasnippets" ; デフォルトスニペット
     )))

(use-package helm-c-yasnippet :ensure
  :after (helm yasnippet)
  :bind ("C-c y"    . helm-yas-complete)
  :custom (helm-yas-space-match-any-greedy t)
  :config
  (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
  (yas-global-mode t))


;;; Projectile
(use-package projectile :ensure
  :custom (projectile-completion-system 'helm)
  :config (projectile-mode t))

(use-package helm-projectile :ensure
  :after (helm projectile)
  :bind ("C-x C-p"  . helm-projectile)
  :bind-keymap ("C-c C-p" . projectile-command-map)
  :config
  (helm-projectile-on)
  ;; helm-projectile-ag が ripgrep で動作しない問題を回避
  (defun helm-projectile-ag (&optional options)
    "Helm version of projectile-ag."
    (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
    (if (require 'helm-ag nil  'noerror)
        (if (projectile-project-p)
            (let ((helm-ag-command-option options)
                  (current-prefix-arg nil))
              (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
          (error "You're not in a project"))
      (error "helm-ag not available"))))


;;; helm-man-woman
(use-package helm-elisp
  :after helm
  :bind ("<M-f1>"   . my/helm-for-document)
  :custom
  (helm-for-document-sources            ; 基本となるソースを定義
   '(helm-source-info-elisp
     helm-source-info-cl
     helm-source-info-eieio))
  :config
  ;; man, info, apropos を串刺し検索する
  (defun my/helm-for-document ()
    "Preconfigured `helm' for helm-for-document."
    (interactive)
    (let ((default (thing-at-point 'symbol)))
      (helm :sources
            (nconc
             (mapcar (lambda (func)
                       (funcall func default))
                     helm-apropos-function-list)
             helm-for-document-sources)
            :buffer "*helm for document*"))))
