;;
;; mu7889yoon Y.Nakamura's Emacs settings
;;



;;
;; START UP
;;
;;クリップボードの共有
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;disable statup message
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode 0)



;;
;;PACKAGE MANAGER
;;
;;straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;
;; FRAMEWORK
;;
;; helm.el
(straight-use-package 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)

;; emacs-which-key.el
(straight-use-package 'which-key)
(which-key-mode)
(add-hook 'after-init-hook' which-key-mode)

;;
;;TEXT COMPLETION
;;
;;copilot.el
(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))
(setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node")
(setq copilot-mode t)
(add-hook 'prog-mode-hook 'copilot-mode)


;; company.el

(straight-use-package 'company)
(straight-use-package 'dash)
(straight-use-package 's)
(straight-use-package 'editorconfig)
(setq company-minimum-prefix-length 1
      company-idle-delay 0
      company-tooltip-align-annotations t
      company-dabbrev-other-buffers nil
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-format-margin-function nil)

;;(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; company-box.el
(straight-use-package 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


;;
;;Interface
;;
;;linum.el 行番号を表示
(straight-use-package 'linum)
(setq linum-format "%d ")
(global-linum-mode t)
(set-face-foreground 'linum "grey")


;;neotree.el ディレクトリツリー表示
(straight-use-package 'neotree)
:init
(setq-default neo-keymap-style 'concise)
  (setq neo-smart-open t)
(setq neo-create-file-auto-open t)



;;doom-themes.el 現在使用しているテーマ（コメントの色変更）
(straight-use-package 'doom-themes)
(setq doom-dracula-brighter-comments t)
(load-theme 'doom-dracula t)


;;(load-theme 'doom-city-lights t)



;; powerline.el
(straight-use-package 'powerline)
(powerline-default-theme)

;; doom-modeline.el
(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)

;;beacon.el 現在のカーソル位置を目立たせるパッケージ
(straight-use-package 'beacon)
(beacon-mode 1)

;;symbol-overlay.el シンボルをハイライト
(straight-use-package 'symbol-overlay)
(add-hook 'prog-mode-hook 'symbol-overlay-mode)

;;hide-mode-line.el neotreeのモードラインを非表示
(straight-use-package 'hide-mode-line)
(add-hook 'neotree-mode-hook #'hide-mode-line-mode)

;;モードラインに時刻表示
(display-time-mode t)
(setq display-time-24hr-format t)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%b %f %& %Z")

;; rainbow-delimiters.el 括弧を色分け
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; バッテリーの残量を表示
(display-battery-mode t)

;; バッテリー表示をアイコンに
(setq battery-mode-line-format "[%b%p%%]")
(setq battery-load-critical 10)
(setq battery-load-low 20)


;;
;;MINOR MODE
;;
;;php-mode.el

(straight-use-package 'php-mode)
(require 'php-mode)

;;docker-mode.el
(straight-use-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;docker-compose-mode
(straight-use-package 'docker-compose-mode)
(require 'docker-compose-mode)

;;latex-mode yatex.el
(straight-use-package 'yatex)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))

;;markdown-mode.el
(straight-use-package 'markdown-mode)
(require 'markdown-mode)

;; org-mode.el
(straight-use-package 'org)
(require 'org)
(setq org-directory "~/Google Drive/マイドライブ/Org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))




;;
;;KEY BIND
;;
(straight-use-package 'bind-key)
;;C-h をバックスペースに
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))


;; ウィンドウ移動を方向キーに
(bind-key "M-<right>" 'windmove-right)
(bind-key "M-<left>" 'windmove-left)

;; copilot-mode.el
(bind-key "C-x ac" 'copilot-accept-completion)
;; org-capture.el
(bind-key "C-c c" 'org-capture)
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Google Drive/マイドライブ/Org/notes.org" "Notes")
	 "* %?\nEntered on %U\n %i\n %a")
	))
;; imenu-list
(bind-key "<f10>" 'imenu-list-smart-toggle)
;; helm
(bind-key "C-s" 'helm-occur)
;;
;; TRAMP mode
;;
(straight-use-package 'tramp)
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)) 'append) 

;;
;; magit.el
;;
(straight-use-package 'magit)
(bind-key "C-x g" 'magit-status)


