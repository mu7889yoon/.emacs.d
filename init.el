;;
;; mu7889yoon Y.Nakamura's Emacs settings
;;



;;
;;START UP SETTING
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
;;FRAMEWORK
;;
;;helm.el
(straight-use-package 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(global-set-key (kbd "C-s") 'helm-occur)

;;
;;TEXT COMPLETION
;;
;;copilot.el
(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))
(setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node")
(setq copilot-mode t)
(add-hook 'prog-mode-hook 'copilot-mode)
;; TABキーを押された時にcopilotで補完する。
;;(defun my/copilot-tab ()
;;  (interactive)
;;  (or (copilot-accept-completion)
;;      (indent-for-tab-command)))

(bind-key "C-x a c" 'copilot-accept-completion)

;; disable inline previews
;; (delq 'company-preview-if-just-one-frontend company-frontends))

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
      company-dabbrev-ignore-case nil)
;;(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)

  
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

;;doom-modeline.el 現在使用しているモードライン
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
;;タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%b %f %& %Z")

;;centaur-tabs.el タブバーを表示
(straight-use-package 'centaur-tabs)
(centaur-tabs-mode t)
(setq centaur-tabs-cycle-scope 'tabs)

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

;;
;;KEY BIND
;;
(straight-use-package 'bind-key)
;;C-h をバックスペースに
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;;centaur-tabs keybind
(bind-key "M-<tab>" 'centaur-tabs-forward)
(bind-key "M-TAB" 'centaur-tabs-forward)
(bind-key "C-M-i" 'centaur-tabs-VERSION)
;;ウィンドウ移動を方向キーに
(bind-key "M-<right>" 'windmove-right)
(bind-key "M-<left>" 'windmove-left)

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

