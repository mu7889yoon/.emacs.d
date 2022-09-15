;; init.el --- mu7889yoon Y.Nakamura

;;
;;STARTUP
;;
(split-window-horizontally)
(other-window 1)
(split-window-vertically)
(other-window 1)
(add-hook 'emacs-startup-hook 'eshell)
;;disable statup message
(setq inhibit-startup-message t)

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
;;company.el
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
(global-company-mode)

;;copilot.el
(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))
(setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node")
(add-hook 'prog-mode-hook 'copilot-mode)
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

;;
;;INTERFACE
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


;;
;;MINOR MODE
;;
;;php-mode
(straight-use-package 'php-mode)
(require 'php-mode)

;;docker-mode.el
(straight-use-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;docker-compose-mode
(straight-use-package 'docker-compose-mode)
(require 'docker-compose-mode)

;;
;;KEY BIND
;;
(straight-use-package 'bind-key)
(bind-key "<tab>" 'my-tab)
(bind-key "TAB" 'my-tab)
(with-eval-after-load 'company
  (bind-key "<tab>" 'my-tab company-active-map)
  (bind-key "TAB" 'my-tab company-active-map)
  (bind-key "<tab>" 'my-tab company-mode-map)
  (bind-key "TAB" 'my-tab company-mode-map))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))


;;
;;VERSION CONTROL
;;

