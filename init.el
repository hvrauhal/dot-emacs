(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(global-auto-revert-mode)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(setq package-initialize-at-startup nil) ; don't do it again

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(defvar my-packages '(rainbow-delimiters
                      clojure-mode
                      markdown-mode
                      cider
                      regex-tool
                      js2-mode
                      better-defaults
                      org
                      use-package
                      leuven-theme
                      editorconfig
		      projectile
                      company
                      smex
                      magit))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)



; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))



(setq inhibit-splash-screen t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(current-language-environment "UTF-8")
 '(git-commit-summary-max-length 130)
 '(indent-tabs-mode nil nil nil "Use spaces only.")
 '(initial-scratch-message nil)
 '(js2-basic-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 2)
 '(org-agenda-files (quote ("~/y/diary.org")))
 '(package-selected-packages
   (quote
    (gnu-elpa-keyring-update ac-js2 ag better-defaults clojure-test-mode coffee-mode color-theme-monokai company company-lua company-qml dash-at-point docker dockerfile-mode editorconfig elpy flx-ido flycheck flycheck-pycheckers gist groovy-mode hipster-theme inf-mongo js2-mode json-mode latex-extra latex-preview-pane less-css-mode leuven-theme lua-mode macrostep magit markdown-mode markdown-preview-mode mediawiki monokai-theme org paredit php-mode pipenv projectile qml-mode rainbow-delimiters regex-tool sass-mode slime smex sql-indent ssh-config-mode use-package yaml-mode)))
 '(sgml-basic-offset 4)
 '(show-paren-mode t)
 '(transient-mark-mode (quote identity))
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))

(require 're-builder)
(setq reb-re-syntax 'string)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 'nil))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(setq python-shell-interpreter "/usr/local/bin/python3")


(global-set-key (kbd "M-S-SPC") 'hippie-expand)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(load-theme 'leuven t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(put 'narrow-to-region 'disabled nil)

(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 130 :weight 'normal)

(defun my-js2-mode-toggle-strict-missing-semi-warning ()
  (interactive)
  (setq js2-strict-missing-semi-warning (eq js2-strict-missing-semi-warning nil))
  (js2-mode))

(setq js-indent-level 2)


; (delete-selection-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


  ;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(put 'downcase-region 'disabled nil)

(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq ediff-split-window-function 'split-window-sensibly)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(server-start)

(global-git-commit-mode)

; Apologize for the lack of path_helper support by just adding the contents of /etc/paths.d/TeX manually
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'ido)
(ido-mode t)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

(size-indication-mode)

;; Lessen the insane escaping in re-builder
(setq reb-re-syntax 'string)
(when (eq window-system 'mac)
  (set-face-attribute 'default nil :family "Hasklig")
  (set-face-attribute 'default nil :height 130)
  (mac-auto-operator-composition-mode))


(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
