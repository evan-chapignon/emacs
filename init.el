(setq use-package-always-ensure t)
(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create
      									"*dashboard*")))
(add-hook 'server-after-make-frame-hook 'dashboard-refresh-buffer)

(setq dashboard-startup-banner "~/.config/emacs/ben.png")

(use-package mood-line
  :config
  (mood-line-mode)
  
  (display-battery-mode 1)
  (display-time-mode 1)
  
  (setq display-time-format "%H:%M" 
      	display-time-default-load-average nil)
  
  (setq battery-mode-line-format " [BAT%p%%]"))

(set-face-attribute 'default nil :font "Iosevka" :height 110)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 110)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq frame-resize-pixelwise t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq visible-bell t)

(use-package olivetti
:ensure t
  :custom
  (olivetti-set-width 150))

(delete-selection-mode t)
(display-battery-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-font-lock-mode 1)
(pixel-scroll-precision-mode -1)
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(which-key-mode 1)
(setq bookmark-save-flag 1)
(setq-default left-margin-width 1 right-margin-width 1)
(setq-default line-spacing 0.2)
(setq-default tab-width 4)
(setq scroll-margin 5)
(setq user-mail-address "evan.delepine-gengembre.etu@univ-lille.fr"
	  user-full-name "Evan Delepine")
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq history-length 25)

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq-default auto-fill-function 'do-auto-fill)
(setq ring-bell-function 'ignore)

(defun ouvrir-config-emacs ()
  "Ouvre... la config emacs"
  (interactive)
  (find-file "~/.config/emacs/init.org"))
(global-set-key (kbd "C-c e") 'ouvrir-config-emacs)


(defun other-other-window ()
  "Passe à la fenêtre précédente."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<iso-lefttab>") 'other-other-window)

(global-set-key (kbd "C-c C-r") 'recentf-open-files)

(global-set-key (kbd "C-:") 'er/expand-region)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-x C-y") 'compile)


;; Pas vraiment une fonction, mais ouvre vterm dans un buffer sous le
;;  buffer actuel, un peu comme vscode
(add-to-list 'display-buffer-alist
  			 '("\\*vterm\\*"
               (display-buffer-reuse-window
  				display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.3)))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("C-c m" . consult-imenu)))

(use-package avy
  :bind (("C-o" . avy-goto-char)
		 ("C-c l" . avy-copy-line)
		 ("C-c s" . avy-goto-line)
		 ))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package company
  :hook (after-init . global-company-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
		'("~/.config/emacs/snippets"))
  (yas-global-mode 1))

(use-package org
  :ensure t
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-modern-mode))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("M-<return>" . org-meta-return))
  :custom
  (org-directory "~/org")
  (org-default-notes-file "~/org/capture.org")
  (org-agenda-files '("~/org/TODO/todo.org" "~/org/edt.org")) 
  
  (org-agenda-span 30) 
  (org-ellipsis "…")
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-startup-indented t)
  
  (org-capture-templates
   '(("t" "Tâche" entry
      (file+headline "~/org/TODO/todo.org" "Tâches")
      "* TODO %?\nSCHEDULED: %^t\n%u\n%a"
      :empty-lines 1))) 
  
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (latex . t)
     (java . t)
     (org . t)))

  (add-hook 'org-mode-hook
            (lambda ()
              (set-face-attribute 'variable-pitch nil :font "Iosevka" :height 110))))

(use-package org-modern
  :ensure t
  :after org
  :custom
  (org-modern-star '("◉" "○" "✸" "✿" "◆"))
  (org-modern-hide-stars t)
  (org-modern-table nil)
  (org-modern-checkbox '((?X . "☑") (?- . "❍") (?\s . "☐"))) 
  :config
  (global-org-modern-mode))

(load "~/.config/emacs/theme.el")
(load "~/.config/emacs/latex.el")
