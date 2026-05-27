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
(setq dashboard-center-content t)
  (setq dashboard-startup-banner 'official)

  ;; (use-package mood-line
  ;;   :config
  ;;   (mood-line-mode)
    
  ;;   (display-battery-mode 1)
  ;;   (display-time-mode 1)
    
  ;;   (setq display-time-format "%H:%M" 
  ;;       	display-time-default-load-average nil)
    
  ;;   (setq battery-mode-line-format " [BAT%p%%]"))

  (set-face-attribute '
   default nil :font "IosevkaNerdFont" :height 110)
  (set-face-attribute 'variable-pitch nil :font "IosevkaNerdFont" :height 110)

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

(require 'imenu)
  (require 'imenu-list)

  (defun my/org-tree-to-indirect-buffer ()
    "Create indirect buffer, narrow it to current subtree and unfold blocks"
    
    (org-tree-to-indirect-buffer)
    (org-show-block-all)
    (setq-local my/org-blocks-hidden nil))

  (defun my/org-sidebar ()
    "Open an imenu list on the left that allow navigation."
    
    (interactive)
    (setq imenu-list-after-jump-hook #'my/org-tree-to-indirect-buffer
          imenu-list-position 'left
          imenu-list-size 36
          imenu-list-focus-after-activation t)

    (let ((heading (substring-no-properties (or (org-get-heading t t t t) ""))))
      (when (buffer-base-buffer)
        (switch-to-buffer (buffer-base-buffer)))
      (imenu-list-minor-mode)
      (imenu-list-stop-timer)
      (hl-line-mode)
      (face-remap-add-relative 'hl-line :inherit 'nano-subtle)
      (setq header-line-format
            '(:eval
              (nano-modeline-render nil
                                    (buffer-name imenu-list--displayed-buffer)
                                    "(outline)"
                                    "")))
      (setq-local cursor-type nil)
      (when (> (length heading) 0)
        (goto-char (point-min))
        (search-forward heading)
        (imenu-list-display-dwim))))

  
(defun my/org-sidebar-toggle ()
  "Toggle the org-sidebar"
  
  (interactive)
  (if (get-buffer-window "*Ilist*")
      (progn 
        (quit-window nil (get-buffer-window "*Ilist*"))
        (switch-to-buffer (buffer-base-buffer)))
    (my/org-sidebar)))

(delete-selection-mode t)
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

(use-package expand-region
  :bind ("C-:" . er/expand-region))

(use-package orderless
:ensure t
:init
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package company
  :hook (after-init . global-company-mode))

(add-hook 'LaTeX-mode-hook 'company-mode)
(require 'lsp-latex)
(add-hook 'LaTeX-mode-hook #'lsp)
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
              (set-face-attribute 'variable-pitch nil :font "IosevkaNerdFont" :height 110))))

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
(load "~/.config/emacs/publish.el")
(load "~/.config/emacs/latex.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0blayout @ auctex avy brief catppuccin-theme chess company-math
              copilot dashboard docker-compose-mode dockerfile-mode
              dracula-theme ement evil expand-region gptel
              gruvbox-theme htmlize imenu-extra imenu-list ivy
              lsp-latex magit marginalia monkeytype mood-line mu4e
              multiple-cursors nano-modeline nix-mode nord-theme nov
              olivetti orderless org-appear org-modern pdf-tools
              rainbow-delimiters solarized-theme spray vertico
              yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
