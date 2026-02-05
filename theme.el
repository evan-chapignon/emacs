(setq custom-safe-themes t)
;; (load-theme 'modus-vivendi)


;; (load-theme 'gruvbox-dark-medium)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t ; Utilise la variable-pitch pour Org-mode
      modus-themes-variable-pitch-ui t) ; Interface plus douce

(setq modus-themes-common-palette-overrides
      '((cursor "#bd93f9")
        (fg-line-number-active "#bd93f9")
        (border-focused "#bd93f9")
        (heading-1 "#bd93f9")
        (bg-region "#44475a") 
        (accent-0 "#bd93f9")
        (accent-1 "#bd93f9")))

(load-theme 'modus-vivendi t)

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2 :foreground "#bd93f9" :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1 :foreground "#bd93f9"))))
 '(org-document-title ((t (:height 1.5 :foreground "#bd93f9" :weight bold))))) 
