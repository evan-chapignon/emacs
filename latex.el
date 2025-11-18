(with-eval-after-load 'ox-latex
  ;; Définir la classe
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Activer listings
  (setq org-latex-listings t)
  (setq org-latex-src-block-backend 'listings)

  ;; Ajouter les packages nécessaires
  (add-to-list 'org-latex-packages-alist '("" "xcolor" t))
  (add-to-list 'org-latex-packages-alist '("" "framed" t))

  ;; Personnalisation globale des listings
  (setq org-latex-listings-options
        '(("basicstyle" "\\ttfamily\\small")      ;; petite fonte typewriter
          ("backgroundcolor" "\\color[gray]{0.95}") ;; gris clair
          ("xleftmargin" "1em")                  ;; marge gauche
          ("xrightmargin" "1em")                 ;; marge droite
          ("aboveskip" "1em")                    ;; espace avant
          ("belowskip" "1em")                    ;; espace après
          ("breaklines" "true")                  ;; retour à la ligne automatique
          ("numbers" "left")                     ;; numéroter les lignes
          ("numberstyle" "\\tiny")               ;; taille des numéros
          ("showstringspaces" "false"))))
