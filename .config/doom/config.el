;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "Chu the Pup"
      user-mail-address "chufilthymutt@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/nextcloud/documents/org")

(setq org-agenda-files '("~/nextcloud/documents/org/roam/20221004221829-todo.org"
                         "~/nextcloud/documents/org/roam/20220823133453-precalculus_algebra.org"
                         "~/nextcloud/documents/org/roam/20220826102101-chem_1110.org"
                         "~/nextcloud/documents/org/roam/20220726210346-important_dates.org"
                         "~/nextcloud/documents/org/roam/20221004222235-notes.org"
                         "~/nextcloud/documents/org/roam/20221004222230-journal.org"
                         "~/nextcloud/documents/org/roam/20221004222226-projects.org"
                         "~/nextcloud/documents/org/roam/20220822103202-engl_1020.org"
                         "~/nextcloud/documents/org/roam/20221002161620-my_conlang.org"))

(setq +org-capture-bookmarks-file "~/nextcloud/documents/org/roam/20221004090130-bookmarks.org")

(setq org-roam-directory "~/nextcloud/documents/org/roam")

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(setq +org-capture-journal-file "~/nextcloud/documents/org/roam/20221004222230-journal.org")

(setq +org-capture-notes-file "~/nextcloud/documents/org/roam/20221004222235-notes.org")

(setq +org-capture-projects-file "~/nextcloud/documents/org/roam/20221004222226-projects.org")

(setq +org-capture-todo-file "~/nextcloud/documents/org/roam/20221004221829-todo.org")

(setq org-id-locations-file "~/nextcloud/documents/org/.orgids")

(require 'org-roam-protocol)

(require 'org-roam-export)

(setq org-attach-id-dir "~/nextcloud/documents/org/.attach")

(setq! org-cite-global-bibliography "~/nextcloud/documents/org/roam/bib.bib")

(setq org-cite-csl-styles-dir "~/nextcloud/documents/org/latex/citeproc-formatters/")

(setq citar-bibliography "~/nextcloud/documents/org/roam/bib.bib")

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "HABIT(H)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(with-eval-after-load 'org
  (require 'org-download)
  (add-hook 'dired-mode-hook 'org-download-enable))

(setq org-image-actual-width 500)

(with-eval-after-load 'ox-latex
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
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-archive-default-command 'org-archive-subtree-hierarchical)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(add-load-path! "~/.config/doom/elisp")

(require 'org-archive-subtree-hierarchical)

(setq erc-server "localhost"
      erc-nick "chuthepup"
      erc-user-full-name "Chu the Pup")

(setq delete-by-moving-to-trash t)

(setq dired-backup-overwrite t)

(setq image-use-external-converter t)

(require 'random-splash-image)

(setq random-splash-image-dir "~/.local/share/random-splash-image-dir/chosen-splash-images/src/")

(with-eval-after-load 'random-splash-image
  (random-splash-image-set))

(setq skeletor-project-directory "~/nextcloud/projects/")

(when (and (not (executable-find "fd"))
           (executable-find "rg"))
  (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (setq rg-ignorefile
                (concat "--ignore-file" " "
                        (expand-file-name "rg_ignore" user-emacs-directory)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd " " rg-ignorefile))))

(elcord-mode)

(achievements-mode)

(setq browse-url-firefox-arguments "-P chu")
(setq browse-url-firefox-new-window-is-tab t)

(use-package! literate-calc-mode :defer t)

(load! (expand-file-name "~/.local/share/roswell/helper.el"))

(use-package! slime
  :defer t ; don't load the package immediately
  :init ; runs this immediately
  (setq inferior-lisp-program "sbcl")
  :config ; runs this when slime loads
  (set-repl-handler! 'lisp-mode #'sly-mrepl)
  (set-eval-handler! 'lisp-mode #'sly-eval-region)
  (set-lookup-handlers! 'lisp-mode
    :definition #'sly-edit-definition
    :documentation #'sly-describe-symbol)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(require 'common-lisp-snippets)
