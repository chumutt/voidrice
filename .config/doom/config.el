;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chu the Pup"
      user-mail-address "chufilthymutt@gmail.com")

(setq erc-server "localhost"
      erc-nick "chuthepup"
      erc-user-full-name "Chu the Pup")

(setq delete-by-moving-to-trash t)

(setq dired-backup-overwrite t)

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-todo-keywords
       '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "HABIT(H)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
         (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
         (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(with-eval-after-load 'org
  (setq org-directory
        (concat
         (getenv "HOME")
        "/nextcloud/documents/org/")))

(with-eval-after-load 'org
  (setq +org-capture-bookmarks-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004090130-bookmarks.org")))

(with-eval-after-load 'org
  (setq org-agenda-files
        (list
         (concat
          (getenv "HOME")
          "/nextcloud/documents/org/roam/20221004222223-agenda.org"))))

(with-eval-after-load 'org
  (setq +org-capture-journal-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004222230-journal.org")))

(with-eval-after-load 'org
  (setq +org-capture-notes-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004222235-notes.org")))

(with-eval-after-load 'org
  (setq +org-capture-projects-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004222226-projects.org")))

(with-eval-after-load 'org
  (setq +org-capture-todo-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004221829-todo.org")))

(with-eval-after-load 'org
  (setq org-roam-directory
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/")))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(require 'org-roam-protocol)

(require 'org-roam-export)

(setq org-id-locations-file
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/.orgids"))

(setq org-attach-id-dir
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/.attach/"))

(setq! org-cite-global-bibliography
       (list
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/bib.bib")))

(setq org-cite-csl-styles-dir
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/latex/citeproc-formatters/"))

(setq citar-bibliography
       (list
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/bib.bib")))

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

(provide 'org-archive-subtree-hierarchical)
(require 'org-archive)
(defun org-archive-subtree-hierarchical--line-content-as-string ()
  "Returns the content of the current line as a string"
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))
(defun org-archive-subtree-hierarchical--org-child-list ()
  "This function returns all children of a heading as a list. "
  (interactive)
  (save-excursion
    ;; this only works with org-version > 8.0, since in previous
    ;; org-mode versions the function (org-outline-level) returns
    ;; gargabe when the point is not on a heading.
    (if (= (org-outline-level) 0)
        (outline-next-visible-heading 1)
      (org-goto-first-child))
    (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
      child-list)))
(defun org-archive-subtree-hierarchical--org-struct-subtree ()
  "This function returns the tree structure in which a subtree belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))
(defun org-archive-subtree-hierarchical ()
  "This function archives a subtree hierarchical"
  (interactive)
  (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
        (this-buffer (current-buffer))
        (file (abbreviate-file-name
               (or (buffer-file-name (buffer-base-buffer))
                   (error "No file associated to buffer")))))
    (save-excursion
      (setq location org-archive-location
            afile (car (org-archive--compute-location
                        (or (org-entry-get nil "ARCHIVE" 'inherit) location)))
            ;; heading (org-extract-archive-heading location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (> (length afile) 0)
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                buffer (or visiting (find-file-noselect afile)))
        (setq buffer (current-buffer)))
      (unless buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer buffer)
      (org-mode)
      (goto-char (point-min))
      (while (not (equal org-tree nil))
        (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
          (if (member (car org-tree) child-list)
              (progn
                (search-forward (car org-tree) nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (goto-char (point-max))
              (newline)
              (org-insert-struct org-tree)
              (setq org-tree nil)))))
      (newline)
      (org-yank)
      (when (not (eq this-buffer buffer))
        (save-buffer))
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))
(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (newline)
    (org-insert-struct (cdr struct))))
(defun org-archive-subtree ()
  (org-archive-subtree-hierarchical))

(setq org-archive-default-command 'org-archive-subtree-hierarchical)

(with-eval-after-load 'org
  (setq org-agenda-files '("~/nextcloud/documents/org/roam/20221004221829-todo.org"
                           "~/nextcloud/documents/org/roam/20220823133453-precalculus_algebra.org"
                           "~/nextcloud/documents/org/roam/20220826102101-chem_1110.org"
                           "~/nextcloud/documents/org/roam/20220726210346-important_dates.org"
                           "~/nextcloud/documents/org/roam/20221004222235-notes.org"
                           "~/nextcloud/documents/org/roam/20221004222230-journal.org"
                           "~/nextcloud/documents/org/roam/20221004222226-projects.org"
                           "~/nextcloud/documents/org/roam/20220822103202-engl_1020.org"
                           "~/nextcloud/documents/org/roam/20221002161620-my_conlang.org")))

(setq image-use-external-converter t)

(require 'random-splash-image)

(setq random-splash-image-dir
      (concat
       (getenv "HOME") "/.local/share/random-splash-image-dir/chosen-splash-images/src/"))

(with-eval-after-load 'random-splash-image
  (random-splash-image-set))

(setq skeletor-project-directory
      (concat
       (getenv "HOME")
       "/nextcloud/projects/"))

(after! projectile
        (setq projectile-ignored-projects '("~/" "/run/media/root/grandarchive")))

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

(defun get-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789.-")
  (or (looking-at "[0123456789.-]+")
      (error "No number at point"))
  (string-to-number (match-string 0)))

(defun round-number-at-point-to-decimals (decimal-count)
  (interactive "NDecimal count: ")
  (let ((mult (expt 10 decimal-count)))
    (replace-match (number-to-string
              (/
               (fround
                (*
                 mult
                 (get-number-at-point)))
                mult)))))

(setq browse-url-firefox-arguments "-P chu")
(setq browse-url-firefox-new-window-is-tab t)

(use-package! literate-calc-mode
  :defer t)

(defun +lisp/find-file-in-quicklisp ()
  "Find a file belonging to a library downloaded by Quicklisp."
  (interactive)
  (doom-project-find-file "~/.local/share/roswell/lisp/quicklisp/dists"))

(load! (expand-file-name "~/.local/share/roswell/helper.el"))

(require 'common-lisp-snippets)

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
