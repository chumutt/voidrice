;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "chu the pup"
      user-mail-address "chu@dogboner.xyz")

(add-hook! org-mode :append
           #'abbrev-mode)

(setq ispell-alternate-dictionary "/usr/share/dict")

(use-package! palimpsest-mode
  :hook (prog-mode . palimpsest-mode))

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(setq auth-source-save-behavior nil)

(setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

(setq emms-source-file-default-directory "~/Music/")

(setq erc-server "localhost"
      erc-nick "chuthepup"
      erc-user-full-name "Chu the Pup")

(setq dired-backup-overwrite t)

(setq delete-by-moving-to-trash t)

(setq org-log-into-drawer "LOGBOOK")

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-todo-keywords
       '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "HABIT(H)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
         (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
         (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  ;;;###autoload
  (defun unpackaged/org-fix-blank-lines (&optional prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
  With prefix, operate on whole buffer. Ensures that blank lines
  exist after each headings's drawers."
    (interactive "P")
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
                         ;; Insert blank lines before entry content
                         (forward-line)
                         (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree)))

(after! 'org
  (setq org-directory
        (concat
         (getenv "HOME")
        "/nextcloud/documents/org/")))

(with-eval-after-load 'org
  (setq +org-capture-bookmarks-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004090130-bookmarks.org")))

(setq org-agenda-files
      '("/home/chu/nextcloud/documents/org/roam/20220726210347-important_dates.org"
        "/home/chu/nextcloud/documents/org/roam/20221004221831-todo.org"
        "/home/chu/nextcloud/documents/org/roam/20220823133456-precalculus_algebra.org"
        "/home/chu/nextcloud/documents/org/roam/20220826102105-chem_1115.org"
        "/home/chu/nextcloud/documents/org/roam/20221004222241-notes.org"
        "/home/chu/nextcloud/documents/org/roam/20221004222237-journal.org"
        "/home/chu/nextcloud/documents/org/roam/20221004222234-projects.org"
        "/home/chu/nextcloud/documents/org/roam/20220822103211-engl_1030.org"
        "/home/chu/nextcloud/documents/org/roam/20221002161631-my_conlang.org"))

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

(setq org-cite-global-bibliography
       (list
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/bib.bib")))

(setq org-cite-csl-styles-dir
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/latex/citeproc-formatters/"))

(with-eval-after-load 'org
  (require 'org-download)
  (add-hook 'dired-mode-hook 'org-download-enable))

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

(use-package! org-tanglesync
  :hook ((org-mode . org-tanglesync-mode)
         ;; enable watch-mode globally:
         ((prog-mode text-mode) . org-tanglesync-watch-mode))
  ;; :custom
  ;; (org-tanglesync-watch-files '("example.org"))
  :bind
  (( "C-c M-i" . org-tanglesync-process-buffer-interactive)
   ( "C-c M-a" . org-tanglesync-process-buffer-automatic)))

(setq doom-font (font-spec :family "Mono" :size 12))

(setq image-use-external-converter t)

(require 'random-splash-image)

(setq random-splash-image-dir
      (concat
       (getenv "HOME")
       "/.local/share/random-splash-image-dir/chosen-splash-images/src/"))

(with-eval-after-load 'random-splash-image
  (random-splash-image-set))

(elcord-mode)

(setf epg-pinentry-mode 'loopback)
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd
              (concat (replace-regexp-in-string "%22" "\""
                      (replace-regexp-in-string "%0A" "\n" desc)) prompt ": ")))) str))

(setq ange-ftp-netrc-filename "~/.authinfo.gpg")

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

(defun kb/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 75))
    (pcase (frame-parameter nil 'alpha-background)
      (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
      (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
     (if (numberp alpha)
         alpha
       (cdr alpha)) ; may also be nil
     100)
    (set-frame-parameter nil 'alpha '(85 . 50))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(defun toggle-background-transparency ()
  "Toggle background transparency, wherein text and other elements in frame are still displayed but a background isn't."
  (interactive)
  (if (get 'toggle-background-transparency 'state)
      (progn
        (set-frame-parameter nil 'alpha-background 100)
        (put 'toggle-background-transparency 'state nil))
    (progn
      (set-frame-parameter nil 'alpha-background 35)
      (put 'toggle-background-transparency 'state t))))

(set-frame-parameter (selected-frame) 'alpha 99) ; set current frame
(add-to-list 'default-frame-alist '(alpha 99)) ; set all frames from this point on

(setq inhibit-x-resources t) ; inhibit .xresources file from being loaded on emacs init

(use-package! literate-calc-mode
  :defer t)

(defun +lisp/find-file-in-quicklisp ()
  "Find a file belonging to a library downloaded by Quicklisp."
  (interactive)
  (doom-project-find-file "~/.local/share/roswell/lisp/quicklisp/dists"))

(load! (expand-file-name "~/.local/share/roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run")

(require 'common-lisp-snippets)

(defun dired-rsync-skip-newer (dest)
  "Asynchronously copy files in dired to `DEST' using rsync
set to resolve symlinks, skip files that are newer in `DEST',
and to run in archive mode."
  (interactive
   (list (read-file-name "rsync to: " (dired-dwim-target-directory)
                         nil nil nil 'file-directory-p)))
  (let ((dired-rsync-options "-aLuz --info=progress2"))
    (dired-rsync dest)))

(map! "C-c C-d C-r" #'dired-rsync-skip-newer)
