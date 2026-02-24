;;; project-dashboard.el --- Project-specific dashboard for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cole

;; Author: James Cole
;; Maintainer: James Cole
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit "3.0.0") (nerd-icons "0.1.0"))
;; Homepage: https://github.com/jamescoleuk/project-dashboard
;; Keywords: projects, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A project-specific dashboard that appears when switching projects.
;; Think of it as a landing page for each project showing:
;;
;; - Git status at a glance (branch, staged/unstaged/untracked files)
;; - Recent files in that specific project
;; - Quick actions with keyboard shortcuts
;;
;; Supports .project.el for project-specific metadata:
;;   (:name "My Project" :description "A cool project")
;;
;; Optional: consult for ripgrep search (falls back to project-find-regexp)
;;
;; Usage:
;;   (setq project-switch-commands #'project-dashboard-switch)
;;
;; See README.org for full documentation.

;;; Code:

(require 'project)
(require 'recentf)
(require 'seq)

;; Dependencies - loaded at runtime
(declare-function magit-status "magit-status")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function consult-ripgrep "consult")

;;; Faces

(defgroup project-dashboard nil
  "Project-specific dashboard settings."
  :group 'project
  :prefix "project-dashboard-")

(defface project-dashboard-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for project dashboard section headings."
  :group 'project-dashboard)

(defface project-dashboard-title
  '((t :inherit font-lock-string-face :weight bold :height 1.3))
  "Face for project dashboard title."
  :group 'project-dashboard)

(defcustom project-dashboard-buffer-name "*project-dashboard*"
  "Name of the project dashboard buffer."
  :type 'string)

(defcustom project-dashboard-recent-files-limit 10
  "Number of recent files to show."
  :type 'integer)

(defcustom project-dashboard-git-files-limit 5
  "Number of changed files to show in git status."
  :type 'integer)

;;; Internal variables

(defvar-local project-dashboard--root nil
  "Root directory of the project for this dashboard buffer.")

;;; Section heading constants

(defconst project-dashboard--section-git "Git"
  "Heading text for the git status section.")

(defconst project-dashboard--section-recent "Recent Files"
  "Heading text for the recent files section.")

(defconst project-dashboard--section-actions "Quick Actions"
  "Heading text for the quick actions section.")

;;; Major mode

(defvar project-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Navigation
    (define-key map (kbd "n") #'project-dashboard-next-item)
    (define-key map (kbd "p") #'project-dashboard-prev-item)
    (define-key map (kbd "C-n") #'project-dashboard-next-item)
    (define-key map (kbd "C-p") #'project-dashboard-prev-item)
    (define-key map (kbd "<down>") #'project-dashboard-next-item)
    (define-key map (kbd "<up>") #'project-dashboard-prev-item)
    (define-key map (kbd "RET") #'project-dashboard-activate-item)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "R") #'project-dashboard-refresh)
    map)
  "Keymap for `project-dashboard-mode'.")

(define-derived-mode project-dashboard-mode special-mode "ProjectDash"
  "Major mode for project dashboard buffers.

Displays project information including git status, recent files,
and quick actions.

\\{project-dashboard-mode-map}"
  :group 'project-dashboard
  (setq-local revert-buffer-function #'project-dashboard--revert))

(defun project-dashboard--revert (_ignore-auto _noconfirm)
  "Revert function for project dashboard buffers."
  (when project-dashboard--root
    (project-dashboard-create project-dashboard--root)))

(defun project-dashboard-refresh ()
  "Refresh the current project dashboard, preserving point position."
  (interactive)
  (when project-dashboard--root
    (let ((pos (point)))
      (project-dashboard-create project-dashboard--root)
      (goto-char (min pos (point-max))))))

;;; Git status

(defun project-dashboard--git-status (root)
  "Get git status info for project at ROOT.
Returns plist with :branch :staged :unstaged :untracked or nil if not git repo."
  (let ((default-directory root))
    (when (and (executable-find "git")
               (file-directory-p (expand-file-name ".git" root)))
      (let* ((branch (string-trim
                      (shell-command-to-string "git branch --show-current 2>/dev/null")))
             (status-output (shell-command-to-string "git status --porcelain 2>/dev/null"))
             (lines (split-string status-output "\n" t))
             staged unstaged untracked)
        (dolist (line lines)
          (when (>= (length line) 3)  ; Need at least "XY " format
            (let ((index-status (aref line 0))
                  (worktree-status (aref line 1))
                  (file (string-trim (substring line 3))))
              ;; Staged changes (index has changes)
              (when (memq index-status '(?M ?A ?D ?R ?C))
                (push (cons (char-to-string index-status) file) staged))
              ;; Unstaged changes (worktree has changes)
              (when (memq worktree-status '(?M ?D))
                (push (cons (char-to-string worktree-status) file) unstaged))
              ;; Untracked files
              (when (and (eq index-status ??) (eq worktree-status ??))
                (push file untracked)))))
        (list :branch (if (string-empty-p branch) "HEAD" branch)
              :staged (nreverse staged)
              :unstaged (nreverse unstaged)
              :untracked (nreverse untracked))))))

;;; Recent files

(defun project-dashboard--recent-files (root)
  "Get recent files for project at ROOT from recentf."
  (let ((root-expanded (expand-file-name root)))
    (seq-take
     (seq-filter (lambda (f)
                   (string-prefix-p root-expanded (expand-file-name f)))
                 recentf-list)
     project-dashboard-recent-files-limit)))

;;; Project metadata (reads .project.el)

(defun project-dashboard--read-metadata (dir)
  "Read project metadata from .project.el in DIR.
Resolves symlinks to find the file."
  (let* ((true-dir (file-truename dir))
         (file (expand-file-name ".project.el" true-dir)))
    (when (file-exists-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))))))

;;; Rendering helpers

(defun project-dashboard--insert-centered (text)
  "Insert TEXT centered in the window."
  (let* ((width (window-width))
         (padding (max 0 (/ (- width (length text)) 2))))
    (insert (make-string padding ?\s) text)))

(defun project-dashboard--icon (name &optional face)
  "Return icon for NAME with optional FACE, or empty string if unavailable."
  (if (and (fboundp 'nerd-icons-mdicon) (display-graphic-p))
      (nerd-icons-mdicon name :face (or face 'project-dashboard-heading))
    ""))

(defun project-dashboard--file-icon (filename)
  "Return file icon for FILENAME."
  (if (and (fboundp 'nerd-icons-icon-for-file) (display-graphic-p))
      (concat (nerd-icons-icon-for-file filename) " ")
    ""))

(defun project-dashboard--insert-heading (title icon-name shortcut)
  "Insert section heading with TITLE, ICON-NAME and SHORTCUT key.
Format: icon + title + (shortcut)"
  (insert "\n")
  ;; Icon first
  (let ((icon (project-dashboard--icon icon-name 'project-dashboard-heading)))
    (unless (string-empty-p icon)
      (insert icon " ")))
  ;; Title
  (insert (propertize title 'face 'project-dashboard-heading))
  ;; Shortcut in parentheses
  (when shortcut
    (insert (propertize (format " (%c)" shortcut) 'face 'project-dashboard-heading)))
  (insert "\n"))

(defun project-dashboard--item-prefix (shortcut)
  "Return the prefix string for an item with optional SHORTCUT."
  (if shortcut
      (format "    %c " shortcut)
    "      "))

(defun project-dashboard--format-git-indicator (type)
  "Return a formatted indicator for git change TYPE."
  (pcase type
    ("M" (propertize "M" 'face 'warning))
    ("A" (propertize "A" 'face 'success))
    ("D" (propertize "D" 'face 'error))
    ("R" (propertize "R" 'face 'warning))
    (_ (propertize "?" 'face 'shadow))))

;;; Navigation

(defun project-dashboard-next-item ()
  "Move to the next item (button) in the dashboard."
  (interactive)
  (let ((pos (next-button (point))))
    (when pos
      (goto-char (button-start pos)))))

(defun project-dashboard-prev-item ()
  "Move to the previous item (button) in the dashboard."
  (interactive)
  (let ((pos (previous-button (point))))
    (when pos
      (goto-char (button-start pos)))))

(defun project-dashboard-activate-item ()
  "Activate the item (button) at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (button-activate button)
      (user-error "No item at point"))))

;;; Main dashboard creation

(defun project-dashboard--magit-status-safe (root)
  "Open magit status for ROOT, or show error if magit unavailable."
  (if (fboundp 'magit-status)
      (magit-status root)
    (user-error "Magit is not installed")))

(defun project-dashboard--insert-file-button (file full-path)
  "Insert a button for FILE that opens FULL-PATH when clicked."
  (let ((path full-path))
    (insert-text-button file
                        'action (lambda (_) (find-file path))
                        'follow-link t
                        'face 'default)))

(defun project-dashboard-create (root)
  "Create and display the project dashboard for ROOT.

This creates a buffer showing:
- Project title and description (from .project.el if present)
- Git status with staged, unstaged, and untracked files
- Recent files in the project
- Quick actions (find file, search, magit, dired, eshell)

Buffer-local keybindings are set up for navigation and actions.
See `project-dashboard-mode' for the full keymap.

The dashboard can be refreshed with \\`R' or `\\[revert-buffer]'."
  (let* ((buf (get-buffer-create project-dashboard-buffer-name))
         (meta (project-dashboard--read-metadata root))
         (project-name (or (plist-get meta :name)
                           (file-name-nondirectory (directory-file-name root))))
         (git-info (project-dashboard--git-status root))
         (recent-files (project-dashboard--recent-files root)))

    (with-current-buffer buf
      ;; Set up mode first (this gives us the base keymap)
      (unless (derived-mode-p 'project-dashboard-mode)
        (project-dashboard-mode))

      ;; Store root for refresh functionality
      (setq-local project-dashboard--root root)

      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory root)

        ;; === Title ===
        ;; Two newlines for visual spacing at top
        (insert "\n\n")
        (project-dashboard--insert-centered
         (propertize project-name 'face 'project-dashboard-title))
        (insert "\n")
        (project-dashboard--insert-centered
         (propertize (abbreviate-file-name root) 'face 'shadow))
        (when-let* ((desc (plist-get meta :description)))
          (insert "\n")
          (project-dashboard--insert-centered
           (propertize desc 'face 'font-lock-comment-face)))
        (insert "\n")

        ;; === Git Status Section ===
        (when git-info
          (project-dashboard--insert-heading
           (format "%s: %s" project-dashboard--section-git (plist-get git-info :branch))
           "nf-md-source_branch"
           ?g)

          ;; Bind g to magit (with safety check)
          (let ((root-copy root))
            (local-set-key (kbd "g")
              (lambda () (interactive)
                (project-dashboard--magit-status-safe root-copy))))

          (let ((staged (plist-get git-info :staged))
                (unstaged (plist-get git-info :unstaged))
                (untracked (plist-get git-info :untracked))
                (shown 0)
                (limit project-dashboard-git-files-limit))

            ;; Show staged files
            (dolist (item (seq-take staged limit))
              (when (< shown limit)
                (let* ((file (cdr item))
                       (type (car item))
                       (full-path (expand-file-name file root)))
                  (insert "    ")
                  (insert (project-dashboard--file-icon file))
                  (insert (propertize "staged " 'face 'success))
                  (insert (project-dashboard--format-git-indicator type) " ")
                  (project-dashboard--insert-file-button file full-path)
                  (insert "\n"))
                (setq shown (1+ shown))))

            ;; Show unstaged files
            (dolist (item (seq-take unstaged (max 0 (- limit shown))))
              (when (< shown limit)
                (let* ((file (cdr item))
                       (type (car item))
                       (full-path (expand-file-name file root)))
                  (insert "    ")
                  (insert (project-dashboard--file-icon file))
                  (insert (propertize "unstaged " 'face 'warning))
                  (insert (project-dashboard--format-git-indicator type) " ")
                  (project-dashboard--insert-file-button file full-path)
                  (insert "\n"))
                (setq shown (1+ shown))))

            ;; Show untracked files
            (dolist (file (seq-take untracked (max 0 (- limit shown))))
              (when (< shown limit)
                (let ((full-path (expand-file-name file root)))
                  (insert "    ")
                  (insert (project-dashboard--file-icon file))
                  (insert (propertize "untracked " 'face 'shadow))
                  (insert (propertize "?" 'face 'shadow) " ")
                  (project-dashboard--insert-file-button file full-path)
                  (insert "\n"))
                (setq shown (1+ shown))))

            ;; Summary line
            (let ((total (+ (length staged) (length unstaged) (length untracked))))
              (insert "\n    ")
              (insert (propertize
                       (format "%d staged, %d unstaged, %d untracked"
                               (length staged) (length unstaged) (length untracked))
                       'face 'shadow))
              (when (> total limit)
                (insert "   ")
                (let ((root-copy root))
                  (insert-text-button "...more"
                                      'action (lambda (_)
                                                (project-dashboard--magit-status-safe root-copy))
                                      'follow-link t
                                      'face 'link)))
              (insert "\n"))))

        ;; === Recent Files Section ===
        (project-dashboard--insert-heading
         (concat project-dashboard--section-recent ":") "nf-md-clock_outline" ?r)

        ;; Bind r to jump to Recent Files section
        (let ((section-name project-dashboard--section-recent))
          (local-set-key (kbd "r")
            (lambda () (interactive)
              (goto-char (point-min))
              (search-forward section-name nil t)
              (forward-line 1))))

        (if recent-files
            (let ((shortcut-char ?1)
                  (idx 0))
              (dolist (file recent-files)
                (let* ((rel-path (file-relative-name file root))
                       (sc (when (< idx 9) shortcut-char))
                       (file-copy file))
                  ;; Bind number key
                  (when sc
                    (local-set-key (kbd (char-to-string sc))
                      (lambda () (interactive) (find-file file-copy))))
                  ;; Insert item - shortcut then icon then path
                  (insert (project-dashboard--item-prefix sc))
                  (insert (project-dashboard--file-icon rel-path))
                  (project-dashboard--insert-file-button rel-path file-copy)
                  (insert "\n")
                  (when sc (setq shortcut-char (1+ shortcut-char)))
                  (setq idx (1+ idx)))))
          (insert "    ")
          (insert (propertize "No recent files in this project" 'face 'shadow))
          (insert "\n"))

        ;; === Quick Actions Section ===
        (project-dashboard--insert-heading
         (concat project-dashboard--section-actions ":") "nf-md-lightning_bolt" ?a)

        ;; Bind a to jump to actions section
        (let ((section-name project-dashboard--section-actions))
          (local-set-key (kbd "a")
            (lambda () (interactive)
              (goto-char (point-min))
              (search-forward section-name nil t)
              (forward-line 1))))

        ;; Define actions with their shortcuts
        (let ((root-copy root))
          ;; Find file
          (local-set-key (kbd "f")
            (lambda () (interactive)
              (let ((default-directory root-copy))
                (project-find-file))))
          (insert (project-dashboard--item-prefix ?f))
          (insert (project-dashboard--file-icon "file.txt"))
          (insert-text-button "Find file"
                              'action (lambda (_)
                                        (let ((default-directory root-copy))
                                          (project-find-file)))
                              'follow-link t
                              'face 'default)
          (insert (propertize " - C-c p f" 'face 'shadow) "\n")

          ;; Search (ripgrep or grep)
          (local-set-key (kbd "s")
            (lambda () (interactive)
              (if (fboundp 'consult-ripgrep)
                  (consult-ripgrep root-copy)
                (project-find-regexp (read-string "Search regexp: ")))))
          (insert (project-dashboard--item-prefix ?s))
          (insert (project-dashboard--icon "nf-md-text_search") " ")
          (insert-text-button (if (fboundp 'consult-ripgrep) "Search (ripgrep)" "Search (regexp)")
                              'action (lambda (_)
                                        (if (fboundp 'consult-ripgrep)
                                            (consult-ripgrep root-copy)
                                          (project-find-regexp (read-string "Search regexp: "))))
                              'follow-link t
                              'face 'default)
          (insert (propertize (if (fboundp 'consult-ripgrep) " - C-c s" "") 'face 'shadow) "\n")

          ;; Magit status (only show if magit available)
          (when (fboundp 'magit-status)
            (local-set-key (kbd "m")
              (lambda () (interactive) (magit-status root-copy)))
            (insert (project-dashboard--item-prefix ?m))
            (insert (project-dashboard--icon "nf-md-source_branch") " ")
            (insert-text-button "Magit status"
                                'action (lambda (_) (magit-status root-copy))
                                'follow-link t
                                'face 'default)
            (insert (propertize " - C-x g" 'face 'shadow) "\n"))

          ;; Dired
          (local-set-key (kbd "d")
            (lambda () (interactive) (dired root-copy)))
          (insert (project-dashboard--item-prefix ?d))
          (insert (project-dashboard--icon "nf-md-folder") " ")
          (insert-text-button "Dired"
                              'action (lambda (_) (dired root-copy))
                              'follow-link t
                              'face 'default)
          (insert (propertize " - C-x d" 'face 'shadow) "\n")

          ;; Eshell
          (local-set-key (kbd "e")
            (lambda () (interactive)
              (let ((default-directory root-copy))
                (project-eshell))))
          (insert (project-dashboard--item-prefix ?e))
          (insert (project-dashboard--icon "nf-md-console") " ")
          (insert-text-button "Eshell"
                              'action (lambda (_)
                                        (let ((default-directory root-copy))
                                          (project-eshell)))
                              'follow-link t
                              'face 'default)
          (insert (propertize " - C-c p e" 'face 'shadow) "\n"))

        ;; === Footer ===
        ;; Two newlines for visual separation
        (insert "\n\n")
        (project-dashboard--insert-centered
         (propertize "q quit  R refresh" 'face 'shadow))
        (insert "\n")

        ;; Position cursor below title (skip 2 blank lines)
        (goto-char (point-min))
        (forward-line 2))

      (setq truncate-lines t))

    (switch-to-buffer buf)))

;;; Interactive commands

;;;###autoload
(defun project-dashboard-open ()
  "Prompt for a project and open its dashboard."
  (interactive)
  (let* ((project (project-prompt-project-dir))
         (root (project-root (project-current nil project))))
    (project-dashboard-create root)))

;;;###autoload
(defun project-dashboard-open-frame ()
  "Prompt for a project and open its dashboard in a new frame."
  (interactive)
  (let* ((project (project-prompt-project-dir))
         (root (project-root (project-current nil project))))
    (select-frame (make-frame))
    (project-dashboard-create root)))

;;;###autoload
(defun project-dashboard-current ()
  "Open dashboard for the current project."
  (interactive)
  (if-let ((proj (project-current)))
      (project-dashboard-create (project-root proj))
    (user-error "Not in a project")))

;;;###autoload
(defun project-dashboard-switch ()
  "Open dashboard for current project. For use with `project-switch-commands'."
  (interactive)
  (project-dashboard-create (project-root (project-current t))))

(provide 'project-dashboard)

;;; project-dashboard.el ends here
