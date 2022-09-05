;; ######################################## INSTALL PACKAGES ######################################
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p `better-defaults) (package-install `better-defaults))
(unless (package-installed-p `material-theme) (package-install `material-theme))
(unless (package-installed-p `auto-complete) (package-install `auto-complete))


;; ##################################### BASIC CUSTOMIZATION ######################################
(setq inhibit-startup-message t) ;; hide the startup message
;; (load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(set-default 'truncate-lines t) ;; do not wrap
(prefer-coding-system 'utf-8) ;; use UTF-8


;;### Shell configuration
(use-package exec-path-from-shell
  :init
  (setenv "SHELL" "/bin/zsh")
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))


;;### Faked full screen
(use-package maxframe
     :ensure t)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))
;; (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen) ;; conflicts with an auctex command to insert an \item in a list.


;;### font size in the modeline
(set-face-attribute 'mode-line nil  :height 240)


;;### hippie-expand
(global-set-key [remap dabbrev-expand]  'hippie-expand)


; ;; Set copy+paste
;  (cua-mode t)
;     (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;     (transient-mark-mode 1) ;; No region when it is not highlighted
;     (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; REMOVE THE SCRATCH BUFFER AT STARTUP
;; Makes *scratch* empty.
;; (setq initial-scratch-message "")
;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)


 
;; Revert Dired and other buffers after changes to files in directories on disk.
;; Source: [[https://www.youtube.com/watch?v=51eSeqcaikM&list=PLEoMzSkcN8oNmd98m_6FoaJseUsa6QGm2&index=2][Dave Wilson]]
(setq global-auto-revert-non-file-buffers t)


;; customize powerline
;; (line above the command line at the bottom of the screen)
(use-package powerline
  :ensure t)
(require 'powerline)
(powerline-default-theme)

;; highlight current line
(global-hl-line-mode +1)
(set-face-background hl-line-face "#1c1f26")
(set-face-attribute 'mode-line nil  :height 360)

;; List recently opened files.
(recentf-mode 1)


;; Revert buffers when the underlying file has changed.
(global-auto-revert-mode 1)


;; Save history going back 25 commands.
;; Use M-p to get previous command used in the minibuffer.
;; Use M-n to move to next command.
(setq history-length 25)
(savehist-mode 1)


;; Save place in a file.
(save-place-mode 1)


;; These settings enables using the same configuration file on multiple platforms.
;; Note that windows-nt includes [[https://www.gnu.org/software/emacs/manual/html_node/elisp/System-Environment.html][windows 10]].
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-cygwin* (eq system-type 'cygwin))
(defconst *is-unix* (not *is-windows*))


;; See this [[http://ergoemacs.org/emacs/emacs_hyper_super_keys.html][ for more information.]]
;; set keys for Apple keyboard, for emacs in OS X 
;; Source http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make option key do Super. 
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper  


;; Minibuffer history keybindings
;; The calling up of a previously issued command in the minibuffer with ~M-p~ saves times.
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)


;; Bibtex configuration
(defconst blaine/bib-libraries (list "~/Documents/global.bib")) 

;; Combined with emacs-mac, this gives good PDF quality for [[https://www.aidanscannell.com/post/setting-up-an-emacs-playground-on-mac/][retina display]].
(setq pdf-view-use-scaling t)


;; PDF default page width behavior
(setq-default pdf-view-display-size 'fit-page)


;; Set delay in the matching parenthesis to zero.
(setq show-paren-delay 0)
(show-paren-mode t)


;; rainbow-delimiters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
            :foreground "magenta"
            :inherit 'error
            :box t)


;; ############################## Package Configurations ################################

;;## A

;;### auto-complete
;; do default config for auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)



;;;### auto-complete-auctex.el --- auto-completion for auctex

;; Copyright (C) 2012 Christopher Monsanto
     
;; Author: Christopher Monsanto <chris@monsan.to>
;; Version: 1.0
;; Package-Requires: ((yasnippet "0.6.1") (auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You can install this by (require 'auto-complete-auctex).
;; Feel free to contribute better documentation!

;;;#### Code:

(require 'tex)
(require 'latex)

(eval-when-compile
  (require 'auto-complete)
  (require 'yasnippet))

(defvar ac-auctex-arg-lookup-table
  '((TeX-arg-define-macro . ("\\MacroName"))
    (TeX-arg-counter . ("Counter"))
    (TeX-arg-define-counter . ("\\CounterName"))
    (TeX-arg-file . ("Filename"))
    (TeX-arg-bibliography . ("Filename"))
    (TeX-arg-bibstyle . ("Style"))
    (TeX-arg-environment . ("Environment"))
    (TeX-arg-define-environment . ("EnvironmentName"))
    (TeX-arg-size . ("(w, h)"))
    (TeX-arg-ref . ("Name"))
    (TeX-arg-index . ("Index"))
    (TeX-arg-define-label . ("Label"))
    (LaTeX-arg-usepackage . (["opt1,..."] "Package"))
    (LaTeX-env-label . nil)
    (LaTeX-amsmath-env-aligned . (["htbp!"]))
    (LaTeX-amsmath-env-alignat . (["# Columns"]))
    (LaTeX-env-array . (["bct"] "lcrpmb|"))
    (LaTeX-env-item . nil)
    (LaTeX-env-document . nil)
    (LaTeX-env-figure . (["htbp!"]))
    (LaTeX-env-contents . ("Filename"))
    (LaTeX-env-minipage . (["htbp!"] "Width"))
    (LaTeX-env-list . ("Label" "\\itemsep,\\labelsep,..."))
    (LaTeX-env-picture . ("(w, h)" "(x, y)"))
    (LaTeX-env-tabular* . ("Width" ["htbp!"] "lcrpmb|><"))
    (LaTeX-env-bib . ("WidestLabel"))
    (TeX-arg-conditional . ([""]))
    (2 . ("" ""))
    (3 . ("" "" ""))
    (4 . ("" "" "" ""))
    (5 . ("" "" "" "" ""))
    (6 . ("" "" "" "" "" ""))
    (7 . ("" "" "" "" "" "" ""))
    (8 . ("" "" "" "" "" "" "" ""))
    (9 . ("" "" "" "" "" "" "" "" "")))
  "Anything not in this table defaults to '(\"\")")

(defun ac-auctex-expand-arg-info (arg-info)
  (loop for item in arg-info
	append (cond
		((or (stringp item) (and (vectorp item) (stringp (elt item 0))))
		 (list item))
		((vectorp item)
		 (loop for item-2 in (or (assoc-default (or (car-safe (elt item 0)) (elt item 0))
							ac-auctex-arg-lookup-table 'equal) '(""))
		       collect [item-2]))
		(t
		 (or (assoc-default (or (car-safe item) item) ac-auctex-arg-lookup-table) '(""))))))

(defun ac-auctex-snippet-arg (n arg)
  (let* ((opt (vectorp arg))
	 (item (if opt (elt arg 0) arg))
	 (m (if (vectorp arg) (1+ n) n))
	 (var (format "${%s}" item)))
    (list (1+ m)
	  (if opt
	      (concat (format "${[") var "]}")
	    (concat "{" var "}")))))

;;;#### Macros
;;

(defun ac-auctex-expand-args (str env)
  (yas/expand-snippet (ac-auctex-macro-snippet (assoc-default str env))))

(defun ac-auctex-macro-snippet (arg-info)
  (let ((count 1))
    (apply 'concat (loop for item in (ac-auctex-expand-arg-info arg-info)
			 collect (destructuring-bind (n val)
				     (ac-auctex-snippet-arg count item)
				   (setq count n)
				   val)))))

(defun ac-auctex-macro-candidates ()
   (let ((comlist (if TeX-symbol-list
		      (mapcar (lambda (item)
			        (or (car-safe (car item)) (car item)))
			    TeX-symbol-list))))
    (all-completions ac-prefix comlist)))

(defun ac-auctex-macro-action ()
  (yas/expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))) 

(ac-define-source auctex-macros
  '((init . TeX-symbol-list)
    (candidates . ac-auctex-macro-candidates)
    (action . ac-auctex-macro-action)
    (requires . 0)
    (symbol . "m")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))

;;;#### Symbols

(defun ac-auctex-symbol-candidates ()
  (all-completions ac-prefix (mapcar 'cadr LaTeX-math-default)))

(defun ac-auctex-symbol-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (if (texmathp)
      (progn
	(insert "\\" candidate)
	(yas/expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))
    (insert "$\\" candidate "$")
    (backward-char)
    (yas/expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))))

(defun ac-auctex-symbol-document (c)
  (let* ((cl (assoc c (mapcar 'cdr LaTeX-math-default)))
         (decode (if (nth 2 cl) (char-to-string (decode-char 'ucs (nth 2 cl))) ""))
         (st (nth 1 cl))
         (hs (if (listp st) (mapconcat 'identity st " ") st)))
    (and decode (concat hs " == " decode))))

(ac-define-source auctex-symbols
  '((init . LaTeX-math-mode)
    (candidates . ac-auctex-symbol-candidates)
    (document . ac-auctex-symbol-document)
    (action . ac-auctex-symbol-action)
    (requires . 0)
    (symbol . "s")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;;;#### Environments

(defvar ac-auctex-environment-prefix "beg")

(defun ac-auctex-environment-candidates ()
  (let ((envlist (mapcar (lambda (item) (concat ac-auctex-environment-prefix (car item)))
			 LaTeX-environment-list)))
    (all-completions ac-prefix envlist)))

(defun ac-auctex-environment-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (let ((candidate (substring candidate (length ac-auctex-environment-prefix))))
    (yas/expand-snippet (format "\\begin{%s}%s\n$0\n\\end{%s}"
				candidate
				(ac-auctex-macro-snippet (assoc-default candidate LaTeX-environment-list))
				candidate)))) 

(ac-define-source auctex-environments
  '((init . LaTeX-environment-list)
    (candidates . ac-auctex-environment-candidates)
    (action .  ac-auctex-environment-action)
    (requires . 0)
    (symbol . "e")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;;;#### Refs

(defun ac-auctex-label-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-label-list)))

(ac-define-source auctex-labels
  '((init . LaTeX-label-list)
    (candidates . ac-auctex-label-candidates)
    (requires . 0)
    (symbol . "r")
    (prefix . "\\\\ref{\\([^}]*\\)\\=")))


;;;#### Bibs

(defun ac-auctex-bib-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-bibitem-list)))

(ac-define-source auctex-bibs
  `((init . LaTeX-bibitem-list)
    (candidates . ac-auctex-bib-candidates)
    (requires . 0)
    (symbol . "b")
    (prefix . ,(concat "\\\\cite"
		       "\\(?:"
		         "\\[[^]]*\\]"
		       "\\)?"
		       "{\\([^},]*\\)"
		       "\\="))))

;;;#### Setup

(defun ac-auctex-setup ()
  (setq ac-sources (append
                      '(ac-source-auctex-symbols
                        ac-source-auctex-macros
			ac-source-auctex-environments
			ac-source-auctex-labels
			ac-source-auctex-bibs)
                      ac-sources)))

(add-to-list 'ac-modes 'latex-mode)
(add-hook 'LaTeX-mode-hook 'ac-auctex-setup)

(provide 'auto-complete-auctex)

;;; auto-complete-auctex.el ends here

;; indent with spaces instead of tabs for pep8 compatibility
(setq tab-width 4)
(setq-default indent-tabs-mode nil)


;;## E

;;### ef-theme

(add-to-list 'load-path "~/latex-emacs/manual-packages/ef-themes")

(require 'ef-themes)
;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(setq ef-themes-to-toggle '(ef-summer ef-winter))

;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).

(setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 . (variable-pitch light 1.9))
        (1 . (variable-pitch light 1.8))
        (2 . (variable-pitch regular 1.7))
        (3 . (variable-pitch regular 1.6))
        (4 . (variable-pitch regular 1.5))
        (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.3))
        (7 . (variable-pitch 1.2))
        (t . (variable-pitch 1.1))))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
(load-theme 'ef-spring :no-confirm)

;; The themes we provide:
;;
;; Light: `ef-day', `ef-light', `ef-spring', `ef-summer'.
;; Dark:  `ef-autumn', `ef-dark', `ef-night', `ef-winter'.
;;
;; Also those which are optimized for deuteranopia (red-green color
;; deficiency): `ef-deuteranopia-dark', `ef-deuteranopia-light'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'#+END_SRC


;; atomic-chrome, used to interact with GhostText extension for Google Chrome.
(use-package atomic-chrome)
(atomic-chrome-start-server)
(setq atomic-chrome-default-major-mode 'python-mode)
(setq atomic-chrome-extension-type-list '(ghost-text))
;;(atomic-chrome-start-httpd)
(setq atomic-chrome-server-ghost-text-port 4001)
(setq atomic-chrome-url-major-mode-alist
      '(("github\\.com" . gfm-mode)
        ("overleaf.com" . latex-mode)
        ("750words.com" . latex-mode)))
; Select the style of opening the editing buffer by atomic-chrome-buffer-open-style.
; full: Open in the selected window.
; split: Open in the new window by splitting the selected window (default).
; frame: Create a new frame and window in it. Must be using some windowing pacakge.
(setq atomic-chrome-buffer-open-style 'split)


;; awesome-tab
(use-package awesome-tab
  :load-path "~/.emacs.default/elisp/awesome-tab"
  :config
  (awesome-tab-mode t))


;; *** Electric-pair mode. Add matching pairs of quotes and parentheses.
(electric-pair-mode)


;; *** electric-spacing
;; An emacs minor-mode to automatically add spacing around [[https://github.com/xwl/electric-spacing][operators] in math expressions.].
;; Backspace over the whitespaces to remove them when none are permitted.
(use-package electric-spacing
      :ensure t)

;; git clone https://github.com/walmes/electric-spacing.git into .emacs.default/elisp
;; byte-compile with (byte-compile-file "~/ess-emacs/elisp/electric-spacing/electric-spacing.el")
;; byte-compile with (byte-compile-file "~/ess-emacs/elisp/electric-spacing/electric-spacing-r.el")
(add-to-list 'load-path "~/ess-emacs/elisp/electric-spacing")
(require 'electric-spacing-r)
(add-hook 'ess-mode-hook #'electric-spacing-mode)
;; restrict to limited number of modes to keep it out of the minibuffer
(defvar my-electic-pair-modes '(python-mode julia-mode org-mode latex-mode))
(defun my-inhibit-electric-pair-mode (char)
  (not (member major-mode my-electic-pair-modes)))
(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)


;;## I

;;### ivy
(use-package ivy
 :ensure t
 :init
 (ivy-mode 1)
 (global-set-key "\C-s" 'swiper)
 (unbind-key "S-SPC" ivy-minibuffer-map)
 (setq ivy-height 15
       ivy-use-virtual-buffers t
       ivy-count-format "(%d/%d) "
       ivy-use-selectable-prompt t))
(use-package ivy-bibtex
   :ensure t)



;;## L

;;### LaTeX helpher functions
;;#### M-x description
;; Converts a selected list into a description list.
;; The elements of the list must begin with a dash.
;; The terms to be inserted into the square brackets
;; have to be added after running the function.
(defun description (beg end) 
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{description}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item[ ]"))
    (end-of-buffer)
    (insert "\\end{description}\n")))


;;#### M-x enumerate
;; Converts a selected list into an enumerated list.
;; The elements of the list must begin with a dash.
(defun enumerate (beg end) 
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{enumerate}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item "))
    (end-of-buffer)
    (insert "\\end{enumerate}\n")))


;;#### M-x itemize
;; Converts a selected list into an itemized list.
;; The elements of the list must begin with a dash.
;; A similar function could be made to make an enumerated list
;; and a description list.
;; Source: \url{https://tex.stackexchange.com/questions/118958/emacsauctex-prevent-region-filling-when-inserting-itemize}
(defun itemize (beg end) 
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{itemize}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item "))
    (end-of-buffer)
    (insert "\\end{itemize}\n")))


;;#### LaTeX related
(unless (package-installed-p `auctex) (package-install `auctex))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq doc-view-continuous t) ;; scroll over all pages in doc view 

;; Settings for minted package issue
(eval-after-load "tex" 
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
    )
  )

; Outline-minor-mode key map Source: https://www.emacswiki.org/emacs/OutlineMinorMode
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)



;;## M


;;### Move selected regions up or down
;; It is commands like these one that enable rapid reorganization of your prose when writing one sentence per row.
;; Thank you to DivineDomain for the suggested upgrade.
;; Source: https://www.emacswiki.org/emacs/MoveText 
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-line-region-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-line-region-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-<up>") 'move-line-region-up)



;; ;;### Move lines up an down
;; It is commands like these one that enable rapid reorganization of your prose when writing one sentence per row.
;; Retained for those who have not mastered regions.
;; (defun move-line (n)
;;   "Move the current line up or down by N lines."
;;   (interactive "p")
;;   (setq col (current-column))
;;   (beginning-of-line) (setq start (point))
;;   (end-of-line) (forward-char) (setq end (point))
;;   (let ((line-text (delete-and-extract-region start end)))
;;     (forward-line n)
;;     (insert line-text)
;;     ;; restore point to original column in moved line
;;     (forward-line -1)
;;     (forward-char col)))

;; (defun move-line-up (n)
;;   "Move the current line up by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) -1 (- n))))

;; (defun move-line-down (n)
;;   "Move the current line down by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) 1 n)))

;; (global-set-key (kbd "M-<up>") 'move-line-up)
;; (global-set-key (kbd "M-<down>") 'move-line-down)


;;## P

;;###  Move to cursor to previously visited window
;; From the book Writing GNU Emacs Extensions by Bill Glickstein.
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key "\C-xp" 'other-window-backward)


;;### pdf-tools

;; This is an alternative to the built-in DocView package.
;; 
;; 
;; The pdf-tools package runs on top of pdf-view package.
;; This making capturing text from PDFs much easier.
;; 
;; I followed a [[http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools][blog post]].
;; You enter highlights by selecting with the mouse and entering C-c C-a h.
;; An annotation menu opens in the minibuffer.
;; Enter ~C-c C-c~ to save the annotation.
;; Enter ~C-c C-a t~ to enter text notes.
;; Enter the note and enter ~C-c C-c~ to save.
;; Right-click the mouse to get a menu of more options.
;; 
;; 
;; #+BEGIN_SRC emacs-lisp
;; (use-package pdf-tools
;;   :pin manual ;; manually update
;;   :config
;;   ;; initialise
;;   (pdf-tools-install)
;; 
;;   ;; This means that pdfs are fitted to width by default when you open them
;;   (setq-default pdf-view-display-size 'fit-width)
;;   ;; open pdfs scaled to fit page
;;   ;;  (setq-default pdf-view-display-size 'fit-page)
;;    ;; automatically annotate highlights
;;   (setq pdf-annot-activate-created-annotations t)
;;   ;; use normal isearch
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
;;   ;; Setting for sharper images with Macs with Retina displays
;;   (setq pdf-view-use-scaling t)
;; #+END_SRC
;; 
;; **** Useful keybindings for viewing PDFs
;; |------------------------------------------+-----------------|
;; | Display                                  |                 |
;; |------------------------------------------+-----------------|
;; | Zoom in / Zoom out                       | ~+~ / ~-~       |
;; | Fit height / Fit width / Fit page        | ~H~ / ~W~ / ~P~ |
;; | Trim margins (set slice to bounding box) | ~s b~           |
;; | Reset margins                            | ~s r~           |
;; | Reset z oom                              | ~0~             |
;; |------------------------------------------+-----------------|
;; 
;; **** Useful keybindings for navigating PDFs
;; 
;; |-----------------------------------------------+-----------------------|
;; | Navigation                                    |                       |
;; |-----------------------------------------------+-----------------------|
;; | Scroll Up / Down by Page-full                 | ~space~ / ~backspace~ |
;; | Scroll Up / Down by Line                      | ~C-n~ / ~C-p~         |
;; | Scroll Right / Left                           | ~C-f~ / ~C-b~         |
;; | First Page / Last Page                        | ~<~ / ~>~             |
;; | Next Page / Previous Page                     | ~n~ / ~p~             |
;; | First Page / Last Page                        | ~M-<~ / ~M->~         |
;; | Incremental Search Forward / Backward         | ~C-s~ / ~C-r~         |
;; | Occur (list all lines containing a phrase)    | ~M-s o~               |
;; | Jump to Occur Line                            | ~RETURN~              |
;; | Pick a Link and Jump                          | ~F~                   |
;; | Incremental Search in Links                   | ~f~                   |
;; | History Back / Forwards                       | ~l~ / ~r~             |
;; | Display Outline                               | ~o~                   |
;; | Jump to Section from Outline                  | ~RETURN~              |
;; | Jump to Page                                  | ~M-g g~               |
;; | Store position / Jump to position in register | ~m~ / ~'~             |
;; |-----------------------------------------------+-----------------------|
;; 











;;## T
;;### TeXcount setup for TeXcount version 2.3 and later
;; See https://app.uio.no/ifi/texcount/howto.html to use from the command line.
(defun texcount ()
  (interactive)
  (let*
    ( (this-file (buffer-file-name))
      (enc-str (symbol-name buffer-file-coding-system))
      (enc-opt
        (cond
          ((string-match "utf-8" enc-str) "-utf8")
          ((string-match "latin" enc-str) "-latin1")
          ("-encoding=guess")
      ) )
      (word-count
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "/opt/local/bin/texcount" nil t nil "-0" enc-opt this-file)
    ) ) ) )
    (message word-count)
) )
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-cw" 'texcount)))
(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map "\C-cw" 'texcount)))



;;### activate word count mode
;; This mode will count the LaTeX markup, but it does give the count of incrementally added words.
(use-package wc-mode)
(add-hook 'text-mode-hook 'wc-mode)
;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-menu-height 15)
 '(ivy-height 20)
 '(package-selected-packages
   '(ef-themes yasnippet wc-mode use-package rainbow-delimiters powerline maxframe material-theme exec-path-from-shell electric-spacing better-defaults auto-complete auctex atomic-chrome)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;## Y 
;;### yasnippet related
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(global-set-key "\C-o" 'yas-expand)
(use-package popup
  :ensure t)
(require 'yasnippet)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))

(defun complete-if-yas-field (&rest _)
  (let ((field (yas-current-field)))
    (when (and field
               (not (yas--field-modified-p field)))
      (company-manual-begin))))

(advice-add 'company-complete-selection :after 'complete-if-yas-field)
(advice-add 'yas-next-field :after 'complete-if-yas-field)


