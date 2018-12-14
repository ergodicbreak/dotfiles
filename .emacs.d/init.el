(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

; package management thanks to https://spwhitton.name/blog/entry/emacs-pkg-subtree/
(setq load-prefer-newer t)

(defconst emacs-pkg-dir (concat user-emacs-directory "lib"))

(let ((default-directory emacs-pkg-dir))
  (normal-top-level-add-to-load-path '("." "magit/lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)
(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package)
  (with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/dotfiles/.emacs.d/lib/use-package/")
  (add-to-list 'Info-directory-list
	       "~/dotfiles/.emacs.d/lib/magit/Documentation/")))

(use-package magit
  :load-path "~/dotfiles/.emacs.d/lib/magit/lisp")
	       


