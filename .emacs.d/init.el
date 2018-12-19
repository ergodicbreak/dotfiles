(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

; package management thanks to https://spwhitton.name/blog/entry/emacs-pkg-subtree/
(setq load-prefer-newer t
      package-enable-at-startup nil
      vc-handled-backends nil)

(defconst emacs-pkg-dir (concat user-emacs-directory "lib"))

(let ((default-directory emacs-pkg-dir))
  (normal-top-level-add-to-load-path '("." "magit/lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package)
  (with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/dotfiles/.emacs.d/lib/use-package/")
  (add-to-list 'Info-directory-list
	       "~/dotfiles/.emacs.d/lib/magit/Documentation/")))

(use-package magit)
	       


