;;; echo-bar.el --- Turn the echo area into a custom status bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Adam Tillou

;; Author: Adam Tillou <qaiviq@gmail.com>
;; Keywords: convenience, tools
;; Version: 1.0.0
;; Homepage: https://github.com/qaiviq/echo-bar.el

;; Note: This package will work without lexical binding, so there is no
;; Emacs 24 requirement.

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

;; This package will allow you to display a custom status at the end
;; of the echo area, like polybar but inside of Emacs.

;; To install, just run `M-x package-install RET echo-bar RET`.
;; To customize the text that gets displayed, set the variable
;; `echo-bar-function` to the name of your own custom function.
;; To turn the echo bar on or off, use `echo-bar-mode`.

;;; Code:

(require 'timer)
(require 'minibuffer)
(require 'overlay)
(require 'seq)

(defgroup echo-bar nil
  "Display text at the end of the echo area."
  :group 'applications)

(defface echo-bar-face
  '((t))
  "Default face used for the echo bar.")

(defcustom echo-bar-text-scale-factor 1
  "Scale factor to determine the size of character in the echo-bar.
This is in relation to a character from the echo area."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-right-padding 2
  "Number of columns between the text and right margin."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-function #'echo-bar-default-function
  "Function that returns the text displayed in the echo bar."
  :group 'echo-bar
  :type 'function)

(defcustom echo-bar-format
  '(:eval (format-time-string "%b %d | %H:%M:%S"))
  "Format of the text displayed in the echo bar.

This format will only apply if `echo-bar-function' is set to
`echo-bar-default-function', otherwise, the output of
`echo-bar-function' will be used.

See `mode-line-format' for more info about the required format."
  :group 'echo-bar
  :type 'sexp)

(defcustom echo-bar-right-padding 2
  "Number of columns between the text and right margin."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-minibuffer t
  "If non-nil, also display the echo bar when in the minibuffer."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-frame nil
  "If non-nil, show echo-bar in a frame instead."
  :group 'echo-bar
  :type 'boolean
  :set (lambda (sym val)
         (unless (and (boundp 'echo-bar-frame)
                      (eq echo-bar-frame val)) ;; No change necessary
         (when echo-bar-mode
           (echo-bar-disable))
         (set-default-toplevel-value sym val)
         (when echo-bar-mode
             (echo-bar-enable)))))

(defcustom echo-bar-update-interval 1
  "Interval in seconds between updating the echo bar contents.

If nil, don't update the echo bar automatically."
  :group 'echo-bar
  :type 'number)

;; Taken from corfu
(defvar echo-bar--frame-buf-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
    (cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . nil)
    (right-fringe-width . nil)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)
    (fringe-indicator-alist . nil)
    (indicate-empty-lines . nil)
    (indicate-buffer-boundaries . nil)
    (buffer-read-only . t))
  "Default child frame buffer parameters.")

;; Taken from corfu
(defvar echo-bar--frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (internal-border-width . 1)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Default child frame parameters for echo-bar.")

;; Taken from corfu
(defvar echo-bar--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (keymap-set map (format "<%s-%s>" k (1+ i)) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defvar echo-bar-timer nil
  "Timer used to update the echo bar.")

(defvar echo-bar-text nil
  "The text currently displayed in the echo bar.")

(defvar echo-bar-overlays nil
  "List of overlays displaying the echo bar contents.")

(defconst echo-bar--frame-buf-name " *echo-bar*")

;;;###autoload
(define-minor-mode echo-bar-mode
  "Display text at the end of the echo area."
  :global t
  (if echo-bar-mode
      (echo-bar-enable)
    (echo-bar-disable)))

;;;###autoload
(defun echo-bar-enable ()
  "Turn on the echo bar."
  (interactive)
  ;; Disable any existing echo bar to remove conflicts
  (echo-bar-disable)

  ;; Create overlays in each echo area buffer. Use `get-buffer-create' to make
  ;; sure that the buffer is created even if no messages were outputted before
  (if echo-bar-frame
      (add-hook 'after-make-frame-functions #'echo-bar--frame-after-make)
  (dolist (buf (mapcar #'get-buffer-create
                       '(" *Echo Area 0*" " *Echo Area 1*")))
    (with-current-buffer buf
      (remove-overlays (point-min) (point-max))
        (echo-bar--new-overlay))))

  ;; Start the timer to automatically update
  (when echo-bar-update-interval
    (run-with-timer 0 echo-bar-update-interval 'echo-bar-update))
  (echo-bar-update) ;; Update immediately

  ;; Add the setup function to the minibuffer hook
  (when echo-bar-minibuffer
    (add-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup)))

;;;###autoload
(defun echo-bar-disable ()
  "Turn off the echo bar."
  (interactive)
  ;; Remove echo bar overlays
  (mapc 'delete-overlay echo-bar-overlays)
  (setq echo-bar-overlays nil)

  ;; Remove text from Minibuf-0
  (if echo-bar-frame
      (progn
        (echo-bar--frame-delete-all echo-bar--frame-buf-name)
        (remove-hook 'after-make-frame-functions #'echo-bar--frame-after-make))
  (with-current-buffer (window-buffer
                        (minibuffer-window))
      (delete-region (point-min) (point-max))))

  ;; Cancel the update timer
  (cancel-function-timers #'echo-bar-update)

  ;; Remove the setup function from the minibuffer hook
  (remove-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup))

;; TODO: Use function `string-pixel-width' after 29.1
(defun echo-bar--string-pixel-width (str)
  "Return the width of STR in pixels."

  ;; Make sure the temp buffer settings match the minibuffer settings
  (with-selected-window (minibuffer-window)
    (if (fboundp #'string-pixel-width)
        (string-pixel-width str)
      (require 'shr)
      (shr-string-pixel-width str))))

(defun echo-bar--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (echo-bar--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun echo-bar-set-text (text)
  "Set the text displayed by the echo bar to TEXT."
  (let* ((wid (+ (string-width text) echo-bar-right-padding))
         (wid (* echo-bar-text-scale-factor wid))
         ;; Maximum length for the echo area message before wrap to next line
         (max-len (- (frame-width) wid 5))
         (spc (propertize " " 'cursor 1 'display
                          `(space :align-to (- (+ right
                                                  right-margin)
                                               ,wid)))))
    (add-face-text-property 0 (length text)
                            'echo-bar-face t text)
    (setq echo-bar-text (concat spc text))

    ;; Add the correct text to each echo bar overlay
    (if echo-bar-frame
        (echo-bar--frame-show)
    (dolist (o echo-bar-overlays)
      (when (overlay-buffer o)
        (with-current-buffer (overlay-buffer o)
          ;; Wrap the text to the next line if the echo bar text is too long
          (if (> (mod (point-max) (frame-width)) max-len)
              (overlay-put o 'after-string (concat "\n" echo-bar-text))
            (overlay-put o 'after-string echo-bar-text)))))

    ;; Display the text in Minibuf-0, as overlays don't show up
    (with-current-buffer (window-buffer
                          (minibuffer-window))
      ;; Don't override existing text in minibuffer, such as ispell
      (when (get-text-property (point-min) 'echo-bar)
        (delete-region (point-min) (point-max)))
      (when (= (point-min) (point-max))
          (insert (propertize echo-bar-text 'echo-bar t)))))))

(defun echo-bar--frame-make-buffer (name)
  "Make buffer with NAME for use in echo-bar's frames."
  (let ((ls line-spacing)
        (buffer (get-buffer name)))
    (or buffer ;; If we have a buffer already, assume we created it.
        (with-current-buffer (setq buffer (get-buffer-create name))
      ;;; XXX HACK from corfu install mouse ignore map
      (use-local-map echo-bar--mouse-ignore-map)
      (dolist (var echo-bar--frame-buf-parameters)
        (set (make-local-variable (car var)) (cdr var)))
      (setq-local line-spacing ls)
          buffer))))

(defun echo-bar--frame-find (buffer &optional parent)
  "Find frame display BUFFER and with a specific PARENT.
If PARENT is nil, ignore that check."
  (cl-find-if
   (lambda (frame)
     (let ((buffer-info (frame-parameter frame 'echo-bar-frame)))
       (and
        (or (null parent)
            (equal (frame-parent frame) parent))
        (or (equal (buffer-name buffer) (car buffer-info))
            (equal buffer (cdr buffer-info))))))
   (frame-list)))

(defun echo-bar--frame-delete-all (buffer &optional parent)
  "Delete all frames BUFFER and have a specific PARENT.
If PARENT is nil, ignore that check."
  (let ((buffer (get-buffer buffer))
        frame)
    (when buffer
      (while (setq frame
                   (echo-bar--frame-find buffer parent))
        (delete-frame frame))
      (kill-buffer buffer))))

(defun echo-bar--frame-make (x y buffer &optional parent)
  "Show BUFFER in child frame at X/Y with a specific PARENT."
  (let* ((window-min-height 1)
         (window-min-width 1)
         (inhibit-redisplay t)
         ;; The following is a hack from posframe and from corfu
         ;; (x-gtk-resize-child-frames corfu--gtk-resize-child-frames)
         (before-make-frame-hook)
         (after-make-frame-functions)
         (parent (or parent (window-frame)))
         (frame (echo-bar--frame-find buffer parent)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame)
                     (and (not (bound-and-true-p exwm--connection)) parent))
                 ;; If there is more than one window, `frame-root-window' may
                 ;; return nil.  Recreate the frame in this case.
                 (window-live-p (frame-root-window frame)))
      (when frame (delete-frame frame))
      (setq frame (make-frame
                   `((name . "echo-bar")
                     (parent-frame . ,parent)
                     (minibuffer . nil)
                     ;; (minibuffer . ,(minibuffer-window parent))
                     (width . 0) (height . 0) (visibility . nil)
                     ,@echo-bar--frame-parameters))))
    ;; Reset frame parameters if they changed.  For example `tool-bar-mode'
    ;; overrides the parameter `tool-bar-lines' for every frame, including child
    ;; frames.  The child frame API is a pleasure to work with.  It is full of
    ;; lovely surprises.
    (let* ((is (frame-parameters frame))
           (should echo-bar--frame-parameters)
           (diff (cl-loop for p in should for (k . v) = p
                          unless (equal (alist-get k is) v) collect p)))
      (when diff (modify-frame-parameters frame diff)))
    (let ((win (frame-root-window frame)))
      (unless (eq (window-buffer win) buffer)
        (set-window-buffer win buffer))
      ;; Disallow selection of root window (gh:minad/corfu#63)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t)
      ;; Mark window as dedicated to prevent frame reuse (gh:minad/corfu#60)
      (set-window-dedicated-p win t))
    (set-frame-parameter frame
                         'echo-bar-frame
                         (cons (buffer-name buffer) buffer))
    (redirect-frame-focus frame parent)
    (fit-frame-to-buffer frame)
    (pcase-let ((`(,px . ,py) (frame-position frame)))
      (unless (and (= x px) (= y py))
        (set-frame-position frame x y)))
    (make-frame-visible frame)
    ;; Unparent child frame if EXWM is used, otherwise EXWM buffers are drawn on
    ;; top of the Corfu child frame.
    (when (and (bound-and-true-p exwm--connection) (frame-parent frame))
      (set-frame-parameter frame 'parent-frame nil))
    frame))

(defun echo-bar--frame-after-make (_)
  "Update echo bar after `make-frame'."
  (echo-bar--frame-show))

(defun echo-bar--frame-show ()
  "Show echo bar text in frame."
  (let (buf)
    (with-current-buffer
        (setq buf
              (echo-bar--frame-make-buffer echo-bar--frame-buf-name))
      (let (buffer-read-only)
        (with-silent-modifications
          (erase-buffer)
          (insert echo-bar-text))))
    (dolist (frm (frame-list))
      (let ((min-buf (minibuffer-window frm)))
      (when (and
             (frame-live-p frm)
             (frame-visible-p frm)
               min-buf
               (equal (window-frame min-buf) frm))
          (echo-bar--frame-make -10 -1 buf frm))))))

(defun echo-bar--new-overlay (&optional remove-dead buffer)
  "Add new echo-bar overlay to BUFFER.
When REMOVE-DEAD is non-nil, also remove any dead overlays, i.e.,
those without a buffer from the beginning of the internal list of
overlays."
  (when remove-dead
    ;; Remove all dead overlays from the list
    (setq echo-bar-overlays
          (seq-filter 'overlay-buffer echo-bar-overlays)))

  (let ((new-overlay (make-overlay (point-max)
                                   (point-max) buffer t t)))
    (push new-overlay echo-bar-overlays)
    new-overlay))

(defun echo-bar--minibuffer-setup ()
  "Setup the echo bar in the minibuffer."
  (unless echo-bar-frame
    (overlay-put (echo-bar--new-overlay t) 'priority 1))
  (echo-bar-update))

(defun echo-bar-update ()
  "Get new text to be displayed from `echo-bar-default-function`."
  (interactive)
  (when echo-bar-mode
    (with-current-buffer (get-buffer-create
                          (if echo-bar-frame
                              echo-bar--frame-buf-name
                            " *Echo Area 0*"))
      (echo-bar-set-text (funcall echo-bar-function)))))

(defun echo-bar-default-function ()
  "The default function to use for the contents of the echo bar.
Returns the formatted text from `echo-bar-format'."
  (format-mode-line echo-bar-format))

(provide 'echo-bar)
;;; echo-bar.el ends here
