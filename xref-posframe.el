;;; xref-posframe.el --- Show xref definitions with posframe -*- lexical-binding: t; -*-

;; Author: 2019 fmdkdd
;; Author: Jeff Walsh <fejfighter@gmail.com>
;; Version: 0.11
;; Keywords: posframe
;; Requires: ('posframe)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (global-set-key (kbd "M-.") #'xref-posframe-dwim)
;; (global-set-key (kbd "M-,") #'xref-posframe-pop)

;;; Code:
(require 'cl-lib)
(require 'xref)
(require 'posframe)

(defvar xref-posframe-preview-lines 3
  "Number of lines to show in xref-posframe.")

(defun xref--item-string (item)
  "Return a preview string for xref ITEM."
  (save-excursion
    (let ((marker (xref-location-marker (xref-item-location item))))
      (set-buffer (marker-buffer marker))
      (xref--goto-char marker)
      (let ((beg (line-beginning-position))
            (end (progn (forward-line xref-posframe-preview-lines)
                        (line-end-position))))
        (font-lock-ensure beg end)
        (buffer-substring beg end)))))


(defvar xref-posframe--visible nil
  "Whether the posframe is visible.")

(defun xref-posframe ()
	 "nothing")
	 
(defvar xref-posframe-buffer "*xref-posframe-buffer*"
  "xref-posframe's buffer which used by posframe.")

(defun xref--posframe-show-xref-buffer (fetcher alist)
  (cl-assert (functionp fetcher))
  message (alist)
  (let* ((xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher)))
         (xref-alist (xref--analyze xrefs)))
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (erase-buffer)
      (xref--xref-buffer-mode)
      ;(xref--show-common-initialize xref-alist fetcher alist)
					;(pop-to-buffer (current-buffer))

      (current-buffer))))

(defvar xref-posframe-alist nil)
;;(defvar helm-xref-candidate-formatting-function 'helm-xref-format-candidate-short)


;; (defun helm-xref-format-candidate-short (file line summary)
;;   "Build short form of candidate format with FILE, LINE, and SUMMARY."
;;   (concat
;;    (propertize (car (reverse (split-string file "\\/")))
;; 	       'font-lock-face 'helm-xref-file-name)
;;    (when (string= "integer" (type-of line))
;;      (concat
;;       ":"
;;       (propertize (int-to-string line)
;; 		  'font-lock-face 'helm-xref-line-number)))
;;    ":"
;;    summary))

;; (defun helm-xref-candidates-27 (fetcher alist)
;;   "Convert XREF-ALIST items to helm candidates and add them to `helm-xref-alist'."
;;   (cl-assert (functionp fetcher))
;;   (let* ((xrefs
;;           (or
;;            (assoc-default 'fetched-xrefs alist)
;;            (funcall fetcher))))
;; 	(dolist (xref xrefs)
;; 	  (with-slots (summary location) xref
;; 	    (let* ((line (xref-location-line location))
;; 		       (file (xref-location-group location))
;; 		       candidate)
;;           (setq candidate
;; 		        (funcall helm-xref-candidate-formatting-function file line summary))
;;           (push (cons candidate xref) helm-xref-alist)))))
;;   (setq helm-xref-alist (reverse helm-xref-alist)))



(defun xref-posframe--insert-line (loc ref)
  (let ((start (point)))
    (apply #'insert strings)
    (add-text-properties start (point) props))
  )

(defun xref-posframe--insert (xref-alist)
  "fff XREF-ALIST"
  (cl-loop for ((group . xrefs) . more1) on xref-alist
	   do
	   (cl-loop for (xref . more2) on xrefs do
                     (with-slots (summary location) xref
                       (let* ((line (xref-location-line location))
                              (prefix
                               (if line
                                   ;; (propertize (format line-format line)
                                   ;;             'face 'xref-line-number)
                                   "  ")))
			 (insert
			  group " " (int-to-string line) " -" summary "\n")))))
  "noper")

(defun xref-posframe--preview (fetcher alist)
  "Show xref ITEM in a posframe. ALIST is ignored"
  (cl-assert (functionp fetcher))
  (let* ((xrefs
	  (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher)))
	 (xref-alist (xref--analyze xrefs))
	 (buffer (get-buffer-create xref-posframe-buffer)))

    (with-current-buffer buffer
      (xref-posframe-mode 1)
      (erase-buffer)
      (xref-posframe--insert xref-alist)
      (current-buffer))
      
  ;;(helm-xref-candidates-27 fetcher alist)
    
    (posframe-show buffer
                   :background-color "#334455"
                   :foreground-color "#baaa93"
		   :posframe-height 4
		   :position (point)      
		   )
        (message "made it to the preview posframe"))

    (setq xref-posframe--visible t)
  ;(add-hook 'post-command-hook #'xref-posframe--auto-clear))
    )


(defun xref-posframe--refs (buff)
  "Show xref ITEM in a posframe."
  (posframe-show buff
		 :string (xref--item-string item)
                 :background-color "#334455"
                 :foreground-color "#baaa93")
  (setq xref-posframe--visible t)
  (add-hook 'post-command-hook #'xref-posframe--auto-clear))


;;;###autoload
(defun xref-posframe-dwim ()
  "Show definitions for symbol at point with posframe.

Use xref to find the definitions for the symbol at point.  If
there is only one definition, preview it with posframe.  If this
command is called twice in succession, jump to the definition
instead.  If there are multiple definitions, fallback on the xref
behavior to display the candidates in a separate window."
  (interactive)
  (let* ((backend (xref-find-backend))
         (id (xref-backend-identifier-at-point backend))
         (defs (xref-backend-definitions backend id)))
    (cond
     ((not defs)
      (user-error "No definitions found for: %s" id))
     ((cdr defs)
      (xref--show-defs defs nil))
     ((eq last-command #'xref-posframe-dwim)
      (xref-push-marker-stack)
      (xref-pop-to-location (car defs) nil)
      (xref-posframe--clear))
     (t
      (xref-posframe--preview (car defs))))))

;;;###autoload
(defun xref-posframe-references ()
  "Show definitions for symbol at point with posframe.

Use xref to find the definitions for the symbol at point.  If
there is only one definition, preview it with posframe.  If this
command is called twice in succession, jump to the definition
instead.  If there are multiple definitions, fallback on the xref
behavior to display the candidates in a separate window."
  (interactive)
   (let* ((backend (xref-find-backend))
          (id (xref-backend-identifier-at-point backend))
          )
   ;;(xref--references backend id)))
     (xref-find-references id)))

(defun xref-posframe-xref-show (func alist)
  ""
  )

(global-set-key (kbd "M-?") 'xref-posframe-references)
(setq xref-show-xrefs-function 'xref-posframe--preview)
(setq debug-on-error t)

(defun xref-posframe--clear ()
  "Clear the xref posframe."
  (posframe-hide xref-posframe-buffer)
  (setq xref-posframe--visible nil)
  )

(defun xref-posframe--auto-clear ()
  "Clear the xref posframe when this command is not `xref-posframe-dwim'."
  (when (not (eq this-command #'xref-posframe-dwim))
    (xref-posframe--clear)))

;;;###autoload
(defun xref-posframe-pop ()
  "Dismiss the xref posframe, or pop the marker stack."
  (interactive)
  (unless xref-posframe--visible
    (xref-pop-marker-stack)))

(defun xref-posframe--abort ()
  (interactive)
  ;; The timer fixes https://github.com/emacs-lsp/lsp-ui/issues/33
  (run-with-idle-timer 0 nil 'xref-posframe--disable))

(defun xref-posframe--next ()
  (interactive)
  (xref-next-line))
  
(defvar xref-posframe-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-g" 'xref-posframe--abort)
    (define-key keymap "q" 'xref-posframe--abort)
    (define-key keymap (kbd "<down>") 'xref-posframe--next)
    (define-key map (kbd "RET") 'lsp-ui-peek--goto-xref)
    keymap)
  "Keymap that is enabled during an active completion.")

(require 'dash)


(defun xref-posframe--disable ()
  "Do not call this function, call `xref-posframe--abort' instead."
  (when (bound-and-true-p xref-posframe-mode)
    (xref-posframe-mode -1)
    (setq xref-posframe--visible nil)
    (xref-posframe--clear)))


;; (defun xref-posframe--peek-hide ()
;;   "Hide the chunk of code and restore previous state."
;;   (setq xref-posframe--visible nil))
;;(set-window-start (get-buffer-window) xref-posframe--win-start))

(defvar-local xref-posframe--deactivate-keymap-fn nil)

(defun xref-posframe--deactivate-keymap ()
  "Deactivate keymap."
  (-when-let (fn xref-posframe--deactivate-keymap-fn)
    (setq xref-posframe--deactivate-keymap-fn nil)
    (funcall fn)))

    
;;;###autoload
(define-minor-mode xref-posframe-mode
  "Display ivy via posframe."
  :init-value nil
  :global t
  ;;:lighter ivy-posframe-lighter
  ;;:group 'xref-posframe
  ;:keymap '(((kbd "C-g") . xref-posframe--clear))
  ;;           ([remap swiper-avy]           . ivy-posframe-swiper-avy)
  ;;           ([remap ivy-read-action]      . ivy-posframe-read-action)
  ;;           ([remap ivy-dispatching-done] . ivy-posframe-dispatching-done))
  (if xref-posframe-mode
      (setq xref-posframe--deactivate-keymap-fn
	    (set-transient-map xref-posframe-active-map t 'xref-posframe--abort))
    (xref-posframe--deactivate-keymap)
    ;(xref-posframe--peek-hide)
    ))
  ;;       (mapc (lambda (elm)
;;               (advice-add (car elm) :around (cdr elm)))
;;             xref-posframe-advice-alist)
;;     (mapc (lambda (elm)
;;             (advice-remove (car elm) (cdr elm)))
;;           xref-posframe-advice-alist)))




;;;###autoload
(defun xref-posframe-enable ()
  (interactive)
(xref-posframe-mode 1)

  (message "xref-posframe: suggest use `xref-posframe-mode' instead."))


(provide 'xref-posframe)
;;; xref-posframe.el ends here
