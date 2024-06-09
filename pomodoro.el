;;; pomodoro.el --- Simple and beautiful pomodoro timer

;;; Code:

(require 'pomodoro-custom)
(require 'pomodoro-interactive)
(require 'pomodoro-utils)

(defvar pomodoro-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'pomodoro-interactive-kill-buffer)
    (define-key map (kbd "Q") 'pomodoro-interactive-quit)
    (define-key map (kbd "S") 'pomodoro-stick-to-the-grid)
    (define-key map (kbd "R") 'pomodoro-interactive-reset)
    (define-key map (kbd "<return>") 'pomodoro-interactive-new-pomodoro)
    (define-key map (kbd "S-<return>") 'pomodoro-interactive-deliberate-pause)
    (define-key map (kbd "<tab>") 'pomodoro-interactive-toggle-display)
    map))

;;;###autoload
(defun pomodoro ()
  "A simple and beautiful pomodoro technique timer."
  (interactive)
  (with-current-buffer (get-buffer-create pomodoro-buffer)
    (use-local-map pomodoro-map)
    (font-lock-mode t))
  (setq pomodoro-last (timestamp))
  (pomodoro-update)
  (when pomodoro-timer (cancel-timer pomodoro-timer))
  (setq pomodoro-timer (run-at-time nil 1 'pomodoro-update))
  (switch-to-buffer pomodoro-buffer))

(global-set-key (kbd "<f12>") 'pomodoro)  ; Bind F12 to start Pomodoro timer

(defun pomodoro-add-to-mode-line ()
  (setq-default mode-line-format
                (cons '(pomodoro-mode-line-string pomodoro-mode-line-string)
                      mode-line-format)))

(provide 'pomodoro)
;;; pomodoro.el ends here
