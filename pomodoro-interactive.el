;;; pomodoro-interactive.el --- Interactive functions for Pomodoro timer

;;; Code:

(require 'pomodoro-custom)

(defvar pomodoro-timer nil
  "Pomodoro timer.")

(defvar pomodoro-events nil
  "Pomodoro event list.")

(defvar pomodoro-current '(ok . 0)
  "Pomodoro current event. Initial value: all fine at the beginning.")

(defvar pomodoro-last 0
  "Pomodoro last timestamp value.")

(defvar pomodoro-debug nil
  "Pomodoro debugging switch.")

(defvar pomodoro-display-tubes t
  "Pomodoro displaying mode, tubes rather than text.")

(defun pomodoro-interactive-deliberate-pause ()
  "Pause deliberately."
  (interactive)
  (let ((event (if (equal (car pomodoro-current) 'pause) pomodoro-current
                 (cons 'reset (cdr pomodoro-current)))))
    (pomodoro-register-event event '(pause . 0)))
  (play-sound-file-async pomodoro-sound-tick))

(defun pomodoro-interactive-kill-buffer ()
  "Kills the buffer."
  (interactive)
  (kill-current-buffer))

(defun pomodoro-interactive-new-pomodoro ()
  "Forgoes the current pomodoro or leaves a break."
  (interactive)
  (let ((event (if (equal (car pomodoro-current) 'pause) pomodoro-current
                 (cons 'reset (cdr pomodoro-current)))))
    (pomodoro-register-event event '(ok . 0)))
  (play-sound-file-async pomodoro-sound-tick))

(defun pomodoro-interactive-reset ()
  "Resets the timer."
  (interactive)
  (if (y-or-n-p "Are you sure you want to reset the timer? ")
      (progn (setq pomodoro-current '(ok . 0)
		   pomodoro-events nil
                   pomodoro-last (timestamp))
             (play-sound-file-async pomodoro-sound-tick))
    (message "Pfew! That was close!")))

(defun pomodoro-interactive-toggle-display ()
  "Toggles between display modes."
  (interactive)
  (setq pomodoro-display-tubes (not pomodoro-display-tubes))
  (pomodoro-update))

(defun pomodoro-interactive-quit ()
  "Turns off Pomodoro."
  (interactive)
  (if (y-or-n-p "Are you sure you want to turn off Pomodoro? ")
      (progn (cancel-timer pomodoro-timer)
             (kill-current-buffer)
             (pomodoro-set-events nil '(ok . 0)))
    (message "Pfew! That was close!")))

(defun pomodoro-stick-to-the-grid ()
  (interactive)
  (setq pomodoro-current
        (pomodoro-ideal-state
         (string-to-number (format-time-string "%M") 10))))

(provide 'pomodoro-interactive)
;;; pomodoro-interactive.el ends here
