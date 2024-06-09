;;; pomodoro-custom.el --- Custom settings for Pomodoro timer

;;; Code:

(require 'cl-lib)

(defgroup pomodoro nil
  "Custom settings for `pomodoro`."
  :group 'productivity)

(defcustom pomodoro-buffer-name "Pomodoro!"
  "Name of the Pomodoro buffer."
  :type 'string :group 'pomodoro)

(defvaralias 'pomodoro-buffer 'pomodoro-buffer-name)

(defcustom pomodoro-bar-length 25
  "Length of a pomodoro bar in tubes mode."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-pomodoro-length 25
  "Time length of a Pomodoro round."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-break-time 5
  "Length of time in minutes for a break period."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-long-break-time 15
  "Length of time in minutes for a long break period."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-nth-for-longer-break 4
  "Number of work cycles before a longer break."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-extra-time 2
  "Number of minutes to add onto a timer when avoiding a cycle change."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-break-start-message "Break time!"
  "Message shown when a break period starts."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-work-start-message "Back to work!"
  "Message shown when a work period starts."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-long-break-start-message "Time for a longer break!"
  "Message shown when a long break starts."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-show-number nil
  "Whether the number of the pomodoro in the series should be shown in the modeline."
  :type 'boolean :group 'pomodoro)

(defcustom pomodoro-desktop-notification nil
  "Whether to show desktop notifications."
  :type 'boolean :group 'pomodoro)

(defcustom pomodoro-play-sounds t
  "Should pomodoro play sounds when starting a new time period."
  :type 'boolean :group 'pomodoro)

(defcustom pomodoro-sound-player "mplayer"
  "Music player used to play sounds."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-break-start-sound ""
  "Sound played when a break period starts."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-work-start-sound ""
  "Sound played when a work period starts."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-time-format "%.2m:%.2s "
  "Time string to display in mode line for countdowns.
Formatted with `format-seconds`."
  :type 'string :group 'pomodoro)

(defcustom pomodoro-inhibit-prompting-messages t
  "Whether to inhibit prompting messages."
  :type 'boolean :group 'pomodoro)

(defcustom pomodoro-start-work-hook nil
  "Hook run when a pomodoro starts."
  :type 'hook :group 'pomodoro)

(defcustom pomodoro-start-break-hook nil
  "Hook run when a break between pomodoros starts."
  :type 'hook :group 'pomodoro)

;;; Faces
(defface pomodoro-time-face
  '((t (:family "DejaVu Sans" :height 6.0 :width semi-condensed)))
  "Pomodoro face for Clock."
  :group 'pomodoro)

(defface pomodoro-ok-face
  '((t (:foreground "#ff0000")))
  "Pomodoro face for valid pomodoro run."
  :group 'pomodoro)

(defface pomodoro-pause-face
  '((t (:foreground "#00ff00")))
  "Pomodoro face for paused pomodoro."
  :group 'pomodoro)

(defface pomodoro-reset-face
  '((t (:foreground "#333333")))
  "Pomodoro face for reset pomodoro."
  :group 'pomodoro)

(defface pomodoro-current-ok-face
  '((t (:height 2.5 :inherit pomodoro-ok-face)))
  "Pomodoro face for current pomodoro."
  :group 'pomodoro)

(defface pomodoro-current-pause-face
  '((t (:height 2.5 :inherit pomodoro-pause-face)))
  "Pomodoro face for current pause."
  :group 'pomodoro)

(provide 'pomodoro-custom)
;;; pomodoro-custom.el ends here
