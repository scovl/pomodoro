# Pomodoro Timer for Emacs

This Emacs package provides a simple and beautiful Pomodoro timer. The Pomodoro Technique is a time management method that uses a timer to break down work into intervals, traditionally 25 minutes in length, separated by short breaks.

## Features

- Customizable Pomodoro session length and break duration
- Simple, distraction-free interface
- Optional sound notifications
- Toggle between different display modes
- Interactive commands for controlling the timer
- System notifications for session transitions
- Pause and resume functionality
- Mode line display of timer status

## Installation

To use the Pomodoro timer in Emacs, you need to load the three files: `pomodoro.el`, `pomodoro-custom.el`, and `pomodoro-interactive.el`. Here's how to set it up:

1. **Download the files**: Save `pomodoro.el`, `pomodoro-custom.el`, and `pomodoro-interactive.el` to your Emacs configuration directory, typically `~/.emacs.d/` or another directory in your `load-path`.

2. **Add the following to your Emacs init file (`~/.emacs` or `~/.emacs.d/init.el`):**

   ```elisp
   (add-to-list 'load-path "~/.emacs.d/") ; Adjust this path if necessary

   (require 'pomodoro)
   ```

3. **Restart Emacs** or evaluate the lines added to your init file.

## Usage

After setting up the Pomodoro timer, you can start using it with the following commands:

- **Start the Pomodoro timer**: Press `F12` or run `M-x pomodoro`
- **Pause the timer**: Press `M-x pomodoro-pause`
- **Resume the timer**: Press `M-x pomodoro-resume`
- **Reset the timer**: Press `R`
- **Quit the Pomodoro timer**: Press `Q`

## Customization

You can customize various aspects of the Pomodoro timer by modifying the customizable variables. Here are some of the key variables you might want to customize:

- `pomodoro-buffer-name`: The name of the Pomodoro buffer.
- `pomodoro-bar-length`: The length of the Pomodoro bar in tubes mode.
- `pomodoro-pomodoro-length`: The length of a Pomodoro session in minutes.
- `pomodoro-break-time`: The length of a break period in minutes.
- `pomodoro-long-break-time`: The length of a long break period in minutes.
- `pomodoro-nth-for-longer-break`: The number of work cycles before a longer break.
- `pomodoro-break-start-message`: The message shown when a break starts.
- `pomodoro-work-start-message`: The message shown when a work period starts.
- `pomodoro-show-number`: Whether to show the number of the current Pomodoro in the mode line.
- `pomodoro-desktop-notification`: Whether to show desktop notifications for session transitions.
- `pomodoro-play-sounds`: Whether to play sounds for session transitions.
- `pomodoro-sound-player`: The command used to play sound files.
- `pomodoro-break-start-sound`: The sound file played when a break starts.
- `pomodoro-work-start-sound`: The sound file played when a work period starts.

To customize these variables, you can use `M-x customize-group` and enter `pomodoro`, or you can set them directly in your Emacs init file. For example:

```elisp
(setq pomodoro-pomodoro-length 30) ; Set Pomodoro session length to 30 minutes
(setq pomodoro-sound-tick "/path/to/tick.wav") ; Set the tick sound file
```

## Contributing

If you find any bugs or have suggestions for improvements, feel free to open an issue or submit a pull request.

## License

This project is licensed under the GNU General Public License v2.0. For more details, see the [LICENSE](LICENSE) file.
