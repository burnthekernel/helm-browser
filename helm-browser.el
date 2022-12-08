;;; helm-browser.el --- Use helm to switch browser windows and tabs  -*- lexical-binding: t -*-

;; Author: BurnTheKernel <burnthekernel@protonmail.com>
;; Maintainer: BurnTheKernel <burnthekernel@protonmail.com>
;; URL: https://github.com/burnthekernel/helm-browser
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "24")(helm "3.0"))
;; Keywords: browser, tabs, helm

;;; Commentary:
;;
;; 'helm-browser' provides easy switching between web browser windows and tabs with helm.
;;

;;; License:

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'helm)
(require 's)

;;;; Customization

(defgroup helm-browser nil
  "Use helm to switch between browser windows and tabs."
  :prefix "helm-browser-"
  :group 'external)

(defcustom helm-browser-wmctrl-path "/usr/bin/wmctrl"
  "Absolute path to wmctrl executable."
  :type '(file :must-match t)
  :group 'helm-browser)

(defcustom helm-browser-bt-path "~/.local/bin/bt"
  "Absolute path to bt (brotab) executable."
  :type '(file :must-match t)
  :group 'helm-browser)

(defcustom helm-browser-focus-tab-wait-time 2.5
  "Delay in seconds.
Time to wait after switching and attempting to switch to window.
This delay is necessary because after activating the selected tab,
it take a couple of seconds before the window's title/URL becomes
available via wmctrl."
  :group 'helm-browser
  :type 'float)

(defcustom helm-browser-title-transform 'identity
  "Transform title of a single client window for display in helm selection."
  :group 'helm-browser
  :type '(repeat function))

;;;;Faces

(defgroup helm-browser-faces nil
  "Customize the appearance of helm-browser."
  :prefix "helm-"
  :group 'helm-browser
  :group 'helm-faces)

(defface helm-browser-browser-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for web browsers."
  :group 'helm-browser-faces)

(defface helm-browser-emacs-face
  '((t (:inherit font-lock-regexp-grouping-construct)))
  "Face used for Emacs."
  :group 'helm-browser-faces)

;;;; Internal Code

(defvar  aj-helm-browser-debug nil "Non-nil to print debug messages.")

(defvar helm-browser-wmclass-width 18 "Column with of wmclass field when displayed.")

(defun helm-browser--list-tabs ()
  "Gets list of browser tabs."
  (let ((bfr (generate-new-buffer " *helm-browser-bt-tab-output*"))
        (tabs-list '()))
    (with-current-buffer bfr
      (call-process-shell-command (concat helm-browser-bt-path " list") nil t)
       (setq tabs-list
             (append tabs-list  (split-string (buffer-string) "\n" t))))
  (kill-buffer bfr)
  tabs-list))

(defun helm-browser-get-tab-url (al)
  "Internal function to kill/copy tab url specified by AL."
  (let* ((al-str (substring (format "%s" al) 1 -1))
         (tab-url))
    (string-match "\t\\(.+\\)\t\\(https://[^[:space:]]+\\)" al-str)
    (setq tab-url (match-string 2 al-str))
    (when aj-helm-browser-debug (message "tab [url] = [%s] Copied" tab-url))
    (message "Tab URL: %s"  tab-url)
    (kill-new tab-url))
  )

(defun helm-browser-focus-tab-by-candidate (al)
  "Internal function to focus tab specified by AL, a window associative list."
  (let* ((al-str (substring (format "%s" al) 1 -1))
         (tab-id (substring al-str 1 (string-match "\t" al-str))) ; find a space/tab as separator
         (tab-switch-cmd (concat helm-browser-bt-path " activate " tab-id ))
         (tab-title)
         (tab-url)
         (win-title)
         (win-url)
         (wlist)
         (sitem))
    (when aj-helm-browser-debug (message "helm-browser: Activating tab with %s" tab-switch-cmd))
    (call-process-shell-command tab-switch-cmd) ; switch to tab
    (sit-for helm-browser-focus-tab-wait-time)  ; wait until tab becomes active and then search for that window.

    ;get the tab's title and url from bt
    (progn
      (string-match "\t\\(.+\\)\t\\(https://[^[:space:]]+\\)" al-str)
      (setq tab-title (match-string 1 al-str))
      (setq tab-url (match-string 2 al-str))
      (when aj-helm-browser-debug (message "tab [id][title]|[url] = [%s][%s]|[%s]" tab-id tab-title tab-url)))

    ;get list of windows/desktops and for each item check if either the window's url or title match the tab's url/title
    (setq wlist (helm-browser--list-windows))
    (dolist (witem wlist)
      (setq sitem (format "%s" witem))
      (when aj-helm-browser-debug (message "sitem = {%s}" sitem))
      (if (or (string-match "wmclass...Navigator.firefox.+title...\\(.+\\)[[:space:]]—[[:space:]]" sitem) ; note firefox uses a special separator
              (string-match "wmclass...brave-browser.Brave-browser.+title...\\(.+\\)[[:space:]]-[[:space:]]" sitem)
              (string-match "wmclass...chromium-browser.Chromium-browser.+title...\\(.+\\)[[:space:]]-[[:space:]]" sitem)
              (string-match "wmclass...chrome-browser.Chrome-browser.+title...\\(.+\\)[[:space:]]-[[:space:]]" sitem))
          (setq win-title (match-string 1 sitem))
        (setq win-title nil))
      (if (string-match "wmclass...Navigator.firefox.+\\(https?://[^[:space:]]+\\)" sitem)
          (setq win-url (match-string 1 sitem))
        (setq win-url nil))

      (if (and win-url (string= win-url tab-url))
          (progn
            (when aj-helm-browser-debug (message "win&tab url match: [%s]. Switching window." win-url))
            (helm-browser-focus-window-by-candidate (list witem)))
        ;;else
        (when aj-helm-browser-debug (message "no win url or no match with tab url"))
        (if (and  win-title (string= win-title tab-title))
            (progn
              (when aj-helm-browser-debug (message "win&tab title match: [%s]. Switching windows." win-title))
              (helm-browser-focus-window-by-candidate (list witem)))
          (when aj-helm-browser-debug (message "no win title or no match with tab title")))))))

(defun helm-browser-close-tab-candidate (al)
  "Closes a tab candidate AL, a list from ‘helm-browser--list-tabs’."
    (let* ((al-str (substring (format "%s" al) 1 -1))
           (tab-id (substring al-str 0 (string-match "[[:blank:]]" al-str)))
           (tab-close-cmd (concat helm-browser-bt-path " close " tab-id )))
      (when aj-helm-browser-debug (message (concat "helm-browser: closing a tab with " tab-close-cmd)))
      (message "Closing tab: %s"  al-str)
      (call-process-shell-command tab-close-cmd)))

(defun helm-browser-close-tab-candidates (_al)
  "Closes all selected candidates.  AL is ignored but seems to be needed by helm."
  (mapc 'helm-browser-close-tab-candidate (helm-marked-candidates)))

(defun helm-browser-format-tab-candidate (al)
  "Formats a candidate tabs for tab switcher.
AL is the associative list for a window."
  (let* (
         (tabstr (substring al (string-match "\t" al))))
    (cons tabstr (list al))))

(defun helm-browser-tab-candidates ()
  "Return a list of tabs."
  (mapcar 'helm-browser-format-tab-candidate (helm-browser--list-tabs)))

(defvar helm-browser-source-tabs
      (helm-build-sync-source "Browser Tabs"
        :fuzzy-match t
        :candidates 'helm-browser-tab-candidates
        :action '(("Focus tab" . helm-browser-focus-tab-by-candidate)
                  ("Copy tab URL" . helm-browser-get-tab-url)
                  ("Close tab(s)" . helm-browser-close-tab-candidates)
                  ("dump client window s-exp" . prin1 )))
      "Helm source that provides tabs and actions.")

(defun helm-browser-format-window-candidate (al)
  "Formats a candidate client window for task switcher.
AL is the associative list for a window."
  (let* ((ddfmt (format "%d.%d" helm-browser-wmclass-width helm-browser-wmclass-width))
         (wmtitlefmt (concat "%-" ddfmt "s %s"))
         (windowstr (format wmtitlefmt (helm-browser-wmclass al) (helm-browser-title al))))
    (message "wmclass= %s title=%s" (helm-browser-wmclass al) (helm-browser-title al))
    (cons windowstr (list al))))

(defun helm-browser-title (al)
  "Formats the title of a window.
AL is an associative list for a window."
  (let ((title   (cdr (assoc 'title   al))))
    (funcall helm-browser-title-transform
             (replace-regexp-in-string "- Google Chrome" "" title))))

(defun helm-browser-wmclass (al)
  "Gets the WMCLASS for window AL associative list."
  (let ((wmclass (cdr (assoc 'wmclass al))))
    (cond ((string-equal wmclass "google-chrome.Google-chrome" )
           (propertize "Chrome Browser" 'face 'helm-browser-browser-face ))
          ((string-equal wmclass "Navigator.firefox" )
           (propertize "Firefox Browser" 'face 'helm-browser-browser-face ))
          ((string-equal wmclass "brave-browser.Brave-browser" )
           (propertize "Brave Browser" 'face 'helm-browser-browser-face ))
          ((string-equal wmclass "chromium-browser.Chromium-browser" )
           (propertize "Chromium Browser" 'face 'helm-browser-browser-face ))
          (t
           (propertize "Other Window" 'face 'helm-browser-emacs-face)))))

(defconst helm-browser--wmctrl-field-count 10)

(defconst helm-browser--wmctrl-field-regex
  (concat "^"
          (mapconcat 'identity (make-list (1- helm-browser--wmctrl-field-count) "\\(\\S-+\\)\\s-+") "")
          "\\(.+\\)"))

(defun helm-browser--list-windows ()
  "Internal function to get a list of desktop windows via wmctrl."
  (let ((bfr (generate-new-buffer " *helm-browser-wmctrl-output*"))
        (windows-list '()))
    (with-current-buffer bfr
      (call-process-shell-command (concat helm-browser-wmctrl-path " -lGxp") nil t)
      (goto-char (point-min))
      (while (re-search-forward helm-browser--wmctrl-field-regex nil t)
        (let ((window-id (match-string 1))
              (desktop-number (match-string 2))
              (pid (match-string 3))
              (x-offset (match-string 4))
              (y-offset (match-string 5))
              (width (match-string 6))
              (height (match-string 7))
              (wmclass (match-string 8))
              (client-host (match-string 9))
              (title (match-string 10)))
          (setq windows-list
                (append windows-list
                        (list
                         `((window-id . ,window-id)
                           (desktop-number . ,desktop-number)
                           (pid . ,pid)
                           (x-offset . ,x-offset)
                           (y-offset . ,y-offset)
                           (width . ,width)
                           (height . ,height)
                           (wmclass . ,wmclass)
                           (client-host . ,client-host)
                           (title . ,title))))))))
    (kill-buffer bfr)
    windows-list))

(defun helm-browser-focus-window-by-candidate (al)
  "Internal function to focus the desktop window specified by AL."
  (let* ((id (cdr (assoc 'window-id (car al))))
         (desktop-id (cdr (assoc 'desktop-id (car al))))
         (desktop-switch-cmd (concat helm-browser-wmctrl-path " -s '" desktop-id "'"))
         (window-switch-cmd (concat helm-browser-wmctrl-path " -i -a '" id "'")))
    (call-process-shell-command desktop-switch-cmd)
    (call-process-shell-command window-switch-cmd)))

(defun helm-browser-close-window (al)
  "Closes a candidate AL.
AL is a window associative list from helm-browser--list-windows."
    (let* ((id (cdr (assoc 'window-id (car al))))
           (cmd (concat helm-browser-wmctrl-path " -i -c '" id "'")))
      (message (concat "helm-browser: closing X client with " cmd))
      (call-process-shell-command cmd)))

(defun helm-browser-close-windows (_al)
  "Closes all selected candidates.  AL is ignored but seems to be needed by helm."
  (mapc 'helm-browser-close-window (helm-marked-candidates)))

(defun helm-browser-window-candidates ()
  "Return a list windows with title and wmclass."
  (mapcar 'helm-browser-format-window-candidate (helm-browser--list-windows)))

(defvar helm-browser-source-browser-windows
      (helm-build-sync-source "Browser/Other Windows"
        :fuzzy-match t
        :candidates 'helm-browser-window-candidates
        :action '(("Focus window" . helm-browser-focus-window-by-candidate )
                  ("Close window(s)" . helm-browser-close-windows)
                  ("dump client window s-exp" . prin1 )))
      "Helm source that provides browser windows and actions.")

;;;###autoload
(defun helm-browser ()
  "Use helm to switch between browser windows and tabs."
  (interactive)
  (run-hooks 'helm-browser-open-hooks)
  (select-frame-set-input-focus (window-frame (selected-window)))
  (make-frame-visible)
  (helm :sources '(helm-browser-source-browser-windows
                   helm-browser-source-tabs)
        :buffer "*helm-browser*"
        :truncate-lines t))

;;;; Footer
(provide 'helm-browser)
;;; helm-browser.el ends here
