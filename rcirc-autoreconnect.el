;; -*- lexical-binding: t; -*-
;;; rcirc-autoreconnect.el ---

;; Copyright (C) 2014 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'rcirc)
(defgroup rcirc-autoreconnect nil
  "Autoreconnect feature for RCIRC"
  :group 'rcirc)

(defvar rcirc-autoreconnect nil
  "Enable RCIRC autoreconnect functionality.")

(defcustom rcirc-autoreconnect-delay 10
  "Timeout before trying to reconnect in second."
  :group 'rcirc-autoreconnect
  :type 'number)

(defadvice rcirc-connect (after rcirc-register-information
                                (server
                                 &optional port nick user-name full-name
                                 startup-channels password encryption))
  (let ((proc ad-return-value))
    (process-put proc 'server server)
    (process-put proc 'port port)
    (process-put proc 'nick nick)
    (process-put proc 'user-name user-name)
    (process-put proc 'full-name full-name)
    (process-put proc 'startup-channels startup-channels)
    (process-put proc 'password password)
    (process-put proc 'encryption encryption)
    proc))

(defun rcirc-toggle-autoreconnect (&optional prefix)
  "Toggle autoreconnect for RCIRC.

If PREFIX is negative disable it, if PREFIX is positive enable
it."
  (interactive "P")
  (cond ((null prefix)
         (setf rcirc-autoreconnect (not rcirc-autoreconnect)))
        ((< (prefix-numeric-value prefix) 0)
         (setf rcirc-autoreconnect nil))
        (t
         (setf rcirc-autoreconnect t)))
  (message "RCIRC autoreconnect %s" (if rcirc-autoreconnect
                                        "enabled"
                                      "disabled"))
  (cond (rcirc-autoreconnect
         (pushnew #'rcirc-autoreconnect-sentinel rcirc-sentinel-hooks)
         (ad-activate #'rcirc-connect))
        (t
         (setf rcirc-sentinel-hooks
               (delete #'rcirc-autoreconnect-sentinel rcirc-sentinel-hooks))
         (ad-deactivate #'rcirc-connect))))

(defun rcirc-autoreconnect-sentinel (proc event)
  (when (and (string-match-p event "connection broken by remote peer")
             (null (process-get proc 'timer)))
    (let ((timer (run-at-time 1 rcirc-autoreconnect-delay
                              (lambda (proc)
                                (if (buffer-live-p (process-buffer proc))
                                    (message "Trying to reconnect to %s:%d. (%s)"
                                             (process-get proc 'server)
                                             (process-get proc 'port)
                                             (if (ignore-errors
                                                   (rcirc-connect (process-get proc 'server)
                                                                  (process-get proc 'port)
                                                                  (process-get proc 'nick)
                                                                  (process-get proc 'user-name)
                                                                  (process-get proc 'full-name)
                                                                  (process-get proc 'startup-channels)
                                                                  (process-get proc 'password)
                                                                  (process-get proc 'encryption))
                                                   (cancel-timer (process-get proc 'timer))
                                                   (process-put proc 'timer nil))
                                                 "succeed"
                                               "failed"))
                                  (cancel-timer (process-get proc 'timer))
                                  (process-put proc 'timer nil)))
                              proc)))
      (process-put proc 'timer timer))))


(provide 'rcirc-autoreconnect)

;;; rcirc-autoreconnect.el ends here
