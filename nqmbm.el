;;; nqmbm.el --- advanced mouse buffer menu mainly for win32

;;; Version 1.0 (2003-01-07)
;;; $Id: nqmbm.el,v 1.11 2003/01/07 00:26:05 noniq Exp $

;;; Copyright (C) 2003 Stefan Daschek (sd@noniq.at)

;;; The latest version of this file can be obtained from
;;; http://v2.noniq.at/tech/emacs/lisp/

;;; This file is *NOT* (yet?) part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; ----------------------------------------------------------------------
;;; Description:
;;;
;;; Ever been annoyed by the somewhat clumsy display of
;;; `mouse-buffer-menu' on win32 (and other platforms that use a
;;; non-fixed-width font for menus)? This package might help. 
;;;
;;; nqMBM is a replacement for `mouse-buffer-menu' with several
;;; improvements:
;;;
;;; + Nicer menu layout with non-fixed-width fonts.
;;; + Show some (by default: 3) buffers at the top of the menu in most
;;;   recently used order for quick access, remaining buffers are
;;;   sorted alphabetically.
;;; + Internal buffers (like *Messages* and *scratch*) are by default
;;;   displayed in a submenu to avoid clobbering the menu.
;;; + The popup menu is displayed with a (configurable) vertical
;;;   offset so that you can quickly select the buffer displayed at
;;;   the top (usually the most recently used one) by a simple double
;;;   click.

;;; ----------------------------------------------------------------------
;;; Installation:
;;;
;;; + Place this file in a directory in your load-path and optionally
;;;   byte-compile it. There should not be any warnings.
;;; + Put the following in your .emacs file:
;;;
;;;   (require 'nqmbm)
;;;   (global-set-key [(control down-mouse-1)] 'nqmbm)
;;;
;;; + Restart your Emacs. nqMBM is now installed and activated.
;;; + To list the possible customizations go to the customization
;;;   group `nqmbm' (M-x customize-group nqmbm).

;;; ----------------------------------------------------------------------
;;; Compatibility:
;;;
;;; nqMBM was developed an tested under GNU Emacs 21.2.1.

;;; ----------------------------------------------------------------------
;;; Parts of this section have been stolen from the comments in
;;; mic-paren.el, thanks to Mikael Sj�din, Klaus Berndl and whoever
;;; else contributed to it.



;;; ======================================================================
;;; User Options

(defgroup nqmbm nil
  "Advanced mouse buffer menu mainly for Win32."
  :tag "nqMBM"
  :prefix "nqmbm-"
  :group 'mouse)

(defcustom nqmbm-show-most-recently-used 3
  "*Number of buffers to show in most-recently-used order.
Set to 0 to disable the most-recently-used section and have all
buffers displayed in alphabetical order. Set to 'unlimited to have all
buffers displayed in most-recently-used order."
  :type '(choice (integer :tag "Number of Buffers")
                 (const :tag "Unlimited" unlimited))
  :group 'nqmbm)

(defcustom nqmbm-handle-internal-buffers 'seperate
  "*How to handle internal buffers like *Messages* and *scratch*.
Possible settings are:
- 'seperate: Make a submenu containing all internal buffers.
- 'normal  : Handle internal buffers like other buffers.
- 'hide    : Do not display internal buffers at all."
  :type '(choice (const :tag "Make Submenu" seperate)
                 (const :tag "Normal" normal)
                 (const :tag "Hide" ignore))
  :group 'nqmbm)

(defcustom nqmbm-ingore-case-on-sorting t
  "*If t, ignore case when sorting buffers alphabetically."
  :type 'boolean
  :group 'nqmbm)

(defcustom nqmbm-popup-y-offset 34
  "*Y-offset (in pixel) for displaying the popup menu.
Customize this value so that the mouse pointer is directly above the
first menu entry when the menu is displayed to enable \"double click\"
switching."
  :type 'integer
  :group 'nqmbm)

(defcustom nqmbm-file-modified-tags '("*" "  ")
  "*Tags displayed in front of the filename.
On systems (such as Win32 or Mac OS) where menus are displayed with a
non-fixed-width font, both tags should be strings that have
approximately the same width when displayed with the menu-font. On
Systems using fixed-with fonts for menus both tags should of course
have the same number of characters."
  :type '(list (string :tag "File is modified    ")
               (string :tag "File is not modified"))
  :group 'nqmbm)

(defcustom nqmbm-max-filename-length 'unlimited
  "*Maximum length for displayed filenames, longer names are truncated.
Set to 0 to disable displaying of filenames completely."
  :type '(choice (integer)
                 (const unlimited))
  :group 'nqmbm)

(defcustom nqmbm-seperate-columns 'tabulator
  "*How to seperate buffername and filename.
Possible settings are:
- 'tabulator: Use a tab-character for seperating. This is known to
              work under Win32 and gives a proper 2 column layout even
              with non-fixed-width fonts.
- 'spaces   : Use fixed-width fields padded with spaces. This of
              course works only with fixed-width fonts.
- nil       : Do not build 2 columns, instead enclose the filename in
              parenthesis and append it directly to the buffername.
If you want to disable displaying of filenames completely, set
`nqmbm-max-filename-length' to 0."
  :type '(choice (const tabulator)
                 (const spaces)
                 (const nil))
  :group 'nqmbm)

;;; End of User Options
;;; ======================================================================


;;; ======================================================================
;;; Internal Variables

(defvar nqmbm-buffername-maxlen nil
  "Holds the maximum length of all buffer names displayed in the pop.
Used for calculating field width when `nqmbm-seperate-columns' is set
to SPACES.")


;;; ======================================================================
;;; Main Code

(defun nqmbm (event)
  "Pop up a menu of buffers for selection with the mouse. This
switches buffers in the window that you clicked on, and selects that
window. Bind this to C-down-mouse-1 as replacement for
`mouse-buffer-menu'."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((most-recently-used-buffers nil)
         (internal-buffers nil)
         (other-buffers nil)
         (sort-function (if nqmbm-ingore-case-on-sorting 'nqmbm-ignore-case-sort-function 'nqmbm-normal-sort-function))
         (num 0))

    ;; fill our own buffer-lists
    (dolist (buffer (cdr (buffer-list)))
      (unless (or (string= (substring (buffer-name buffer) 0 1) " ")
                  (and (eq nqmbm-handle-internal-buffers 'ignore)
                       (nqmbm-internal-buffer-p buffer)))
        (if (or (eq nqmbm-show-most-recently-used 'unlimited) (< num nqmbm-show-most-recently-used))
            (if (and (eq nqmbm-handle-internal-buffers 'seperate)
                     (nqmbm-internal-buffer-p buffer))
                (push buffer internal-buffers)
              (push buffer most-recently-used-buffers)
              (setq num (1+ num)))
          (if (and (eq nqmbm-handle-internal-buffers 'seperate)
                   (nqmbm-internal-buffer-p buffer))
              (push buffer internal-buffers)
            (push buffer other-buffers)))))
    (setq most-recently-used-buffers (reverse most-recently-used-buffers)
          other-buffers (sort other-buffers sort-function)
          internal-buffers (sort internal-buffers sort-function))
    (let* ((posx (car (nth 2 (nth 1 event))))
           (posy (cdr (nth 2 (nth 1 event))))
           (wind (nth 0 (nth 1 event)))
           (nqmbm-buffername-maxlen
            (car (sort (mapcar (lambda (elt) (length (buffer-name elt)))
                               (append most-recently-used-buffers
                                       other-buffers
                                       internal-buffers)) '>)))
           (popup-menu
            (nqmbm-make-menu "Buffer Menu" (append (mapcar 'nqmbm-make-menulist-entry most-recently-used-buffers)
                                                   (if most-recently-used-buffers '(("--" . seperator-1)) nil)
                                                   (or (mapcar 'nqmbm-make-menulist-entry other-buffers)
                                                       '((menu-item
                                                          (concat (nth 1 nqmbm-file-modified-tags)
                                                                  "  No other buffers")
                                                          info-1 :enable nil)))
                                                   (if internal-buffers '(("--" . seperator-2)) nil))))
           (buf nil)
           (window nil))
      (when internal-buffers
        (define-key-after popup-menu [internal-buffers]
          (cons (concat (nth 1 nqmbm-file-modified-tags) "  Internal")
                (nqmbm-make-menu "Internal Buffers" (mapcar 'nqmbm-make-menulist-entry internal-buffers)))
          'seperator-2))
      (setq buf (car (reverse (x-popup-menu (list (list posx (- posy nqmbm-popup-y-offset)) wind) popup-menu))))
      (setq window (posn-window (event-start event)))
      (when buf
        (or (framep window) (select-window window))
        (switch-to-buffer (symbol-name buf))))))


(defun nqmbm-make-menulist-entry (buffer)
  "Constructs an entry for the menu based on a buffer."
  (cons
   (concat (if (and (buffer-modified-p buffer) (buffer-file-name buffer))
               (nth 0 nqmbm-file-modified-tags)
             (nth 1 nqmbm-file-modified-tags))
           ;; space between tags and buffer name
           "  "
           ;; buffername and maybe filename
           (if (eq nqmbm-max-filename-length 0)
               (buffer-name buffer)
             (format (cond ((eq nqmbm-seperate-columns 'tabulator) "%s\t%s")
                           ((eq nqmbm-seperate-columns 'spaces) (format "%%-%ds  %%s" nqmbm-buffername-maxlen))
                           (t "%s (%s)"))
                     (buffer-name buffer)
                     (or (let ((path (buffer-file-name buffer)))
                           (if (and (numberp nqmbm-max-filename-length) (> (length path) nqmbm-max-filename-length))
                               (concat "..." (substring path (- nqmbm-max-filename-length)))
                             path))
                         ""))))
   buffer))

(defun nqmbm-internal-buffer-p (buffer)
  "Return t if BUFFER is an internal buffer like *Messages* or *scratch*."
  ;; Internal buffer if first char of buffername is a "*"
  (string= (substring (buffer-name buffer) 0 1) "*"))

(defun nqmbm-normal-sort-function (elt1 elt2)
  ;; make sure internal buffers are sorted last
  (let ((name1 (concat (if (nqmbm-internal-buffer-p elt1) "2" "1")
                       (buffer-name elt1)))
        (name2 (concat (if (nqmbm-internal-buffer-p elt2) "2" "1")
                       (buffer-name elt2))))
    (string< name1 name2)))

(defun nqmbm-ignore-case-sort-function (elt1 elt2)
  ;; make sure internal buffers are sorted last
  (let ((name1 (concat (if (nqmbm-internal-buffer-p elt1) "2" "1")
                       (buffer-name elt1)))
        (name2 (concat (if (nqmbm-internal-buffer-p elt2) "2" "1")
                       (buffer-name elt2))))
    (string< (downcase name1) (downcase name2))))

(defun nqmbm-make-menu (title entries)
  (let ((foo (make-sparse-keymap title)))
    (dolist (entry (reverse entries))
      (define-key
        foo
        (vector (if (bufferp (cdr entry))
                    (make-symbol (buffer-name (cdr entry)))
                  (cdr entry)))
        entry))
    foo))

(provide 'nqmbm)
