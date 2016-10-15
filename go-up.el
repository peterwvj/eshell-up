;;; go-up.el --- Quickly go to a specific parent directory in eshell

;; Copyright (C) 2016 Peter W. V. Tran-Jørgensen

;; Author: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; Maintainer: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; URL: https://github.com/peterwvj/go-up
;; Created: 14th October 2016
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
;; Keywords: eshell

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Package for quickly navigating to a specific parent directory in
;; eshell without having to repeatedly typing 'cd ..'.  This is
;; achieved using the 'go-up' function, which can be bound to an
;; eshell alias such as 'up'.  As an example, assume that the current
;; working directory is:
;;
;; /home/user/first/second/third/fourth/fifth $
;;
;; Now, in order to quickly go to (say) the directory named 'first' one
;; simply executes:
;;
;; /home/user/first/second/third/fourth/fifth $ up fir
;; /home/user/first $
;;
;; This command searches the current working directory from right to
;; left for a directory that matches the user's input ('fir' in this
;; case).  If a match is found then eshell changes to that directory,
;; otherwise it does nothing.
;;
;; It is recommended to invoke 'go-up' using an alias as done in the
;; example above.  To do that, add the following to your
;; .eshell.aliases file:
;;
;; alias up go-up $1
;;
;; This package is inspired by 'bd', which uses bash to implement
;; similar functionality.
;; See: https://github.com/vigneshwaranr/bd

;;; Code:

;; User-definable variables

(defvar go-up-ignore-case t "Non-nil if searches must ignore case.")

(defun get-sep (os)
  "Get the file separator, e.g. '/', for an operating system.
Argument OS a string representation of an operating system, e.g. the
value of `system-type'."
  (if (or (string= os "windows-nt") (string= os "ms-dos"))
      "\\"
    "/"))

(defun str-rev (str)
  "Utility function to reverse a string.
Argument STR the string to reverse."
  (apply #'string
         (reverse
          (string-to-list str))))

(defun find-parent-dir (match path sep)
  "Find the parent directory based on the user's input.
Argument MATCH a string that identifies the parent directory to search for.
Argument PATH the source directory to search from.
Argument SEP the file separator, e.g. '/'."
  (if (or (not (stringp match)) (string= "" match))
      path
    (let* ((path-rev (str-rev path))
           (match-rev (str-rev match))
           (case-fold-search go-up-ignore-case)
           (idx-rev (string-match (regexp-quote match-rev) path-rev)))
      (if (not (null idx-rev))
          (let* ((path-length (length path))
                 (idx (- path-length idx-rev)))
            (find-sub-str idx path sep))
        path))))

(defun find-sub-str (i path sep)
  "Find the directory of a path pointed to by an index.
Argument I the PATH index to search from.
Argument SEP the file separator."
  (if (= i (length path))
      path
    (let
        ((c (string (aref path i)) ))
      (if (string= c sep)
          (substring path 0 (+ i 1))
        (find-sub-str (+ i 1) path sep)))))


(defun go-up (match)
  "Go to a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to go
to."
  (let* ((path default-directory)
         (parent-dir (find-parent-dir match path (get-sep system-type))))
    (eshell/cd parent-dir)))

(provide 'go-up)

;;; go-up.el ends here
