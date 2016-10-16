;;; eshell-up.el --- Quickly go to a specific parent directory in eshell

;; Copyright (C) 2016 Peter W. V. Tran-Jørgensen

;; Author: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; Maintainer: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; URL: https://github.com/peterwvj/eshell-up
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
;; achieved using the 'eshell-up' function, which can be bound to an
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
;; It is recommended to invoke 'eshell-up' using an alias as done in the
;; example above.  To do that, add the following to your
;; .eshell.aliases file:
;;
;; alias up eshell-up $1
;;
;; This package is inspired by 'bd', which uses bash to implement
;; similar functionality.
;; See: https://github.com/vigneshwaranr/bd

;;; Code:

;; User-definable variables

(defvar eshell-up-ignore-case t "Non-nil if searches must ignore case.")

(defun eshell-up-find-parent-dir (match path)
  "Find the parent directory based on the user's input.
Argument PATH the source directory to search from.
Argument MATCH a string that identifies the parent directory to search for."
  (let ((case-fold-search eshell-up-ignore-case))
    (locate-dominating-file path
                            (lambda (parent)
                              (let ((dir (file-name-nondirectory
                                          (directory-file-name parent))))
                                (if (string-match match dir)
                                    dir
                                  nil))))))

(defun eshell-up (match)
  "Go to a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to go
to."
  (let* ((path default-directory)
         (parent-dir (eshell-up-find-parent-dir match path)))
    (when parent-dir
      (eshell/cd parent-dir))))

(provide 'eshell-up)

;;; eshell-up.el ends here
