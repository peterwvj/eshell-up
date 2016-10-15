

(ert-deftest linux-common-usage-test ()
  (let ((sep "/")
        (current-path "/home/user/first/second/third/"))
    (should (equal "/home/user/first/" (find-parent-dir "st" current-path sep)))
    (should (equal "/home/user/first/second/" (find-parent-dir "s" current-path sep)))
    (should (equal "/home/user/first/second/third/" (find-parent-dir "h" current-path sep)))
    (should (equal "/home/user/" (find-parent-dir "ser" current-path sep)))
    (should (equal "/home/" (find-parent-dir "hom" current-path sep)))))

(ert-deftest windowsx-common-usage-test ()
  (let ((sep "\\")
        (current-path "C:\\Program Files\\WindowsApps\\first"))
    (should (equal "C:\\Program Files\\WindowsApps\\first" (find-parent-dir "fi" current-path sep)))
    (should (equal "C:\\Program Files\\WindowsApps\\" (find-parent-dir "sA" current-path sep)))
    (should (equal "C:\\Program Files\\" (find-parent-dir " " current-path sep)))
    (should (equal "C:\\" (find-parent-dir ":" current-path sep)))))

 
(ert-deftest case-test ()
  (let ((sep "/")
        (current-path "/path/paTh/pATh/PATH/"))
    (setq go-up-ignore-case t)
    (should (equal "/path/paTh/pATh/PATH/" (find-parent-dir "pa" current-path sep)))
    (setq go-up-ignore-case nil)
    (should (equal "/path/paTh/" (find-parent-dir "pa" current-path sep)))
    (should (equal "/path/paTh/pATh/PATH/" (find-parent-dir "PATH" current-path sep)))
    (should (equal "/path/" (find-parent-dir "pat" current-path sep)))))
