;;; karabiner-rule.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This script run like below
;;
;;   $ emacs --batch -l /.make/$< --eval='(karabiner-rule-print-json shingeta-rule)'

;;; Code:

(prog1 "Change user-emacs-directory"
  ;; enable debug
  (setq debug-on-error  t
        init-file-debug t)

  ;; you can run like 'emacs -q -l ~/hoge/init.el'
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name)))))

(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-initialize))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)       ; renew local melpa cache if fail
         (package-install 'leaf))))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))

  (prog1 "optional packages for leaf-keywords"
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone  . t)))))

(leaf json :require t)
(leaf json-mode :ensure t :require t)


(defconst karabiner-rule-us
  (cdr '(:dummy
         \`  \1  \2  \3  \4  \5  \6  \7  \8  \9  \0   -   =  nil
         nil  q   w   e   r   t   y   u   i   o   p  \[  \]  \\
         nil  a   s   d   f   g   h   j   k   l  \;  \'  nil nil
         nil  z   x   c   v   b   n   m  \,  \.   /  nil nil nil))
  "The keyboard layout of US.")

(defun karabiner-rule-make-rule-from-table (layout tree &optional basekey)
  (declare (indent 1))
  (let* ((lout layout)
         (key  (nth 0 tree))
         (rule (nth 1 tree))
         (next (nth 2 tree))
         lst)
    (if (not (= (length lout) (length rule)))
        (error "There is no one-to-one correspondence.")
      (while lout
        (let ((lkey (pop lout))
              (out  (pop rule)))
          (when (and lkey out)
            (push `(,(append (and basekey `(,basekey)) (and key `(,key)) (and lkey `(,lkey))) . ,out) lst))))
      (append (nreverse lst)
              (when next
                (mapcan
                 (lambda (elm)
                   (karabiner-rule-make-rule-from-table layout elm (append (and basekey `(,basekey)) (and key `(,key)))))
                 next))))))

(defconst shingeta-rule
  (karabiner-rule-make-rule-from-table karabiner-rule-us
    ;;              '(:dummy
    ;;                \`    \1    \2    \3    \4    \5    \6    \7    \8    \9    \0     -     =    nil
    ;;                nil    q     w     e     r     t     y     u     i     o     p    \[    \]    \\
    ;;                nil    a     s     d     f     g     h     j     k     l    \;    \'    nil   nil
    ;;                nil    z     x     c     v     b     n     m    \,    \.     /    nil   nil   nil)
    `(nil ,(cdr     '(:dummy
                      nil   "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"   "0"   "-"   "="   nil
                      nil   "-"   "ni"  "ha"  ","   "ti"  "gu"  "ba"  "ko"  "ga"  "hi"  "ge"  nil   nil
                      nil   "no"  "to"  "ka"  "nn"  "xtu" "ku"  "u"   "i"   "si"  "na"  nil   nil   nil
                      nil   "su"  "ma"  "ki"  "ru"  "tu"  "te"  "ta"  "de"  "."   "bu"  nil   nil   nil))
          ((k ,(cdr '(:dummy
                      nil   "xa"  "xi"  "xu"  "xe"  "xo"  nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "fa"  "go"  "hu"  "fi"  "fe"  nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "ho"  "ji"  "re"  "mo"  "yu"  nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "du"  "zo"  "bo"  "mu"  "fo"  nil   nil   nil   nil   nil   nil   nil   nil)))
           (d ,(cdr '(:dummy
                      nil   nil   nil   nil   nil   nil   nil   nil   "["   "]"   ";"   "@"   nil   nil
                      nil   nil   nil   nil   nil   nil   "wi"  "pa"  "yo"  "mi"  "we"  "wo"  nil   nil
                      nil   nil   nil   nil   nil   nil   "he"  "a"   "re"  "o"   "e"   nil   nil   nil
                      nil   nil   nil   nil   nil   nil   "se"  "ne"  "be"  "pu"  "vu"  nil   nil   nil)))
           (l ,(cdr '(:dummy
                      nil   "xya" "mya" "myu" "myo" "xwa" nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "di"  "me"  "ke"  "thi" "dhi" nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "wo"  "sa"  "o"   "ri"  "zu"  nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "ze"  "za"  "gi"  "ro"  "nu"  nil   nil   nil   nil   nil   nil   nil   nil)))
           (s ,(cdr '(:dummy
                      nil   nil   nil   nil   nil   nil   nil   nil   "("   ")"   ":"   "*"   nil   nil
                      nil   nil   nil   nil   nil   nil   "she" "pe"  "do"  "ya"  "je"  nil   nil   nil
                      nil   nil   nil   nil   nil   nil   "bi"  "ra"  "ji"  "sa"  "so"  nil   nil   nil
                      nil   nil   nil   nil   nil   nil   "wa"  "da"  "pi"  "po"  "che" nil   nil   nil)))
           (i ,(cdr '(:dummy
                      nil   "xyu" "bya" "byu" "byo" nil   nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "hyu" "syu" "syo" "kyu" "tyu" nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "hyo" nil   nil   "kyo" "tyo" nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "hya" nil   "sya" "kya" "tya" nil   nil   nil   nil   nil   nil   nil   nil)))
           (o ,(cdr '(:dummy
                      nil   "xyo" "pya" "pyu" "pyo" nil   nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "ryu" "ju"  "jo"  "gyu" "nyu" nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "ryo" nil   nil   "gyo" "nyo" nil   nil   nil   nil   nil   nil   nil   nil
                      nil   "rya" nil   "ja"  "gya" "nya" nil   nil   nil   nil   nil   nil   nil   nil))))))
  "Shingeta rule table")

(defun karabiner-rule-json-buffer ()
  (save-excursion
    (progn
      (dotimes (_ 2)
        (forward-char)
        (progn (forward-sexp 2) (forward-char 1) (insert "\n"))
        (progn (forward-sexp 1) (forward-char 2) (insert "\n")))
      (ignore-errors
        (while t
          (forward-char) (insert "\n")
          (dotimes (_ 4)
            (progn (forward-sexp 2) (forward-char 1) (insert "\n")))
          (delete-char -1)
          (forward-char) (insert "\n")))
      (delete-char -1)
      (backward-char 1) (delete-char -1)))
  (save-excursion (while (search-forward ":" nil t) (replace-match " : " nil t)))
  (save-excursion (while (search-forward "," nil t) (replace-match ", " nil t)))
  (delete-trailing-whitespace)
  (let ((js-indent-level 2))
    (json-mode)
    (indent-region (point-min) (point-max)))
  (align-regexp (point-min) (point-max) "\\(\\s-*\\):"))

(defmacro karabiner-rule-print-json (var)
  `(with-temp-buffer
     (let ((rule (mapcar
                  (lambda (elm)
                    (let ((from (car elm))
                          (to   (cdr elm)))
                      `((:type . "basic")
                        (:conditions . (((:type . "input_source_if")
                                         (:input_sources . (((:language . "ja")))))))
                        (:from . ,(if (= 1 (length (if (listp from) from (split-string from "" 'omit))))
                                      (car (mapcar
                                            (lambda (elm)
                                              `((:key_code . ,elm)))
                                            (if (listp from) from (split-string from "" 'omit))))
                                    `((:simultaneous . ,(mapcar
                                                         (lambda (elm)
                                                           `((:key_code . ,elm)))
                                                         (if (listp from) from (split-string from "" 'omit)))))))
                        (:to   . ,(let ((ret (mapcar
                                              (lambda (elm)
                                                `((:key_code . ,elm)))
                                              (if (listp to) to (split-string to "" 'omit)))))
                                    (if (= 1 (length ret))
                                        `((,(caar (last ret)) (repeat . :json-false)))
                                      (setcdr (last ret 2) `((,(caar (last ret)) (repeat . :json-false))))
                                      ret))))))
                  ,var)))
       (erase-buffer)
       (save-excursion
         (insert
          (json-encode
           `((title . "日本語用かな配列")
             (rules . (((description . "新下駄配列 ver. 1.0 (for keyboard type: jis)")
                        (manupulators . ,rule))))))))
       (karabiner-rule-json-buffer)
       (princ (buffer-substring-no-properties (point-min) (point-max)))
       (princ "\n"))
     nil))

(provide 'karabiner-rule)
;;; karabiner-rule.el ends here
