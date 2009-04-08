;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER; -*-

;; cl-lisp.asd --- Lisp information system definition

;; Copyright (c) 2006-2009 Paul Ward <asmodai@gmail.com>

;; Version:    1.0
;; Time-stamp: <Wednesday Apr  8, 2009 06:21:50 asmodai>

;; Author:     Paul Ward <asmodai@gmail.com>
;; Maintainer: Paul Ward <asmodai@gmail.com>
;; Created:    Tue Mar  7 17:44:56 2006
;; Keywords:   Common Lisp Version Information
;; URL:        https://github.com/Asmodai/cl-lisp/tree

;; {{{ License:

;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by
;; the Free Software Foundation, as clarified by the Franz
;; preamble to the LGPL found in
;; http://opensource.franz.com/preamble.html.

;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.

;; Version 2.1 of the GNU Lesser General Public License can be
;; found at http://opensource.franz.com/license.html.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA

;; }}}
;; {{{ Commentary:

;; }}}
;; {{{ Code:
;; ===================================================================

(in-package :common-lisp-user)

(defpackage :cl-lisp-system
  (:use #:asdf #:common-lisp))

(pushnew :cl-lisp *features*)

(in-package :cl-lisp-system)

(defsystem cl-lisp
    :name "cl-lisp"
    :author "Paul Ward <asmodai@gmail.com>"
    :version "1.0"
    :maintainer "Paul Ward <asmodai@gmail.com>"
    :licence "GNU Lesser General Public License"
    :description "Lisp information library for Common Lisp"
    :long-description "cl-lisp provides common information on
Lisp implementation, and software and machine types."
    
    :components
    ((:module :src
              :components
              ((:file "cl-lisp")))))

;; ===================================================================
;; }}}

;; cl-lisp.asd ends here
