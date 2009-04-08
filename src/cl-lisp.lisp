;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-LISP; -*-

;; cl-lisp.lisp --- Lisp identification functions

;; Copyright (c) 2006-2009 Paul Ward <asmodai@gmail.com>

;; Version:    1.0
;; Time-stamp: <Wednesday Apr  8, 2009 06:21:31 asmodai>

;; Author:     Paul Ward <asmodai@gmail.com>
;; Maintainer: Paul Ward <asmodai@gmail.com>
;; Created:    Tue Mar  7 17:44:34 2006
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

;; It appears that there is no real common "standard" for what
;; the SOFTWARE-* and MACHINE-* functions return, so... let's define
;; some functions that return common information across all Lisp
;; platforms.
;;
;; This is probably the only module that resulted in me spending
;; too much time trying out as many Lisp flavours as I could get my
;; grubby hands on, now that's something to treasure for the rest
;; of my life.

;; p.s. I hope you have folding-mode :)

;; }}}
;; {{{ Code:
;; ===================================================================

(in-package :common-lisp-user)

;; -------------------------------------------------------------------
;; {{{ DEFPACKAGE:

#-medley
(defpackage :cl-lisp
  (:use #:common-lisp)
  (:export
   #:common-lisp-implementation-type
   #:common-software-type
   #:common-software-version
   #:common-machine-type
   #:common-machine-version
   )
  ;; {{{ Documentation:
  #-poplog
  (:documentation
   "CL-LISP - Common Lisp Identification.

This package was designed as a work-around for various lossages
within Common Lisp implementations.

Both ``Common Lisp, The Language'' and ``ANSI Common Lisp'' fail to
define how LISP-IMPLEMENTATION-TYPE, SOFTWARE-TYPE,
SOFTWARE-VERSION, MACHINE-TYPE, and MACHINE-VERSION work.

The result is an inconsistency across many Common Lisp platforms
which can lead to confusion if one wishes to present such
information to the public.

Take the following conversations with LispWorks, ECL, SBCL, GCL,
and Allegro:

LispWorks:
 (software-type)
=> ``Linux''
 (software-version)
=> ``Unknown version''

ECL:
 (software-type)
=> ``linux-gnu''
 (software-version)
=> ``unknown''

SBCL:
 (software-type)
=> ``Linux''
 (software-version)
=> ``2.4.29''

GCL (same box as all the others):
 (software-type)
=> ``BSD''
 (software-version)
=> ``BSD''
 (machine-type)
=> NIL

Allegro:
 (software-type)
=> ``Linux 2.x, Redhat 6.x and 7.x''
 (software-version)
=> ``Linux galileo 2.4.29...''

From those listener conversations, we can see that the replies are
all implementation-dependent, not the best choice in the world.

In fact, GCL's implementation of MACHINE-TYPE is:

(defun machine-type ()
  #+sun ``SUN''
  #+hp-ux ``HP-UX''
  #+eclipse ``ECLIPSE''
  #+vax ``VAX''
  )


Given that the Single Unix Specification defines uname(1) with a
strictness akin to a rather strict drill sergeant, we should really
return values here that are consistent across many platforms.

CL-LISP aims to do just this.

The defined functions in this package are:
COMMON-LISP-IMPLEMENTATION-TYPE
COMMON-SOFTWARE-TYPE
COMMON-SOFTWARE-VERSION
COMMON-HARDWARE-TYPE
COMMON-HARDWARE-VERSION

There is no version of MACHINE-INSTANCE here, as that is just
totally undefined.  Most Common Lisp systems return the hostname
of the machine, others return information such as serial numbers,
or a hardware ID key.  Comments on this will be welcome.

Supported Lisp Systems:

ABCL, CLISP, CMU, ECL, GCL (ANSI), LispWorks, MCL, OpenMCL,
Poplog, Scieneer.")
  ;; }}}
  )

;; }}}
;; -------------------------------------------------------------------

(in-package :cl-lisp)

;; -------------------------------------------------------------------
;; {{{ Variables:

(defvar *software-version* nil
  "Software version string.")

(defvar *machine-version* nil
  "Machine version string.")

(defvar *machine-type* nil
  "Machine type string.")

;; }}}
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; {{{ DELIMITED-STRING-TO-LIST and other utility functions:

(defun position-char (char string start max)
  ;; {{{ Documentation:
  "Syntax:
POSITION-CHAR char string start max => output

Arguments and Values:
char - a character.
string - a string.
start - an integer.
max - an integer.
output - result of operation.

Description:
Returns the position of CHAR within STRING, starting from START, and
processing MAX number of characters from the given start.

Examples:
 (position-char #\m ``A man and a cat'' 0 15)
=> 2
 (position-char #\l ``Hello'' 3 5)
=> 3

Affected By:
None.

See Also:
None.

Notes:
This has been taken from KMRs' lovely library of CL utility functions.  I
included it her verbatim in order to save requiring the KMRCL package."
  ;; }}}
  (declare (optimize (speed 3) (safety 0) (space 0))
           (fixnum start max)
           (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char= char (schar string i)) (return i))))

(defun delimited-string-to-list (string &optional (separator #\space)
                                 skip-terminal)
  ;; {{{ Documentation:
  "Syntax:
DELIMITED-STRING-TO-LIST string &optional separator skip-terminal => output

Arguments and Values:
string - a string.
separator - a character.
skip-terminal - a boolean.

Description:
Processes a delimited string and returns each portion as an element in a list.

If SKIP-TERMINAL is T, then any empty list element after a terminal separator
will be skipped.

Examples:
 (delimited-string-to-list ``this is a list'')
=> (``this'' ``is'' ``a'' ``list'')
 (delimited-string-to-list ``this:is:a:list'' #\:)
=> (``this'' ``is'' ``a'' ``list'')
 (delimited-string-to-list ``this-is-a-list-'' #\-)
=> (``this'' ``is'' ``a'' ``list'' ``'')
 (delimited-string-to-list ``this-is-a-list-'' #\- t)
=> (``this'' ``is'' ``a'' ``list'')

Affected By:
None.

See Also:
None.

Notes:
This has been taken from KMRs' lovely library of CL utility functions.  I
included it her verbatim in order to save requiring the KMRCL package."
  ;; }}}
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))
           (type string string)
           (type character separator))
  (do* ((len (length string))
        (output '())
        (pos 0)
        (end (position-char separator string pos len)
             (position-char separator string pos len)))
       ((null end)
        (if (< pos len)
            (push (subseq string pos) output)
            (when (or (not skip-terminal) (zerop len))
              (push "" output)))
        (nreverse output))
    (declare (type fixnum pos len)
             (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

;; }}}
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; {{{ RUN-PROGRAM utility function:

(defun join-cmd-and-args (cmd args)
  ;; {{{ Documentation:
  "Syntax:
JOIN-CMD-AND-ARGS cmd args => string

Arguments and Values:
cmd - a string.
args - a list.
string - a string.

Description:
Joins together a command and an argument list into a single
string.

Examples:
 (join-cmd-and-args ``/bin/uname'' '(``-r''))
=> ``/bin/uname -r''

Affected By:
None.

See Also:
None.

Notes:
None."
  ;; }}}
  (format nil "~A ~{~A ~}" cmd args))

(defun run-program (cmd &optional (args ()))
  ;; {{{ Documentation:
  "Syntax:
RUN-PROGRAM cmd &optional args => output

Arguments and Values:
cmd - a string.
args - a list.
output - result of operation.

Description:
Executes a command and returns the results.

Examples:
 (run-program ``/bin/uname'')
=> ``Linux''
 (run-program ``/bin/arch'')
=> ``i686''

Affected By:
None.

See Also:
None.

Notes:
This is trying to be portable, but there are a few Lisp implementations
that I don't have.  Your mileage may vary."
  ;; }}}
  (string-trim '(#\Newline)
               (with-output-to-string (s)
                 ;; {{{ Armed Bear:
                 #+abcl			; in run-shell-command.abcl
                 (extensions:run-shell-command
                  (join-cmd-and-args cmd args)
                  :output s)
                 ;; }}}
                 ;; {{{ Steel Bank
                 #+sbcl
                 (sb-ext:run-program
                  cmd args
                  :input nil :output s)
                 ;; }}}
                 ;; {{{ CMU or Scieneer:
                 #+(or cmu scl)
                 (ext:run-program
                  cmd args
                  :input nil :output s)
                 ;; }}}
                 ;; {{{ Franz Allegro on UNIX:
                 #+(and allegro unix)
                 (multiple-value-bind (out err pid)
                     (excl:run-shell-command
                      (join-cmd-and-args cmd args)
                      :output :stream
                      :wait nil)
                   (write-string (read-line out) s))
                 ;; }}}
                 ;; {{{ LispWorks on UNIX:
                 #+(and lispworks unix)
                 (multiple-value-bind (out err pid)
                     (system:run-shell-command
                      (join-cmd-and-args cmd args)
                      :wait nil
                      :output :stream
                      :error-output :stream)
                   (write-string  (read-line out) s))
                 ;; }}}
                 ;; {{{ CLISP on UNIX:
                 #+(and clisp unix)	; from runprog.lisp
                 (let ((out
                        (ext:run-program
                         cmd
                         :arguments args
                         :output :stream
                         :wait nil)))
                   (write-string (read-line out) s))
                 ;; }}}
                 ;; {{{ OpenMCL:
                 #+openmcl
                 (nth-value 1
                            (ccl:run-program
                             "/bin/sh"
                             (append  '("-c")
                                      (list (join-cmd-and-args cmd args)))
                             :input nil
                             :output s
                             :wait t))
                 ;; }}}
                 ;; {{{ ECL:
                 #+ecl			; from unixsys.d
                 (let ((out
                        (ext:run-program
                         cmd
                         args
                         :output :stream
                         :error :stream)))
                   (write-string (read-line out) s))
                 ;; }}}
                 ;; {{{ Poplog:
                 #+poplog		; from contrib.lsp et al
                 (progn
                   (pushnew "$popcontrib/lisp/modules/"
                            poplog:*module-directory-list* :test #'equal)
                   (require "run-unix-program.lsp")
                   (multiple-value-bind (out err pid)
                       (poplog:run-unix-program cmd
                                                :args args
                                                :output :stream
                                                :error-output :stream)
                     (write-string (read-line out) s)))
                 ;; }}}
                 ;; {{{ GCL:
                 #+(and gcl unix)	; from gcl-si.dvi
                 (let ((out (si:run-process
                             "/bin/sh"
                             (append
                              '("-c")
                              (list (join-cmd-and-args cmd args))))))
                   (write-string (read-line out) s))
                 ;; }}}
                 ;; {{{ Unknown:
                 #-(or abcl sbcl cmu scl allegro lispworks clisp
                       openmcl ecl poplog gcl)
                 (error "RUN-PROGRAM is not implemented.")
                 ;; }}}
                 )))

;; }}}
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; {{{ Lisp information:

;;
;; If any systems here pre-date Common Lisp, or if any are missing...
;; let me know.
(defun common-lisp-implementation-type ()
  ;; {{{ Documentation:
  "Syntax:
COMMON-LISP-IMPLEMENTATION-TYPE <no arguments> => description

Arguments and Values:
description---a string or nil.

Description:
Returns a string that identifies the generic name of the particular
Common Lisp implementation.

If no appropriate and relevant result can be prodiced, nil is
returned instead of a string.

Examples:
 (lisp-implementation-type)
=> ``Steel Bank Common Lisp''
OR=> ``Guy's Common Lisp''

Notes:
None."
  ;; }}}
  (let ((lispm
         #+abcl "Armed Bear Common Lisp"
         #+(and akcl (not gcl)) "Austin Kyoto Common Lisp"
         #+allegro "Franz Allegro Common Lisp"
         #+clisp "GNU Common Lisp (CLISP)"
         #+CLOE-Runtime "Symbolics CLOE"
         #+cmu "CMU Common Lisp"
         #+cormanlisp "Corman Common Lisp"
         #+ecl "Embeddable Common Lisp"
         #+(and excl (not allegro)) "Franz Common Lisp"
         #+gcl "GNU Common Lisp (GCL)"
         #+gclisp "Gold Hill Golden Common Lisp"
         #+genera "Symbolics Genera"
         #+hp-hplabs "HP Common Lisp"
         #+ibcl "Ibuki Common Lisp"
         #+imach "Symbolics Ivory"
         #+(and kcl (not gcl akcl)) "Kyoto Common Lisp"
         #+lispworks "LispWorks Common Lisp"
         #+Lucid "Lucid Common Lisp"
         #+mcl "Mac Common Lisp"
         #+openmcl "Clozure Common Lisp"
         #+poplog "Sussex Poplog Common Lisp"
         #+pyramid "Pyramid Common Lisp"
         #+sbcl "Steel Bank Common Lisp"
         #+scl "Scieneer Common Lisp"
         #+ti "Texas Instruments Common Lisp"
         #+(and xerox medley) "Xerox InterLisp (Venue Medley)"
         #+(and xerox (not MEDLEY)) "Xerox InterLisp"
         ))
    lispm))
	 
;; }}}
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; {{{ Software information:

;; GCL hackery is ugly isn't it.
(defun common-software-type ()
  ;; {{{ Documentation:
  "Syntax:
COMMON-SOFTWARE-TYPE <no arguments> => description

Arguments and Values:
description - a string or nil.

Description:
Returns a string that identifies the generic name of any relevant
supporting software, or nil if no appropriate or relevant result can
be produced.

Examples:
 (common-software-type)
=> ``Linux''
OR => ``SunOS''

Affected By:
Operating system environment.

See Also:
COMMON-SOFTWARE-VERSION

Notes:
GNU (Kyoto) Common Lisp (a.k.a GCL) seems to have both :LINUX and
:BSD in *FEATURES*.  I am presuming that the :LINUX symbol is not
present in BSD."
  ;; }}}
  (let ((os #+(and linux (not gcl)) "Linux"
            #+sunos "SunOS"
            #+osf1 "OSF/1"
            #+(and gcl linux bsd) "Linux"
            #+(and gcl bsd (not linux)) "BSD"
            #+darwin "Darwin"
            #+freebsd "FreeBSD"
            #+openbsd "OpenBSD"
            #+netbsd "NetBSD"
            #+mswindows "Microsoft Windows"
            #+(and unix (not (or bsd linux sunos osd1 darwin
                                 freebsd openbsd netbsd)))
            "UNIX"
            ))
    os))

(defun common-software-version ()
  ;; {{{ Documentation:
  "Syntax:
COMMON-SOFTWARE-VERSION <no arguments> => description

Arguments and Values:
description - a string or nil.

Description:
Returns a string that identifies the version of any relevant
supporting software, or nil if no appropriate or relevant result can
be produced.

Examples:
 (common-software-version)
=> ``2.4.29''
OR=> ``5.10''

Affected By:
Operating system environment, *SOFTWARE-VERSION*.

See Also:
COMMON-SOFTWARE-TYPE

Notes:
None."
  ;; }}}
  #+unix
  (or *software-version*
      (setf *software-version*
            #+unix (run-program
                    #+(or darwin bsd) "/usr/bin/uname"
                    #-(or darwin bsd) "/bin/uname"
                    '("-r"))
            ))
  #-unix
  nil
  )

;; }}}
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; {{{ Machine information:

;; TODO: Add support for BSD's sysctl et al.
(defun get-machine-version ()
  ;; {{{ Documentation:
  "Syntax:
GET-MACHINE-VERSION <no arguments> => description

Arguments and Values:
description - a string.

Description:
Ascertains and returns the machine version from either the operating
system or the underlaying hardware.

Example:
 (get-machine-version)
=> ``Intel(R) Core(TM)2 Duo CPU''
OR=> ``UltraSPARC-IIi''

Notes:
None."
  ;; }}}
  #+(and linux (not abcl))
  (with-open-file (stream "/proc/cpuinfo"
                          :if-does-not-exist nil)
    (loop with line while (setf line (read-line stream nil))
          when (eql (search "model name" line) 0)
          return (string-trim " " (subseq line
                                          (1+ (position #\: line))))))
  #+abcl
  (java:jstatic "getProperty" "java.lang.System" "java.version")
  #+darwin
  (string-trim '(#\space #\tab)
               (cadr
                (delimited-string-to-list
                 (run-program "/usr/sbin/sysctl" '("machdep.cpu.brand_string"))
                 #\:)))
  #-(or linux abcl darwin)
  "Unknown"
  )

(defun common-machine-version ()
  ;; {{{ Documentation:
  "Syntax:
COMMON-MACHINE-VERSION <no arguments> => description

Arguments and Values:
description - a string or nil.

Description:
Returns a string that identifies the version of the computer hardware
on which Common Lisp is running, or nil if no such value can be
produced.

Examples:
 (common-machine-version)
=> ``Intel Pentium III''
OR=> ``UltraSPARC-IIi''

Affected By:
The machine version.

See Also
COMMON-MACHINE-TYPE, *MACHINE-VERSION*

Notes:
None."
  ;; }}}
  (or *machine-version*
      (setf *machine-version*
            (get-machine-version))))

(defun get-machine-type ()
  ;; {{{ Documentation:
  "Syntax:
GET-MACHINE-TYPE <no arguments> => description

Arguments and Values:
description - a string.

Description:
Ascertains and returns the machine type either using *FEATURES*
or from the OS and/or hardware.

Examples:
 (get-machine-type)
=> ``x86''
OR=> ``SPARCv9''

Notes:
None."
  ;; }}}  
  (let* ((mtype "Unknown"))
    #+abcl
    (setf mtype "Java Virtual Machine")
    #+(or x86 pc386 pc iapx386 i386 i486 i586 i686 pentium
          pentiummmx pentiumpro pentium2 pentium3 pentium4)
    (setf mtype "x86")
    #+(or x86_64 x86-64)
    (setf mtype "x86-64")
    mtype))

(defun common-machine-type ()
  ;; {{{ Documentation:
  "Syntax:
COMMON-MACHINE-TYPE <no arguments> => description

Arguments and Values:
description - a string or nil.

Description:
Returns a string that identifies the generic name of the computer
hardware on which Common Lisp is running.

Examples:
 (common-machine-type)
=> ``x86''
OR=> ``SPARCv9''

Affected By:
The machine type.

See Also:
COMMON-MACHINE-VERSION, *MACHINE-TYPE*

Notes:
None."
  ;; }}}
  (or *machine-type*
      (setf *machine-type*
            (get-machine-type))))

;; }}}
;; -------------------------------------------------------------------

;; ===================================================================
;; }}}

;; cl-lisp.lisp ends here
