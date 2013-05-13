fs-mode
=======

Linux file system management mode for emacs

What is fs-mode?
----------------
fs-mode is a file system management mode for emacs. It can
manage your file system, you can mount or umount file
system.  It also support mount ecryptfs file system.
(I put my little secret into ecryptfs file systems)

How to install it?
------------------
add following code into emacs's init file.

#+BEGIN_SRC emacs-lisp  
  ;; change the directory according to your needs
  (add-to-list 'load-path "directory/to/install/")
  (require 'fs-mode-autoload)
#+END_SRC

How to use it?
--------------
Use M-x list-fs to enter the fs-mode. and there are
following key shortcuts bindings.

m - mount a file system.
u - umount a file system.
q - quit the fs-mode.

Use root-cmd.el to execute commands which needs root privilege.
---------------------------------------------------------------

When I wrote the fs-mode, I need a elisp package to make a
process with root privilege, because the mount and umount
command need root privilege. So I make root-cmd package to
execute root command.

It is really convenient to use the root-cmd. Following is a
sample to illustrate how to use it.

#+BEGIN_SRC emacs-lisp
  
  (require 'root-cmd)
  ;; execute a root command
  (root-cmd "ls /root/")
  ;; return the output of above command if needed.
  ;; you can get the output directly from variables root-cmd-output-keep
  (root-cmd-output)
  ;; stop the root command process.
  (root-cmd-stop)
  
#+END_SRC 

