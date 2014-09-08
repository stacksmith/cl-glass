A simple 'glass' fixed-width font console for text applications using SDL.

Rationale
---------
I could not find a simple, stable solution for a text UI.  ncurses bindings were painful and do not provide good keyboard support.  Well, this is Lisp, and I am an old hacker...

Description
-----------
cl-glass generally follows the patters of lispbuilder-sdl.  Default font and glass are created upon initialization.  Functions take optional :glass parameter in case you have more then one, or use the default.  Easy does it.

SDL-ttf font rendering is too ugly, so until a better solution is found, I created 'glass-fonts' - fonts pre-imaged by Emacs.  These are found in the *default-font-dir*

Documentation
-------------

Note: surface and glass will use defaults if not explicitly passed...

initialize &key surface           call after initializing SDL. 

out str &key glass                output a string

gotoxy x y &key glass             move cursor to x y location

cr &key glass                     move cursor to next line

clear &key glass                  clear glass

TODO:
----
It may be necessary to free cffi objects...
