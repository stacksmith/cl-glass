CL-GLASS
========

A simple 'glass' fixed-width font console for text applications using SDL.

Work-in-progress
----------------

A bare minimum has been implemented.  Font loading and description will be updated soon.

Rationale
---------

I could not find a simple, stable and interactive solution for a text UI.  ncurses bindings were painful and do not provide good keyboard support.  GTK bindings were unstable on my machine, and many possible solutions were rejected as too hard to use interactively in REPL.

Description
-----------
cl-glass generally follows the patters of lispbuilder-sdl.  Default font and glass are created upon initialization.  Functions take optional :glass parameter in case you have more then one, or use the default.

SDL-ttf font rendering is too ugly, so until a better solution is found, I created 'glass-fonts' - fonts pre-imaged by Emacs.  These are found in the *default-font-dir*

Interactive Development
-----------------------

lispbuilder-sdl library provides a 'with init' macro that wraps the entire sdl application, initializing, running and tearing it down.  test-xxx functions show that it's possible to initialize the libraries from REPL, opening a window with an SDL surface.  Now interactive REPL development is possible - drawing to the screen followed by (update-display).  Even the keyboard loop is accessible as long as one of the keys provides a push-quit-event to get out.  Don't forget to shut down sdl to avoid memory leaks.  

Memory leaks
------------

Digging around lispbuilder shows that many allocated foreign objects require free... I will have to figure it out someday...


Documentation
-------------

Note: surface and glass are optional...

-----
**initialize** &key surface 

call after initializing SDL. 
-----
**out** str &key glass from to       

Output a string. No wrapping or escape processing.  Optionally, a portion of the string.
-----
**gotoxy** x y &key glass             

move cursor to x y location
-----
**cr** &key glass pixels                    

move cursor to next line.  Y is advanced by line height or optional pixel amount.
-----
**clear** &key glass                  

clear glass
-----
