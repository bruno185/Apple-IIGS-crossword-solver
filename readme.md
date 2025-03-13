# Apple IIGS Crossword Solver 

## Description :            
This program is a crossword solver for Apple IIGS.\
\
It reads a pattern from the keyboard, and displays all words that match the pattern.
The pattern can contain letters (A-Z, a-z) and '?' as a wildcard standing for any letter.\
The program uses a list of words (WORDS file) to find matching words.\
The program is written in Merlin 16 Assembler, and is intended to run on an Apple IIGS.
The program uses GS/OS calls to manage files I/O.
The program mimic a console application, on a 320x200 graphic screen.

It works for french and english, both provided in this archieve, but can be adapted to any language. Of course, data (Words and index) must be adapted to the language then.\
See the Apple II version here : 
https://github.com/bruno185/Apple-II-crossword-solver-v.2.1-French 
for details on data files and how to create them.\
This Apple IIGS version is a port of the Apple II version, but data are exactly the same.\
\
Copilot integratd in Visual Studio Code helped for comments mainly and for faster code writing.
Merlin32 compiled the source files.
Crossrunner emulator was used to test and debug the program.


