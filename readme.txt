Team members: Pratyaksh Sharma (120050019)
              Nishant Kumar Singh (120050043)

Files: 
       che.ss
       This file has the code for parsing our chess engine's move to the xboard .
       It is this file that has to be made executable in order to run our program.

       base.ss 
       This file provides basic definitions like boards , 
       pieces , copy-board etc.

       validmoves.ss:
       This file includes the functions related to making moves on the board,
       checking whether the move is valid or not etc.

       check.ss
       This as the name suggests checks whether in a given position of the board
       , any side's king is under check or not. The check function can also 
       find the attack and defence values on a piece other than king.

       engine.ss
       This file is the core of our chess engine . It has the evaluation function 
       and the best move selector. 

       
Instructions (How to Run:)
   
       To run our engine the system needs to have xboard(for ubuntu) or winboard
       (for windows) installed. 
       
       	Installing xboard:
	On ubuntu: sudo apt-get install xboard
	On windows: download from http://www.chess.com/download/view/winboard-427 
	and install
	

	To play against the engine navigate (from terminal) to the folder containing
	the file(executable) 'final', and use the command: 
		xboard -fcp ./final
	
	To play our program against another engine (engine2), use
		xboard -fcp /path/engine2 -scp /path/final
	And click on Mode>Two Machines to start.


      
