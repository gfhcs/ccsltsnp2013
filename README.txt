This is an interpreter for the "Calculus of Communicating Systems" (CCS).
It has been designed for compatibility with the Concurrent Programming Cource 2013 at Saarland University.

This document is not complete yet...

TODO...


CCS: The following procedures are to be used for conversions from string to expression and vice versa. They also handle bindings:

	lex : string -> token list
	
		Takes a CCS expression and breaks it into a list of tokens.
		A note on restriction sets: You have to use \ instead of \\ for the setminus to be recognized properly by Moscow ML.
		
	parse : token list -> bindings * exp
	
		Takes a sequence of tokens and converts it into a CCS syntax tree and a set of bindings.
		A binding of the identifier X to the expression e is denoted as X := e.
		To specify bindings, terminate your expressin with a semicolon (;) and then write down all your bindings. Commata (,) or semicola between bindings are optional and plain white space as binding separator will work as well.
		There will be no complaints if you don't define all the identifiers you are using, so be careful!
		
		The expression syntax is exactly the one defined the NP lectures:
		
			P ::=  0  |  X  |  a.P  |  P + P  |  P '|' P  | P \ H
		
		where a is an arbitrary action name (optionally having the active (!) or the passive (?) suffix) and H is a set of restricted action names.
		H is given in curly braces ({, }). If it contains an action name a without passive/active suffix, this is tread as meaning a, a! and a?.
		The given alternatives for P are given in the order of strength with which they attract their constituents, so A + B | C is to be read as (A + B) | C.
	
	ccs : string -> bindings * exp
	
		Composition of lex and parse: Takes a string and converts it to an expression syntax tree and a set of bindings.
		See 'parse' for more information!
	
	toString: exp -> string
	
		Kind of the inverse of 'ccs': Takes an expression (without bindings!) and converts to a minimally parenthesised string.
		
	matches : action -> action -> bool
	
		Decides if two action names match each other (for synchronisation).

	lookup : bindings -> string -> exp
	
		Takes a set of bindings and an identifier and looks up the definition for the identifier in the set.		
	
	load : string -> bindings * exp
	
		Loads the file identified by the given string and applies ccs to its entire content.
		Note that in order not to override the Moscow ML binding for 'load', load can only be referenced by 'CCS.load' !
		You may change this by declaring
		
			val load = CCS.load
			
LTS: The following procedures are meant to treat an expression as a node in an Labeled Transition System (LTS):

	edges : bindings -> exp -> edge Set.set
	
		Takes a set of bindings and an expression and returns the set of edges that this expression has under these bindings in an LTS.
	
	setFormat : outputFormat -> unit
	getFormat : unit -> outputFormat
	
		Gets or sets the file format for LTS graphs. Available are SVG, PNG and PDF. Standard is SVG.
	
	setFileName : string -> unit
	getFileName : unit -> string
	
		Gets or sets the output file name for LTS graphs. Standard is "graph".
	
	setDotCommand : string -> unit
	getDotCommand : unit -> string
	
		Gets or sets the command for calling the Graphviz tool "dot".
		In general you should not use setDotCommand. Instead you should try to change the command right at the start of the file "lts.sml", where it says
		
			(*  Enter dot path here: *)

			val dot = "dot";
			
		Paste your custom command instead of dot within the quotes.
	
	draw : bindings * exp -> unit
	
		Takes a set of bindings and an expression, converts the resulting LTS to dot code and calls the "dot" tool on this code.
		The return values of parse and CCS.load make good argument values for this procedure!
		To get to know how the output files are namend and in what format the graph will be written, use getFileName and getDotCommand.
		
Stepper: The following procedures give access to an interactive "stepper", that allows you to walk through an LTS:
		
	reset : bindings * exp -> unit
	
	TODO...
	
	current : unit -> bindings * exp
	show  : unit -> unit (* shows the current expression*)
	bindings : unit -> unit
	post  : unit -> unit (* shows the currently possible transitions (action and successor) in a numbered list*)
	succ  : int -> unit (* Switches to the n-th successor in the post list*)
	back  : unit -> unit (* Goes back to the last expression before the current one*)
		