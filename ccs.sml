 	datatype token = NULL | PLUS | DOT | PAR | BACKSLASH | ID of string | LPAR | RPAR | LCPAR | RCPAR | COMMA | SEMICOLON | DEFINE
	datatype exp = Stop | Id of string | Choice of exp * exp | Prefix of exp * exp |  Parallel of exp * exp | Restrict of exp * string Set.set
	exception Parse of string
	
	type binding = string -> exp
	
 structure CCS :> sig
 
	val lex : string -> token list
	val parse : token list -> binding * exp
	
	val ccs : string -> binding * exp
	
	val toString: exp -> string
	val compare : exp * exp -> order
	val matches : string -> string -> bool
	
 end
 = struct
 
 

fun kc (#"0")  = true
|   kc (#"+")  = true
|   kc (#".")  = true
|   kc (#"|")  = true
|   kc (#"\\") = true
|   kc (#"(") = true
|   kc (#")") = true
|   kc (#"{") = true
|   kc (#"}") = true
|   kc (#",") = true
|   kc (#";") = true
|   kc    _    = false



fun lex' nil ((#"0") :: cr) = (NULL, cr)
|   lex' nil ((#"+") :: cr) = (PLUS, cr)
|   lex' nil ((#".") :: cr) = (DOT, cr)
|   lex' nil ((#"|") :: cr) = (PAR, cr)
|   lex' nil ((#"\\") :: cr) = (BACKSLASH, cr)
|   lex' nil ((#"(") :: cr) = (LPAR, cr)
|   lex' nil ((#")") :: cr) = (RPAR, cr)
|   lex' nil ((#"{") :: cr) = (LCPAR, cr)
|   lex' nil ((#"}") :: cr) = (RCPAR, cr)
|   lex' nil ((#",") :: cr) = (COMMA, cr)
|   lex' nil ((#";") :: cr) = (SEMICOLON, cr)
|   lex' nil ((#":") :: (#"=")::cr) = (DEFINE, cr)
|   lex' nil (c::cr) = if Char.isSpace c then lex' nil cr else lex' [c] cr
|   lex' id    nil   = (ID ((implode o rev) id), nil)
|   lex' id ((#":") :: (#"=")::cr) = (ID ((implode o rev) id), (#":") :: (#"=")::cr)
|   lex' id  (c::cr) = if kc c orelse Char.isSpace c then (ID ((implode o rev) id), c::cr)
                       else lex' (c::id) cr
fun eatWS nil = nil
|   eatWS (c::cr) = if Char.isSpace c then eatWS cr else (c::cr) 

fun lex'' nil = nil
|   lex'' cs = let val (t, cr) = lex' nil cs in t :: lex'' (eatWS cr) end


val lex = lex'' o explode


fun match (a,ts) t = if null ts orelse hd ts <> t then raise Error "match" else (a, tl ts)

fun extend (a,ts) p f = let val (a',tr) = p ts in (f(a,a'),tr) end







fun fork a = let val l = String.substring (a, String.size a -1, 1) in
				 if l = "?" orelse l = "!" then [a] else [a ^"?", a^"!"] end
				 
				 
fun matches a a' = (case (rev (explode a), rev (explode a')) of
						  ((#"?")::cr, (#"!")::cr') => (cr = cr')
						 | ((#"!")::cr, (#"?")::cr') => (cr = cr') 
						 |    _                      => false  )
				 
fun assertProper' R =  if Set.isEmpty R then ()
                      else let val a = Set.any R
						       val R' = Set.filter (fn a' => a <> a' andalso not(matches a a')) R
						   in
								if Set.size(R') = Set.size (R) - 2 then assertProper' R'
								else raise Error ("Invalid restriction set: " ^ a ^ " violates the restriction set invariant!")
							end		 
fun assertProper R = (assertProper' R; R)				 
				 
fun parse ts = (case exp ts of
			   (e, SEMICOLON::tr) => ((bindings (fn _=>raise Empty) tr), e)
			 |  (e, _) => (fn _ => raise Empty,e))

and bindings b nil = b
|   bindings b (SEMICOLON::tr) = bindings b tr
|   bindings b (COMMA::tr) = bindings b tr
|   bindings b ((ID id)::DEFINE::tr) = let val (e, tr) = (exp tr) in bindings (update String.compare b id e) tr end
|   bindings b  _                    = raise Parse "Could not parse bindings!"
and exp ts = (case parallel ts of 
               (l, BACKSLASH::tr) => extend (l, tr) set Restrict
              |  s => s)
and set  (LCPAR:: (ID id)::tr) = (case restrictList (Set.set String.compare (fork id)) tr of (R, RCPAR::tr) => (assertProper R, tr)
                                                                       |  _ => raise Parse "Expected '}'!")
|   set _ = raise Parse "Expected a set!"
and restrictList R (COMMA:: (ID id)::tr) = restrictList (Set.union R (Set.set String.compare (fork id))) tr
|   restrictList R tr                    = (R, tr)

and parallel ts = parallel'(choice ts)
and parallel' (l, PAR::tr) = parallel' (extend (l, tr) choice Parallel)
|   parallel' s       = s
and choice ts = choice'(prefix ts)
and choice' (l, PLUS::tr) = choice' (extend (l, tr) prefix Choice)
|   choice' s        = s
and prefix    ts = (case primitive ts of (p, DOT::tr) => extend (p, tr) prefix Prefix | s => s)
and primitive (NULL   :: tr) = (Stop, tr)
|   primitive ((ID s) :: tr) = (Id s, tr)
|   primitive (LPAR :: tr) = match (exp tr) RPAR
|   primitive _ = raise Parse "Expected '0', identifier or parenthesised expression!"


fun toString (Restrict (e, e')) = (parallelToStr e) ^ " \\\\ {" ^ (setToString e') ^"}"
|   toString e = parallelToStr e
and parallelToStr (Parallel (e, e')) = (parallelToStr e) ^ " | " ^ (choiceToStr e')
|   parallelToStr e = choiceToStr e
and choiceToStr (Choice (e, e')) = (choiceToStr e) ^ " + " ^ (prefixToStr e')
|   choiceToStr e = prefixToStr e
and prefixToStr (Prefix (e, e')) = (primitiveToStr e) ^ "." ^ (prefixToStr e')
|   prefixToStr e = primitiveToStr e
and primitiveToStr Stop = "0"
|   primitiveToStr (Id s) = s
|   primitiveToStr  e =  "(" ^ toString e ^ ")"
and setToString s = #1(Set.fold (fn (s, (a, prefix)) => (a^prefix^ s , ", ")) ("", "") (Set.map String.compare (fn s => String.substring (s,0, String.size s - 1)) s))


                    
fun compare (Stop, Stop) = EQUAL
|   compare (Stop, _) = LESS
|   compare (_, Stop) = GREATER
|   compare (Id s, Id s') = String.compare (s, s')
|   compare (Id _, _) = LESS
|   compare (_, Id _) = GREATER
|   compare (Choice t, Choice t') = cmpTuple compare (t, t') 
|   compare (Choice _, _) = LESS
|   compare (_, Choice _) = GREATER
|   compare (Prefix t, Prefix t') = cmpTuple compare (t, t') 
|   compare (Prefix _, _) = LESS
|   compare (_, Prefix _) = GREATER
|   compare (Parallel t, Parallel t') = cmpTuple compare (t, t') 
|   compare (Parallel _, _) = LESS
|   compare (_, Parallel _) = GREATER
|   compare (Restrict (e1, r1), Restrict (e2, r2)) = (case compare (e1, e2) of EQUAL => Set.compare (r1, r2) | s => s )

val ccs = parse o lex

 
 end
 
 open CCS