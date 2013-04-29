load "Char";
load "Int";




datatype token = NULL | PLUS | DOT | PAR | BACKSLASH | ID of string | LPAR | RPAR | LCPAR | RCPAR | COMMA | SEMICOLON | DEFINE

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

fun lex nil = nil
|   lex cs = let val (t, cr) = lex' nil cs in t :: lex (eatWS cr) end



structure Set :> sig
   
   type 'a set
   
   val empty : ('a * 'a -> order) -> 'a set
   val set : ('a * 'a -> order) -> 'a list -> 'a set
   val union : 'a set ->  'a set -> 'a set
   val map : ('b * 'b -> order) -> ('a  -> 'b) -> 'a set -> 'b set 
   val filter : ('a -> bool) -> 'a set -> 'a set 
   val element : 'a  ->  'a set -> bool
   val plus :  'a set -> 'a  -> 'a set
   val toList : 'a set -> 'a  list  
   val equal : 'a set -> 'a set -> bool
   val fold : ('a*'b -> 'b) -> 'b -> 'a set -> 'b
   val any : 'a set -> 'a
   val intersection : 'a set ->  'a set -> 'a set
   val exists : ('a -> bool) -> 'a set -> bool
   val compare : ('a set * 'a set) -> order
   val size : 'a set -> int
   val isEmpty : 'a set -> bool
 end
= struct
 
   type 'a set = ('a * 'a -> order) * 'a list
    
    
   fun empty cmp = (cmp, nil) 
     
   fun union' cmp nil s = s
   |   union' cmp s nil = s
   |   union' cmp (x::xs) (y::ys) = case cmp(x, y) of
 			      LESS => x :: union' cmp xs (y::ys)
 			     | EQUAL => x ::union' cmp xs ys
 			     | GREATER => y :: union' cmp (x::xs) ys
     
   fun public h (cmp, xs) (_, ys) = (cmp, h cmp xs ys)
   
   val union = public union'
  
   fun split xs = foldl (fn (x, (l, r)) => (x ::r, l))(nil, nil) xs
   
   fun sort cmp nil = nil
   |   sort cmp [x] = [x]
   |   sort cmp xs = let val (l, r) = split xs in union' cmp (sort cmp l) (sort cmp r) end
   
   
   fun set cmp l = (cmp, sort cmp l)
 			      
   fun map cmp' f (cmp, xs) = set cmp' (List.map f xs)
   
   fun filter p (cmp, xs)= (cmp, List.filter p xs)
   
   fun element' cmp y nil = false
   |   element' cmp y (x::xr) = case cmp(x, y) of
 			  LESS => element' cmp y xr
 			 | EQUAL => true
 			 | GREATER => false
 			
   fun element y (cmp, xs) = element' cmp y xs		
 			
   fun plus' cmp nil y     = [y]
   |   plus' cmp (x::xr) y = case cmp(x, y) of
 			  LESS => x::plus' cmp xr y
 			 | EQUAL => x::xr
 			 | GREATER => y::x::xr
 			
   fun plus (cmp, xs) y = (cmp, plus' cmp xs y)
 			
   fun toList (_, s) = s
   
   fun equal' cmp nil nil = true
   |   equal' cmp nil l = false
   |   equal' cmp l nil = false
   |   equal' cmp (x::xs) (y::ys) = case cmp(x, y) of
 			  LESS => false
 			 | EQUAL => equal' cmp xs ys
 			 | GREATER => false
 			 
   fun equal (cmp, xs) (_, ys) = equal' cmp xs ys
   
   fun fold f a (_, s) = foldl f a s 
   
   fun any (_, nil) = raise Match
   |   any (_, x::xs) = x
   
   fun intersection' cmp nil _ = nil
   |   intersection' cmp _ nil = nil
   |   intersection' cmp (x::xs) (y::ys) = case cmp(x, y) of 
 					  LESS => intersection' cmp xs (y::ys)
 					| GREATER => intersection' cmp (x::xs) ys
 					| EQUAL => x :: (intersection' cmp xs ys)
 					  
   fun intersection (cmp, a) (_, b) = (cmp, intersection' cmp a b)
   
   fun exists p (cmp, nil) = false
   |   exists p (cmp, x::xs) = p x orelse exists p (cmp, xs) 
   
   fun compare' cmp nil nil = EQUAL
   |   compare' cmp  _  nil = GREATER
   |   compare' cmp nil  _  = LESS
   |   compare' cmp (x::xs) (y::ys) = case cmp(x, y) of EQUAL => compare' cmp xs ys | s => s
   
   fun compare ((cmp, xs), (cmp', ys)) = compare' cmp xs ys
   
   fun size (cmp, xs) = List.length xs
   
   fun isEmpty (cmp, nil) = true
   |   isEmpty _ = false
   
 end



exception Error of string

datatype exp = Stop | Id of string | Choice of exp * exp | Prefix of exp * exp |  Parallel of exp * exp | Restrict of exp * string Set.set

fun match (a,ts) t = if null ts orelse hd ts <> t then raise Error "match" else (a, tl ts)

fun extend (a,ts) p f = let val (a',tr) = p ts in (f(a,a'),tr) end




fun update cmp env x a y = if cmp(x,y) = EQUAL then a else env y

exception Parse of string


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


fun cmpTuple cmp ((l1, r1), (l2, r2)) = case cmp (l1, l2) of EQUAL => cmp (r1, r2) | s => s                    
                    
fun compareExp (Stop, Stop) = EQUAL
|   compareExp (Stop, _) = LESS
|   compareExp (_, Stop) = GREATER
|   compareExp (Id s, Id s') = String.compare (s, s')
|   compareExp (Id _, _) = LESS
|   compareExp (_, Id _) = GREATER
|   compareExp (Choice t, Choice t') = cmpTuple compareExp (t, t') 
|   compareExp (Choice _, _) = LESS
|   compareExp (_, Choice _) = GREATER
|   compareExp (Prefix t, Prefix t') = cmpTuple compareExp (t, t') 
|   compareExp (Prefix _, _) = LESS
|   compareExp (_, Prefix _) = GREATER
|   compareExp (Parallel t, Parallel t') = cmpTuple compareExp (t, t') 
|   compareExp (Parallel _, _) = LESS
|   compareExp (_, Parallel _) = GREATER
|   compareExp (Restrict (e1, r1), Restrict (e2, r2)) = (case compareExp (e1, e2) of EQUAL => Set.compare (r1, r2) | s => s )      

                    
fun compareEdge ((l1, a1, r1), (l2, a2, r2)) = case cmpTuple compareExp ((l1,r1),(l2,r2)) of EQUAL => String.compare(a1, a2) | s => s              



fun own e es = Set.map compareEdge (fn (l, a, r) => (e, a, r)) es

fun edges gamma N Stop = Set.set compareEdge nil
|   edges gamma N (Id s) = ((if Set.element s N then Set.set compareEdge nil
							else own (Id s) (edges gamma (Set.plus N s)(gamma s))) handle Empty => Set.empty compareEdge)
|   edges gamma N (Choice (l, r)) = let val (el, er) = (edges gamma N l, edges gamma N r) in
								   Set.union (own (Choice(l,r)) el) (own (Choice(l,r)) er) end
|   edges gamma N (Prefix (Id a, p)) = Set.set compareEdge [(Prefix (Id a, p), a, p)]
|   edges gamma N (Parallel (l, r)) =  let 
									     val e = Parallel (l, r)
										 val (el, er) = (edges gamma N l, edges gamma N r) 
										 val parLeft  = Set.map compareEdge (fn (l', a, r') => (e, a, Parallel (r',r)))
										 val parRight  = Set.map compareEdge (fn (l', a, r') => (e, a, Parallel (l,r')))
										 
										 fun sync (l, a, r) (l', a', r') = matches a a'																	   
										
										 fun findSync e s = Set.filter (fn x =>  sync e x) s						  
																	
										 fun applySync (l, a, r) (l', a', r')= (e, ".", Parallel (r, r'))
																	
										 val union = Set.union (parLeft el) (parRight er)							 
										 
									 in Set.fold (fn (e,s) => Set.union (Set.map compareEdge (applySync e) (findSync e er)) s) union el end
									 
									 
|   edges gamma N (Restrict (e, R)) = Set.map compareEdge (fn (l, a, r) => (Restrict(e, R), a, Restrict(r, R))) (Set.filter (fn (e, a, e') => not (Set.element a R)) (edges gamma N e))
|   edges _ _ _ = raise Error "Invalid expression!"


val (count, reset) = let val c = ref 1
	    in
		(fn () => #1(!c, c := !c +1),
		 fn () => c := 1)
	    end
    

	    
fun convert' (nodes : (exp -> string) ref) (gamma: string -> exp) (e:exp) = (!nodes e; ()) handle Empty => 

							let fun id (e:exp) = !nodes e handle Empty => (let val i = Int.toString(count()) 
														 val _ = print (i ^ " [shape = box, label = \""^ toString e ^"\", style = rounded];\n") 
									                 in nodes := update compareExp (!nodes) e i; i end)
								val _ = id e
							in
								Set.fold (fn ((l, a, r), ()) => (convert' nodes gamma r;print (id l ^ " -> " ^ id r ^"[label = \"" ^ a ^"\"]\n"))) () (edges gamma  (Set.empty String.compare)e)
							end
							

fun convert s = 
let 
	val _ = reset()
	val (gamma, e) = parse (lex (explode s))
in 
	print "\n\n\ndigraph \n{\n";
	print "0 [shape = plaintext, label = \"\"]\n";
	convert' (ref(fn _ => raise Empty)) gamma e;
	print "0 -> 1\n}\n\n\n\n"
end