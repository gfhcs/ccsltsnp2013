	type edge = exp * string * exp
	
structure LTS :> sig
	
	val compareEdge : edge * edge -> order
	val edges : binding -> exp -> edge Set.set 
	
	val draw : binding * exp -> unit
	
	
	
 end
 = struct
 
 fun compareEdge ((l1, a1, r1), (l2, a2, r2)) = case cmpTuple compare ((l1,r1),(l2,r2)) of EQUAL => String.compare(a1, a2) | s => s              

 


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

 val edges' = edges
 fun edges gamma e = edges' gamma (Set.empty String.compare) e
 
 val (count, reset) = let val c = ref 1
	    in
		(fn () => #1(!c, c := !c +1),
		 fn () => c := 1)
	    end
  	    
fun draw' (nodes : (exp -> string) ref) (gamma: string -> exp) (e:exp) = (!nodes e; ()) handle Empty => 

							let fun id (e:exp) = !nodes e handle Empty => (let val i = Int.toString(count()) 
														 val _ = print (i ^ " [shape = box, label = \""^ toString e ^"\", style = rounded];\n") 
									                 in nodes := update compare (!nodes) e i; i end)
								val _ = id e
							in
								Set.fold (fn ((l, a, r), ()) => (draw' nodes gamma r;print (id l ^ " -> " ^ id r ^"[label = \"" ^ a ^"\"]\n"))) () (edges gamma e)
							end
							

fun draw (b, e) = 
let 
	val _ = reset()
in 
	print "\n\n\ndigraph \n{\n";
	print "0 [shape = plaintext, label = \"\"]\n";
	draw' (ref(fn _ => raise Empty)) b e;
	print "0 -> 1\n}\n\n\n\n"
end
 
 
 
 end
  
open LTS