(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter(fn s => Char.isUpper (String.sub (s, 0)))

val longest_string1 =
	List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc)
		""

val longest_string2 =
	List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc)
		""

fun longest_string_helper f strs =
	List.foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc)
		"" 
		strs

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) =>  a >= b)
		
val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode 

fun first_answer f xs =	
	case xs of
		[] => raise NoAnswer
		| x::xs' => case f x of
			SOME v => v
			| _ => first_answer f xs'

fun all_answers f xs =
	let
	  fun aux (items, acc) =
		case items of
			[] => SOME acc
			| x::items' => case f x of
				SOME lst => aux(items', acc @ lst) 
				| _ => NONE
	in
		aux(xs, [])
	end

	
val count_wildcards = g (fn _ => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (s, p) = 
	g (fn _ => 0) (fn x =>  if s = x then 1 else 0) p

fun check_pat p1 = 
	let 
		fun g p =
			case p of
				  Variable x        => [x]
				| TupleP ps         => List.foldl (fn (p,l) => (g p) @ l) [] ps
				| ConstructorP(_,p) => g p
				| _                 => []

		fun exist xs =
			case xs of
				[] => true
				| x::xs' => if List.exists (fn i => i = x) xs' then false else exist xs'
	in
		exist (g p1)
	end

fun match (v, p) = 
	case (v, p) of
		(_, Wildcard) => SOME []
		| (v1, Variable s) => SOME [(s, v1)]
		| (Unit, UnitP) => SOME []
		| (Const n, ConstP i) => if n = i then SOME [] else NONE
		| (Tuple vs, TupleP ps) => if List.length(ps) = List.length(vs)
								then all_answers match (ListPair.zip(vs, ps))
								else NONE
		| (Constructor(s2, v2), ConstructorP(s1, p1)) => if s1 = s2 
														then match(v2, p1) 
														else NONE
		| _ => NONE

fun first_match v ps =
	SOME (first_answer (fn (p) => match(v, p)) ps) handle NoAnswer => NONE