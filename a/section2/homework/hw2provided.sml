(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except(s, lst) =
   let 
      fun aux(sl, acc) =
         case sl of
            [] => if acc = lst then [] else acc
            | x::sl' => if same_string(s, x) then aux([], acc @ sl') else aux(sl', acc @ [x])
   in
      aux(lst, [])
   end

fun all_except_option (s, lst) =
   case lst of
      [] => NONE
      | _ => SOME (all_except(s, lst))


fun get_substitutions1 (str_list_list, s) =
   case str_list_list of
      [] => []
      | sl::sll' => all_except(s, sl) @ get_substitutions1(sll', s)
      
fun get_substitutions2 (str_list_list, s) =
   let fun aux(sll, acc) =
      case sll of
         [] => acc
         | sl::sll' => aux(sll', acc @ all_except(s, sl))
   in
      aux(str_list_list, [])
   end

fun similar_names (str_list_list, full_name) =
   let 
      val {first=x, middle=y, last=z} = full_name
      val subs = get_substitutions2(str_list_list, x)
      fun aux(sl, acc) =
         case sl of
            [] => acc
            | s::sl' => aux(sl', acc @ [{first=s, middle=y, last=z}])
   in
      aux(x::subs, [])
   end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, rank) =
   case suit of
      Spades => Black
      | Clubs => Black
      | _ => Red

fun card_value (suit, rank) =
   case rank of
      Num i => i
      | Ace => 11
      | _ => 10

fun all_except_card(cs,c) =
   case cs of
      [] => []
      | x::cs' => if x = c
         then cs'
         else x::all_except_card(cs', c)

fun remove_card (cs, c, e) =
   let
     val cards = all_except_card(cs, c)
   in
      case cards of
         [] => if cs <> cards then [] else raise e
         | _ => if cs <> cards then cards else raise e
   end

fun all_same_color (cs) =
   case cs of
   [] => true
   | a::[] => true
   | a::b::cs' => if card_color(a) = card_color(b)
                  then all_same_color(b::cs')
                  else false

fun sum_cards (cs) =
   let fun aux(cl, acc) =
      case cl of
         [] => acc
         | a::cl' => aux(cl', card_value(a) + acc)

   in
      aux(cs, 0)
   end

fun score (held_cards, goal) =
   let
     val sum = sum_cards(held_cards)
     val pre_score = if sum > goal then (sum - goal) * 3 else goal - sum
   in
      if all_same_color(held_cards) then pre_score div 2 else pre_score
   end

fun officiate (card_list, move_list, goal) =
   let
     fun trick(cs, hs, ms) =
      case ms of
         [] => hs
         | m::ms' => case m of
            Discard c => trick(cs, remove_card(hs, c, IllegalMove), ms')
            | Draw => case cs of
               [] => hs
               | card::cs' => if sum_cards(card::hs) > goal
                  then card::hs
                  else trick(cs', card::hs, ms')
   in
      score(trick(card_list, [], move_list), goal)
   end