(*
Victor Sun
V00894734
*)

structure Patterns =

struct

exception NoAnswer
exception NotFound

datatype tree = emptyTree |
                nodeTree of int * tree * tree


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype value = Const of int
	       | Unit
	       | Tuple of value list
	       | Constructor of string * value

(* write your tree functions here *)
fun tree_insert_in_order (t, v) =
  case t of
  emptyTree => nodeTree(v, emptyTree, emptyTree)
  | nodeTree(n1, l1, r1) => if v <= n1
                            then nodeTree(n1, tree_insert_in_order(l1, v), r1)
                            else nodeTree(n1, l1, tree_insert_in_order(r1, v))

fun tree_height t =
  case t of
  emptyTree => 0
  | nodeTree(n1, l1, r1) => let
                              val left = tree_height(l1) + 1
                              val right = tree_height(r1) + 1
                            in
                              if left < right
                              then right
                              else left
                            end

fun tree_fold_pre_order f acc t =
  case t of
  emptyTree => acc
  | nodeTree(n1, l1, r1) => tree_fold_pre_order f (tree_fold_pre_order f (f(n1, acc)) l1 ) r1

val tree_max = fn t => case t of
                       emptyTree => NONE
                       | nodeTree(n1, l1, r1) => SOME (tree_fold_pre_order (fn(v, acc) => if v < acc
                                                                                    then acc
                                                                                    else v ) (valOf Int.minInt) t)
val tree_to_list = fn t => case t of
                           emptyTree => []
                           | nodeTree(n1, l1, r1) => tree_fold_pre_order(fn(v, acc) => if null acc
                                                                                       then v::acc
                                                                                       else acc@[v]) [] t
fun tree_delete(t, v)=
  case t of
  emptyTree => raise NotFound
  | nodeTree(n1, l1, r1) => if n1 < v
                            then nodeTree(n1, l1, tree_delete(r1, v))
                            else
                              if n1 > v
                              then nodeTree(n1, tree_delete(l1, v), r1)
                              else
                                case (l1, r1) of
                                (emptyTree, emptyTree) => emptyTree
                                |(l1, emptyTree) => l1
                                |(emptyTree, r1) => r1
                                |(l1, r1) => nodeTree(valOf (tree_max(l1)), tree_delete(l1, valOf(tree_max(l1))), r1)

fun tree_filter f t =
  case t of
  emptyTree => emptyTree
  | nodeTree(n1, l1, r1) => if f n1
                            then nodeTree (n1, tree_filter f l1, tree_filter f r1)
                            else tree_filter f (tree_delete(t, n1))

val tree_sum_even = tree_fold_pre_order (op+) 0 o tree_filter (fn f => (f mod 2) = 0) 

fun first_answer f lst = 
  case lst of
  [] => raise NoAnswer
  | head::tail => if isSome(f(head))
                  then valOf (f(head))
                  else first_answer f tail

fun all_answers f lst = 
  case lst of
  [] => SOME []
  | head::tail => if isSome(f(head))
                  then
                    let
                      val tl_ans = all_answers f tail
                    in
                     if isSome(tl_ans)
                     then SOME (valOf (f(head))@(valOf tl_ans))
                     else NONE
                    end
                  else NONE

fun check_pattern p =
  let
    fun functionHelper f acc list=
      case list of
      [] => acc
      | head::tail => case head of
                      Wildcard => functionHelper f acc tail
                      | Variable s => functionHelper f (foldl f acc [s]) tail
                      | UnitP => functionHelper f acc tail
                      | ConstP i => functionHelper f acc tail
                      | TupleP ps => functionHelper f (functionHelper f acc ps) tail 
                      | ConstructorP (s1, p) => functionHelper f (functionHelper f (foldl f acc [s1]) [p]) tail           
    
    fun stringHelper(list: string list)=
      case list of
      [] => true
      | head::tail => if List.exists (fn y => y = head) tail
                      then false
                      else stringHelper(tail)

  in
    case p of
    Wildcard => true
    | Variable s => true
    | UnitP => true
    | ConstP i => true  
    | TupleP ps => stringHelper (functionHelper (fn (v, acc) => acc@[v]) [] ps)
    | ConstructorP (s1, p) => stringHelper (functionHelper (fn (v, acc) => acc@[v]) [s1] [p])
  end

fun match (v, p) =
  case (v, p) of
  (_, Wildcard) => SOME []
  | (v, Variable s ) => SOME [(s, v)]
  | (Unit, UnitP) => SOME []
  | (Const i, ConstP p) => if i = p then SOME [] else NONE
  | (Tuple vs, TupleP ps) => if List.length(vs) <> List.length(ps) then NONE else all_answers match (ListPair.zip(vs, ps))
  | (Constructor(s2, v), ConstructorP(s1, p)) => if s2 = s1 then match(v, p) else NONE
  | (_, _) => NONE

fun first_match v lstP = 
  SOME (first_answer(fn x => match(v, x)) lstP)
  handle NoAnswer => NONE

(* leave the following functions untouched *)

fun tree_root t =
    case t of
        emptyTree => NONE
      | nodeTree (n, _, _) => SOME n

fun tree_equal t1 t2  =
    case (t1, t2) of
        (emptyTree, emptyTree) => true
      | (emptyTree, _) =>  false
      | (_, emptyTree) => false
      | (nodeTree(n1, l1, r1),nodeTree(n2, l2, r2)) =>
        n1 = n2 andalso (tree_equal l1 l2) andalso (tree_equal r1 r2)

infix 9 ++
infix 9 --
infix 7 == 

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

end