(*
Victor Sun
V00894734
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

fun is_empty_set s =
    case s of
    EmptySet(_) => true
    | Set(_ , _) => false

fun min_in_set s =
    case s of 
    EmptySet(_) => raise SetIsEmpty  
    | Set(list, compare) => let
                                 fun minHelper(list)=
                                   hd list
                            in
                              minHelper(list)
                            end 

fun max_in_set s =
    case s of 
    EmptySet(_) => raise SetIsEmpty  
    | Set(list, compare) => let
                                 fun maxHelper(list)=
                                   if null(tl list)
                                   then hd list
                                   else maxHelper(tl list)
                            in
                              maxHelper(list)
                            end

fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
  case (a,b) of
  ([],[]) => EQUAL
  | ([], head::tail) => LESS
  | (head::tail, []) => GREATER
  | (a_head::a_tail, b_head::b_tail) => if fcomp(a_head, b_head) = EQUAL
                                        then comp_list_any(a_tail, b_tail,fcomp)
                                        else fcomp(a_head, b_head)

fun list_to_set(lst, f) =
    if null lst
    then EmptySet f
    else Set(lst, f)

fun in_set(s, v) =
    case s of
    EmptySet(_) => false
    | Set(list, compare) => let
                              fun insetHelper(v, list, compare)=
                                let
                                  val x = hd list
                                  val y = v
                                in
                                  if null(tl list)
                                  then 
                                    if compare(x,y) = EQUAL
                                    then true
                                    else false
                                  else 
                                    if compare(x,y) = EQUAL
                                    then true
                                    else insetHelper(v, tl list, compare)
                                end
                            in
                              insetHelper(v, list, compare)
                            end

fun insert_into_set(s,v) =
   case s of
   EmptySet(compare) => Set([v], compare)
   | Set(list,compare) => let 
                            fun insertHelper(v, list, compare)=
                              if in_set(s, v)
                              then list
                              else
                                let
                                  val result = case comp_list_any([hd list], [v], compare) of
                                               GREATER => v
                                               | LESS => hd list
                                               | EQUAL => hd list
                                in
                                  if null(tl list)
                                  then
                                    if comp_list_any([result], [hd list], compare) = EQUAL
                                    then list@[v]
                                    else v::list
                                  else
                                    if comp_list_any([result], [hd list], compare) = EQUAL 
                                    then result::insertHelper(v, tl list, compare)
                                    else v::list
                                end           
                            in
                              list_to_set(insertHelper(v, list, compare), compare)
                            end

fun remove_from_set(s,v) =
    case s of 
    EmptySet(compare) => EmptySet(compare)
    | Set(list, compare) => let
                              fun removeHelper(v, list, compare)=
                                case list of
                                [] => []
                                | head::tail => if compare(head, v) = EQUAL
                                                then removeHelper(v, tail, compare)
                                                else head::removeHelper(v, tail, compare)
                              val list = removeHelper(v, list, compare)
                            in  
                              list_to_set(list, compare)
                            end

fun union_set(s, t) =
  case (s,t) of
  (EmptySet(compare), EmptySet(_)) => EmptySet(compare)
  | (EmptySet(_), Set(t_list, compare2)) => Set(t_list, compare2)
  | (Set(s_list, compare), EmptySet(_)) => Set(s_list, compare)
  | (Set(s_list, compare), Set(t_list, compare2)) => let
                                                       val acc = s
                                                       fun unionHelper(acc, t_list, compare)=
                                                         if null(tl t_list)
                                                         then
                                                           if in_set(acc, hd t_list)
                                                           then acc
                                                           else insert_into_set(acc, hd t_list)
                                                         else
                                                           if in_set(acc, hd t_list)
                                                           then unionHelper(acc, tl t_list, compare)
                                                           else unionHelper(insert_into_set(acc, hd t_list), tl t_list, compare)
                                                     in
                                                       unionHelper(acc, t_list, compare)
                                                     end


fun intersect_set(s, t) =
  case (s,t) of
  (EmptySet(compare), EmptySet(_)) => EmptySet(compare)
  | (EmptySet(_), Set(t_list, compare2)) => EmptySet(compare2)
  | (Set(s_list, compare), EmptySet(_)) => EmptySet(compare)
  | (Set(s_list, compare), Set(t_list, compare2)) => let
                                                       val acc = EmptySet(compare)
                                                       fun intersectHelper(acc, s, t_list, compare)=
                                                         if null(tl t_list)
                                                         then
                                                           if in_set(s, hd t_list)
                                                           then insert_into_set(acc, hd t_list)
                                                           else acc
                                                         else
                                                           if in_set(s, hd t_list)
                                                           then intersectHelper(insert_into_set(acc, hd t_list),s, tl t_list, compare)
                                                           else intersectHelper(acc, s, tl t_list, compare)
                                                     in
                                                       intersectHelper(acc, s, t_list, compare)
                                                     end

fun except_set(s, t) =
  case (s,t) of
  (EmptySet(compare), EmptySet(_)) => EmptySet(compare)
  | (EmptySet(_), Set(t_list, compare2)) => EmptySet(compare2)
  | (Set(s_list, compare), EmptySet(_)) => Set(s_list, compare)
  | (Set(s_list, compare), Set(t_list, compare2)) => let
                                                       val acc = s
                                                       fun exceptHelper(acc, t_list, compare)=
                                                         if null(tl t_list)
                                                         then
                                                           if in_set(acc, hd t_list)
                                                           then remove_from_set(acc, hd t_list)
                                                           else acc
                                                         else
                                                           if in_set(acc, hd t_list)
                                                           then exceptHelper(remove_from_set(acc, hd t_list), tl t_list, compare)
                                                           else exceptHelper(acc, tl t_list, compare)
                                                     in
                                                       exceptHelper(acc, t_list, compare)
                                                     end
    
fun size_set(s: 'a set) =
  case s of 
  EmptySet(_) => 0
  | Set(list, _) => let 
                      fun sizeHelper(list, acc)=
                        if null (tl list)
                        then acc + 1
                        else sizeHelper(tl list, acc + 1)
                    in
                      sizeHelper(list, 0)
                    end

fun equal_set(s, t) =
  case (s,t) of
  (EmptySet(_), EmptySet(_)) => true
  | (EmptySet(_), Set(t_list, compare2)) => false
  | (Set(s_list, compare), EmptySet(_)) => false
  | (Set(s_list, compare), Set(t_list, compare2)) => let
                                                       fun equalHelper(s_list, t_list, compare)=
                                                           if null(tl s_list)
                                                           then
                                                             if null(tl t_list)
                                                             then
                                                               if compare(hd s_list, hd t_list) = EQUAL
                                                               then true
                                                               else false
                                                             else false
                                                           else
                                                             if compare(hd s_list, hd t_list) = EQUAL
                                                             then equalHelper(tl s_list, tl t_list, compare)
                                                             else false
                                                      in
                                                        equalHelper(s_list, t_list, compare)
                                                      end

fun is_subset_of(s, t) =
    case (s, t) of
    (EmptySet(_), EmptySet(_)) => true
    | (EmptySet(_), Set(_,_)) => true
    | (Set(_,_), EmptySet(_)) => false
    | (Set(s_list, compare), Set(t_list, compare2)) => let
                                                         fun subsetHelper(s_list, t_list)=
                                                           if null(tl s_list)
                                                           then 
                                                             if in_set(t, hd s_list)
                                                             then true
                                                             else false
                                                           else 
                                                             if in_set(t, hd s_list)
                                                             then subsetHelper(tl s_list, t_list)
                                                             else false
                                                       in
                                                         subsetHelper(s_list, t_list)
                                                       end 

fun set_to_list s =
    case s of 
    EmptySet(_) => []
    | Set(list,_) => list

fun str_set (s, fstr) =
    case s of
    EmptySet(_) => "{}"
    | Set(list, compare) => let
                             fun strHelper(list, fstr)=
                               if null(tl list)
                               then fstr(hd list)
                               else fstr(hd list) ^ ":" ^ strHelper(tl list, fstr)
                           in
                             "{" ^ strHelper(list, fstr) ^ "}"
                           end 
      
fun map_set (s, fcomp, f) =
    case (s, fcomp, f) of
    (EmptySet(_), fcomp, _) => EmptySet fcomp
    | (Set(list, compare), fcomp, f) => let
                              val acc = EmptySet(fcomp)
                              fun mapSetHelper(acc, list, f)=
                                if null(list)
                                then acc
                                else mapSetHelper(insert_into_set(acc, f(hd list)), tl list, f)
                            in
                              mapSetHelper(acc, list, f)
                            end

fun s -- v = remove_from_set(s, v)
fun s ++ v = insert_into_set(s, v)
fun s IDENTICAL t = equal_set(s, t)
fun s UNION t = union_set(s, t)
fun s INTERSECT t = intersect_set(s, t)
fun s EXCEPT t = except_set(s, t)
fun v IN s = in_set(s,v)
fun s IS_SUBSET_OF t = is_subset_of(s, t)
fun s CONTAINS v = in_set(s,v)

end
end    
