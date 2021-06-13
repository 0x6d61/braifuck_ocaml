open Printf

module Brainfuck = struct
  let string_to_char_list str = let rec func c l = 
                                  if c < 0 then 
                                    l 
                                  else
                                    func (c - 1) (str.[c] :: l) in func (String.length str - 1) []
  let rec take list n =
    if n = 0 || list = [] then
      []
    else List.hd list :: take (List.tl list) (n-1)

  let search_braces_left list = let rec func lst c = 
                                  if c = List.length lst || List.length lst = 0 then
                                    []
                                  else
                                  if List.nth lst c = '[' then
                                    c :: func lst (c+1)
                                  else
                                    func lst (c+1)
    in func list 0

  let search_braces_right list = let rec func lst c = 
                                   if c = List.length lst || List.length lst = 0 then
                                     []
                                   else
                                   if List.nth lst c = ']' then
                                     c :: func lst (c+1)
                                   else
                                     func lst (c+1)
    in func list 0
  let braces_pair list =List.combine (search_braces_left list) (List.rev (search_braces_right list))
  let create_tape = let rec func n c lst = if n = c then
                        lst
                      else 
                        func n (c+1) (0 :: lst)
    in func 10000 0 []
  let print_list list = List.iter (printf "%d ") list
  let rec assoc_value value list = match list with
      [] -> raise Not_found
    | (k , v) :: rest -> if value = v then
        k
      else assoc_value value rest 

  let add_tape tape ptr v = 
    take tape ptr @ [v] @ List.rev (take (List.rev tape) ((List.length tape)-1-ptr))
  let inc_tape tape ptr c = 
    if c = '+' then
      add_tape tape ptr ((List.nth tape ptr)+1)
    else
      add_tape tape ptr ((List.nth tape ptr)-1)

  let run list = let rec func list ptr tape c =
                   (
                     print_string "[";                       
                     print_int c;
                     print_string ",";
                     print_char (List.nth list c);
                     print_string "]";
                     print_int ptr;
                     print_string "[";
                     print_list tape;
                     print_string "]";
                     print_newline ()
                   );
                   let braces = braces_pair list in
                   if c = List.length list then
                     tape
                   else
                     let now_order = (List.nth list c) in
                     if '>' = now_order then
                       func list (ptr+1) tape (c+1)
                     else if '<' = now_order then
                       func list (ptr-1) tape (c+1)
                     else if '+' = now_order then
                       func list ptr (inc_tape tape ptr '+') (c+1)
                     else if '-' = now_order then
                       func list ptr (inc_tape tape ptr '-') (c+1)
                     else if '.' = now_order then
                       (print_char (Char.chr (List.nth tape ptr));func list ptr tape (c+1))
                     else if ',' = now_order then
                       func list ptr (add_tape tape ptr (Char.code (read_line()).[0])) (c+1)
                     else if '[' = now_order then
                       if (List.nth tape ptr) = 0 then
                         func list ptr tape ((List.assoc c braces)+1)
                       else
                         func list ptr tape (c+1)
                     else if ']' = now_order then
                       if (List.nth tape ptr) <> 0 then
                         func list ptr tape ((assoc_value c braces)+1)
                       else
                         func list ptr tape (c+1)
                     else
                       func list ptr tape (c+1)
    in func list 0 (create_tape) 0
end