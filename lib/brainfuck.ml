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

  let braces_right list index bc = let rec func i c = 
                                     if index = List.length list || List.length list = 0 then
                                       raise Not_found
                                     else
                                     if List.nth list i = ']' && bc = c then
                                       i
                                     else if List.nth list i = '[' then
                                       func (i+1) (c+1)
                                     else if List.nth list i = ']' then
                                       func (i+1) (c-1)
                                     else
                                       func (i+1) c 
    in func index bc

  let braces_pair list  = let rec func index =
                            if index = List.length list || List.length list = 0 then
                              []
                            else
                            if List.nth list index = '[' then
                              (index,(braces_right list (index+1) 1)) :: ((braces_right list (index+1) 1),index) :: func (index+1)
                            else
                              func (index+1)
    in func 0
  let create_tape = let rec func n c lst = if n = c then
                        lst
                      else 
                        func n (c+1) (0 :: lst)
    in func 10000 0 []
  let print_list list = List.iter (printf "%d ") list
  let add_tape tape ptr v = 
    take tape ptr @ [v] @ List.rev (take (List.rev tape) ((List.length tape)-1-ptr))
  let inc_tape tape ptr c = 
    if c = '+' then
      add_tape tape ptr ((List.nth tape ptr)+1)
    else
      add_tape tape ptr ((List.nth tape ptr)-1)

  let run list = let rec func ptr tape c =
                   let braces = braces_pair list in
                   if c = List.length list then
                     tape
                   else
                     let now_order = (List.nth list c) in
                     if '>' = now_order then
                       func (ptr+1) tape (c+1)
                     else if '<' = now_order then
                       func (ptr-1) tape (c+1)
                     else if '+' = now_order then
                       func ptr (inc_tape tape ptr '+') (c+1)
                     else if '-' = now_order then
                       func ptr (inc_tape tape ptr '-') (c+1)
                     else if '.' = now_order then
                       (print_char (Char.chr (List.nth tape ptr));
                       func ptr tape (c+1))
                     else if ',' = now_order then
                       func ptr (add_tape tape ptr (Char.code (read_line()).[0])) (c+1)
                     else if '[' = now_order then
                       if (List.nth tape ptr) = 0 then
                         func ptr tape ((List.assoc c braces)+1)
                       else
                         func ptr tape (c+1)
                     else if ']' = now_order then
                       if (List.nth tape ptr) <> 0 then
                         func ptr tape ((List.assoc c braces)+1)
                       else
                         func ptr tape (c+1)
                     else
                       func ptr tape (c+1)
    in func 0 (create_tape) 0
end