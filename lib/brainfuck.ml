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
  

  let make_tape = let rec func n c lst =
    if n = c then
      lst
  else 
    func n (c+1) (0 :: lst)
  in func 10000 0 []
  let inc_tape tape ptr c = 
    if c = '+' then
      take tape ptr @ [List.nth tape ptr +1] @ take (List.rev tape) ((List.length tape-1)-ptr)
    else
      take tape ptr @ [List.nth tape ptr -1] @ take (List.rev tape) ((List.length tape-1)-ptr)
  
  let add_tape tape ptr ascii_code = 
    take tape (List.length tape -1 ) @ [ascii_code] @ take (List.rev tape) ((List.length tape-1)-ptr)

  let run list = let rec func list ptr tape = 
    match list with
    [] -> tape
    | first :: rest ->
      if '>' = first then
         func rest (ptr+1) tape
      else if '<' = first then
        func rest (ptr-1) tape
      else if '+' = first then
        func rest ptr (inc_tape tape ptr '+')
      else if '-' = first then
        func rest ptr (inc_tape tape ptr '-')
      else if '.' = first then
        (
          print_char (Char.chr (List.nth tape ptr));
          func rest ptr tape
        )
      else if ',' = first then
        func rest ptr (add_tape tape ptr (Char.code (read_line()).[0]))
      else
        func rest ptr tape
      in func list 0 (make_tape)
end