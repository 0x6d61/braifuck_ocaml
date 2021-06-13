open Brainfuck

let _ = let code = (read_line ()) in Brainfuck.run (Brainfuck.string_to_char_list code)