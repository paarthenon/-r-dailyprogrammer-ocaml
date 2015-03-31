let unique list = 
	let uniq l x = if List.mem x l then l else x::l in
	List.rev (List.fold_left uniq [] list) 

let () =
	let input = read_line () in
	let input_list = Str.split (Str.regexp "[ \t\n]+") input in
	let unique_list = unique input_list in
	List.iter (Printf.printf "%s ") unique_list; print_endline ""