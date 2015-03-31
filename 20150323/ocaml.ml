let translate_list list = 
	let translate = function
		| "A" -> "T"
		| "C" -> "G"
		| "T" -> "A"
		| "G" -> "C"
		| _ -> ""
	in
	List.map translate list

let () =
	let input = read_line () in
	let input_list = Str.split (Str.regexp "[ \t\n]+") input in
	List.iter (Printf.printf "%s ") (translate_list input_list)
