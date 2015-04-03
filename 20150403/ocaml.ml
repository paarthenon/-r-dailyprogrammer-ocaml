module TM = struct
	(* Type Definitions *)
	type state = string
	type symbol = char
	type direction =
		| Left
		| Right
	type action =
	{
		state: state;
		symbol: symbol;
		move: direction;
	}
	type t =
	{
		tape : symbol list;
		head : int;
		state : state;
		accept : state;
		transition : state -> symbol -> action;
		offset : int;
	}

	let trim =
		let has_extra_back tm =
			(tm.head = (List.length tm.tape) - 2) && ((List.hd (List.rev tm.tape)) = '_') in

		let rec drop_back =  function
		| [] -> []
		| [x] -> []
		| (h::t) -> h::(drop_back t) in

		function
		| tm when tm.head = 1 && ((List.hd tm.tape)= '_') && tm.offset > 0 -> 
			{tm with head = 0; tape = List.tl tm.tape; offset = tm.offset - 1}
		| tm when (has_extra_back tm) -> {tm with head = tm.head - 1; tape = drop_back tm.tape}
		| tm -> tm 

	let apply tm action = 
		let replace list index item = 
			List.mapi (fun i elem -> if i = index then item else elem) list in

		let tm' = 
		{ tm with
			tape = (replace tm.tape tm.head action.symbol);
			state = action.state;
			head = match action.move with Left -> tm.head - 1 | Right -> tm.head + 1;
		} in

		match tm'.head with 
		| (-1) -> 
			{tm' with tape = '_'::tm'.tape; head = 0; offset = tm.offset + 1}
		| n when n = (List.length tm.tape) ->
			{tm' with tape = tm'.tape@['_']}
		| _ ->
			tm'

	let step tm = trim (apply tm (tm.transition tm.state (List.nth tm.tape tm.head)))

	let create tape start final transition_table =
		let transition_of_table table = Hashtbl.find table in

		let explode s =
			let rec exp i l =
				if i < 0 then l else exp (i - 1) (s.[i] :: l) in
			exp (String.length s - 1) [] in

		{ tape = explode tape;
		  head = 0;
		  state = start;
		  accept = final;
		  transition = (fun state symbol -> transition_of_table transition_table (state, symbol));
		  offset = 0 }

	let rec execute tm = 
		if tm.state = tm.accept then tm else execute (step tm)

	let print tm =
		List.iter (fun s -> print_char s) tm.tape;
		print_newline ();
		let loc_string = List.mapi (fun i elem -> if i = (0 + tm.offset) then '|' else if i = tm.head then '^' else ' ') tm.tape in
		List.iter (fun s -> print_char s) loc_string;
		print_newline ()
end

let () =
	let lang_str = read_line () in
	let states = read_line () in
	let start_state = read_line () in
	let accept_state = read_line () in
	let tape = read_line () in

	let num_letters = (String.length lang_str) + 1 in
	let num_states = (List.length (Str.split (Str.regexp " ") states)) in
	let num_transitions = num_letters * (num_states - 1) in

	let transition_table = Hashtbl.create num_transitions in
	let parse_direction = function '<' -> TM.Left | '>' -> TM.Right in
	Printf.printf "Expecting %d rules" num_transitions;
	for i = 1 to (num_transitions) do
		let transition_str = read_line () in
		Scanf.sscanf transition_str "%s %c = %s %c %c" (fun s1 l1 s2 l2 dir -> 
			Hashtbl.add transition_table (s1, l1) ({TM.state = s2; TM.symbol = l2; TM.move = parse_direction dir})
		)
	done;

	let tm = TM.create tape start_state accept_state transition_table in
	let tm' = TM.execute tm in
	TM.print tm';


