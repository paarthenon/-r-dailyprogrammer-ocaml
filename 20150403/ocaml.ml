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
			transition : state -> symbol -> action; (* The transition function is an actual function *)
			offset : int; (* This is necessary so that we can keep 
			track of the relative position of the beginning pipe*)
		}

		(* Trim a turing machine so that _ values on either end dont stick around if theyre not needed *)
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

		(* Apply an action to a turing machine *)
		let apply tm action = 
			let replace list index item = 
				List.mapi (fun i elem -> if i = index then item else elem) list in

			(* stuff that always happens *)
			let tm' = 
			{ tm with
				tape = (replace tm.tape tm.head action.symbol);
				state = action.state;
				head = match action.move with Left -> tm.head - 1 | Right -> tm.head + 1;
			} in

			(* Extend the end if necessary *)
			match tm'.head with 
			| (-1) -> 
				{tm' with tape = '_'::tm'.tape; head = 0; offset = tm.offset + 1}
			| n when n = (List.length tm.tape) ->
				{tm' with tape = tm'.tape@['_']}
			| _ ->
				tm'

		(* Apply an action and trim the result *)
		let step tm = trim (apply tm (tm.transition tm.state (List.nth tm.tape tm.head)))

		(* Build a new TM *)
		let create tape start final transition_func =
			let explode s =
				let rec exp i l =
					if i < 0 then l else exp (i - 1) (s.[i] :: l) in
				exp (String.length s - 1) [] in

			{ tape = explode tape;
			  head = 0;
			  state = start;
			  accept = final;
			  transition = (fun state symbol -> transition_func (state, symbol));
			  offset = 0 }

		(* Execute: Step until the end state is reached *)
		let rec execute tm = 
			if tm.state = tm.accept then tm else execute (step tm)

		(* Print the TM output *)
		let print tm =
			List.iter (fun s -> print_char s) tm.tape;
			print_newline ();
			let loc_string = List.mapi 
				(fun i elem -> if i = (0 + tm.offset) then '|' else if i = tm.head then '^' else ' ') tm.tape in
			List.iter (fun s -> print_char s) loc_string;
			print_newline ()
	end

	let () =
		(* Grab raw inputs *)
		let _ = read_line () in (* I actually dont care about the alphabet *)
		let _ = read_line () in (* Nor do I care about what the states are *)
		let start_state = read_line () in
		let accept_state = read_line () in
		let tape = read_line () in

		(* This is my first attempt at generating a chained function. Is this useful? Probably not, but it's kind of cool.*)
		let build_transition_function ()= 
			let parse_direction = function '<' -> TM.Left | '>' -> TM.Right | _ -> raise (Failure "Invalid direction") in
			let rec build_func f =
				try
					Scanf.sscanf (read_line ()) "%s %c = %s %c %c" (fun s1 l1 s2 l2 dir ->
					build_func
					(Some (function
					| (state, symbol) when state = s1 && symbol = l1 -> {TM.state = s2; TM.symbol = l2; TM.move = parse_direction dir} 
					| (state, symbol) -> match f with Some f' -> f' (state, symbol) | None -> raise (Failure "Not a valid transition"))))
				with
					End_of_file -> f in
			let unpack_function = function
			| Some x -> x
			| None -> raise (Failure "Unable to create transition function") in
			build_func None |> unpack_function in

		let tm = TM.create tape start_state accept_state (build_transition_function()) in
		let tm' = TM.execute tm in
		TM.print tm'


