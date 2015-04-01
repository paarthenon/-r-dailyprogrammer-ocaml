	(* Type Definitions *)
	type point = Point of (int * int)
	type command =
		| Radial of point * int
		| Linear of point * point
		| None

	(* Helper functions *)
	let distance (Point (x,y)) (Point (x',y')) = 
		sqrt((float_of_int(x - x') ** 2.0) +. (float_of_int(y - y') ** 2.0))

	(* Vector scalar projection. I admit I had to look this up *)
	let scalar_proj (Point (ax,ay)) (Point (bx, by)) =
		(float_of_int (ax * bx + ay * by)) /. (distance (Point (0, 0)) (Point (ax, ay)))

	(* Given a shading value, return the most appropriate character from the gradient string*)
	let value_for_shade gradient shade =
		let len = String.length gradient in
		let index = int_of_float (floor ((shade *. (float_of_int len)))) in
		match index with
		| n when n < 0 -> gradient.[0]
		| n when n >= len -> gradient.[len-1]
		| n -> gradient.[n]

	(* Take in a gradient string, a command, and a particular point to return the shading value for that point *)
	let shade gradient cmd (Point (px, py)) =
		let this_shade = value_for_shade gradient in
		match cmd with
		| Radial (Point (x,y), radius) ->
			let magnitude = distance (Point (x,y)) (Point (px,py)) in
			let scaled_magnitude = magnitude /. (float_of_int radius) in
			this_shade scaled_magnitude
		| Linear (Point (x1, y1), Point (x2, y2)) ->
			let overall_length = distance (Point (x1,y1)) (Point (x2,y2)) in
			let origin_gradient = Point (x2 - x1, y2-y1) in
			let origin_point = Point (px - x1, py - y1) in
			let scaled_value = (scalar_proj origin_gradient origin_point) /. overall_length in
			this_shade scaled_value
		| None -> " ".[0]

	(* Main *)
	let () =
		let dim_str = read_line () in (* Dimensions *)
		let val_str = read_line () in (* Gradient Values *)
		let cmd_str = read_line () in (* Command *)
		let Point (columns, rows) = Scanf.sscanf dim_str "%d %d" (fun x y -> Point (x, y)) in

		let parse_command str =
			let str_list = Str.split (Str.regexp " ") str in
			match str_list with
			| "radial"::xstr::ystr::rstr::[] -> 
				let x = int_of_string xstr in
				let y = int_of_string ystr in
				let r = int_of_string rstr in
				Radial (Point (x,y), r)
			| "linear"::x1str::y1str::x2str::y2str::[] -> 
				let x1 = int_of_string x1str in
				let y1 = int_of_string y1str in
				let x2 = int_of_string x2str in
				let y2 = int_of_string y2str in
				Linear (Point (x1,y1), Point(x2,y2))
			| _ -> None
		in

		let cmd = parse_command cmd_str in
		let applied_shade = shade val_str cmd in (* Partial application is cool y'all *)

		(* Calculate the "shade" for each position and print it*)
		for row = 0 to rows do
			for col = 0 to columns do
				print_char (applied_shade (Point (col,row)))
			done;
			print_newline ();
		done
		

