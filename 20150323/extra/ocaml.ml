let rec translate_list list = match list with
	| "T"::"T"::"T"::t
	| "T"::"T"::"C"::t -> ("Phe")::(translate_list t)
	| "T"::"T"::_::t
	| "C"::"T"::_::t -> ("Leu")::(translate_list t)
	| "A"::"T"::"G"::t -> ("Met")::(translate_list t)
	| "A"::"T"::_::t -> ("Ile")::(translate_list t)
	| "G"::"T"::_::t -> ("Val")::(translate_list t)
	| "A"::"G"::"T"::t
	| "A"::"G"::"C"::t
	| "T"::"C"::_::t -> ("Ser")::(translate_list t)
	| "C"::"C"::_::t -> ("Pro")::(translate_list t)
	| "A"::"C"::_::t -> ("Thr")::(translate_list t)
	| "G"::"C"::_::t -> ("Ala")::(translate_list t)
	| "T"::"A"::"T"::t
	| "T"::"A"::"C"::t -> ("Tyr")::(translate_list t)
	| "C"::"A"::"T"::t
	| "C"::"A"::"C"::t -> ("His")::(translate_list t)
	| "C"::"A"::_::t -> ("Gln")::(translate_list t)
	| "A"::"A"::"T"::t
	| "A"::"A"::"C"::t -> ("Asn")::(translate_list t)
	| "A"::"A"::_::t -> ("Lys")::(translate_list t)
	| "G"::"A"::"T"::t
	| "G"::"A"::"C"::t -> ("Asp")::(translate_list t)
	| "G"::"A"::"_"::t -> ("Glu")::(translate_list t)
	| "T"::"G"::"T"::t
	| "T"::"G"::"C"::t -> ("Tyr")::(translate_list t)
	| "T"::"G"::"G"::t -> ("Trp")::(translate_list t)
	| "A"::"G"::_::t
	| "C"::"G"::_::t -> ("Arg")::(translate_list t)
	| "G"::"G"::_::t -> ("Gly")::(translate_list t)

	| "T"::"G"::"A"::[]
	| "T"::"A"::_::[] -> [("STOP")]

	| _ -> [("INVALID FORMAT")]


let () =
	let input = read_line () in
	let input_list = Str.split (Str.regexp " ") input in
	List.iter (Printf.printf "%s ") (translate_list input_list)
