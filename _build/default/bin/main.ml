(* let user_input () = *)
(*   let stdin = In_channel.input_line In_channel.stdin in *)
(*   let stdin_val = Option.get stdin in *)
(*   match String.contains stdin_val 'q' with *)
(*   | true -> Printf.sprintf "You have quit the program." *)
(*   | false -> stdin_val *)
(* ;; *)

let user_input () =
  let stdin = In_channel.input_line In_channel.stdin in
  let stdin_val = Option.get stdin in
  match String.contains stdin_val 'q' with
  | true -> Printf.sprintf "You have quit the program."
  | false -> stdin_val
;;

let () = Printf.printf "%s\n" (user_input ())
