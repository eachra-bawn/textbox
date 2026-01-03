let original_termios = Unix.tcgetattr Unix.stdin

let is_ctrl c =
  let char_ascii = int_of_char c in
  if char_ascii <= 31 || char_ascii = 127 then true else false
;;

let user_input () =
  let stdin = In_channel.stdin in
  let read_char () =
    let byte_seq = Bytes.make 1 ' ' in
    let input = In_channel.input stdin byte_seq 0 1 in
    if input = 0 then None else Some (Bytes.get byte_seq 0)
  in
  let rec input () =
    match read_char () with
    | None | Some 'q' -> ()
    | Some c ->
      if is_ctrl c
      then Printf.printf "%d\n" (Char.code c)
      else Printf.printf "%d ( '%c' )\n" (Char.code c) c;
      Out_channel.flush Out_channel.stdout;
      input ()
  in
  input ()
;;

let enable_raw_mode () =
  let open Unix in
  let attr = tcgetattr Unix.stdin in
  attr.c_echo <- false;
  attr.c_icanon <- false;
  attr.c_isig <- false;
  attr.c_ixon <- false;
  tcsetattr stdin TCSAFLUSH attr
;;

let disable_raw_mode () =
  let open Unix in
  tcsetattr stdin TCSAFLUSH original_termios
;;

let () =
  enable_raw_mode ();
  user_input ();
  disable_raw_mode ()
;;
