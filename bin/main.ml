let original_termios = Unix.tcgetattr Unix.stdin
let textboxx_version = "0.0.1"

type editor_config =
  { termios : Unix.terminal_io
  ; mutable cx : int
  ; mutable cy : int
  ; mutable screen_rows : int
  ; mutable screen_cols : int
  }

(* type editor_keys =
  | Arrow_left
  | Arrow_right
  | Arrow_up
  | Arrow_down *)

(* let editor_keys_map = function
  | Arrow_left -> 1000
  | Arrow_right -> 1001
  | Arrow_up -> 1002
  | Arrow_down -> 1003
;;  *)

let () = Out_channel.set_buffered Out_channel.stdout true

let editor_config =
  { termios = original_termios; cx = 1; cy = 1; screen_rows = 0; screen_cols = 0 }
;;

let getWindowSize () =
  let stty_cmd = Unix.open_process_args_in "stty" [| "stty"; "size" |] in
  let get_stty_cmd = In_channel.input_all stty_cmd in
  let stty_cmd_split = String.split_on_char ' ' get_stty_cmd in
  let rows = int_of_string (String.trim (List.nth stty_cmd_split 0)) in
  let cols = int_of_string (String.trim (List.nth stty_cmd_split 1)) in
  editor_config.screen_rows <- rows;
  editor_config.screen_cols <- cols
;;

(* let is_ctrl c = *)
(*   let char_ascii = int_of_char c in *)
(*   if char_ascii <= 31 || char_ascii = 127 then true else false *)
(* ;; *)

let ctrl_key c = int_of_char c land 0x1f

let editor_read_key () =
  let stdin = In_channel.stdin in
  let byte_seq = Bytes.create 1 in
  let input = In_channel.input stdin byte_seq 0 1 in
  if input = 0
  then None
  else (
    match Some (Bytes.get byte_seq 0) with
    | Some c ->
      if c = '\x1b'
      then (
        let seq = Bytes.create 3 in
        if In_channel.input stdin seq 0 1 != 1 || In_channel.input stdin seq 1 1 != 1
        then Some '\x1b'
        else (
          match Bytes.get seq 0 = '[' with
          | true ->
            (match Bytes.get seq 1 with
             | 'A' -> Some 'w'
             | 'B' -> Some 's'
             | 'C' -> Some 'd'
             | 'D' -> Some 'a'
             | _ -> Some '\x1b')
          | false -> None))
      else Some c
    | None -> None)
;;

let editor_draw_rows () =
  let rec draw y =
    let open Out_channel in
    let max = editor_config.screen_rows in
    if y = max
    then ()
    else if y = editor_config.screen_rows / 3
    then (
      let welcome_str = Printf.sprintf "Textboxx <version %s>" textboxx_version in
      let welcome_str_match =
        match String.length welcome_str > editor_config.screen_cols with
        | false -> welcome_str
        | true -> String.sub welcome_str 1 editor_config.screen_cols
      in
      let padding =
        ref ((editor_config.screen_cols - String.length welcome_str_match) / 2)
      in
      if !padding != 0
      then (
        output_string stdout "~";
        output_string stdout "\x1b[K";
        padding := !padding - 1)
      else ();
      while !padding != 0 do
        output_string stdout " ";
        padding := !padding - 1
      done;
      output_string stdout welcome_str_match;
      output_string stdout "\x1b[K";
      output_string stdout "\r\n";
      draw (y + 1))
    else if y = editor_config.screen_rows - 1
    then (
      output_string stdout "~";
      output_string stdout "\x1b[K")
    else (
      output_string stdout "\x1b[K";
      output_string stdout "~\r\n";
      draw (y + 1))
  in
  draw 0
;;

let editor_update_cursor () =
  let cursor_pos = Printf.sprintf "\x1b[%d;%dH" editor_config.cy editor_config.cx in
  Out_channel.output_string Out_channel.stdout cursor_pos;
  Out_channel.flush Out_channel.stdout
;;

let editor_refresh_screen () =
  let open Out_channel in
  output_string stdout "\x1b[H";
  editor_draw_rows ();
  editor_update_cursor ();
  output_string stdout "\x1b[H";
  output_string stdout "\x1b[?25h";
  flush stdout
;;

let editor_move_cursor = function
  | 'w' ->
    if editor_config.cy != 0
    then editor_config.cy <- editor_config.cy - 1
    else editor_config.cy <- 1
  | 's' ->
    if editor_config.cy != editor_config.screen_rows
    then editor_config.cy <- editor_config.cy + 1
    else ()
  | 'd' ->
    if editor_config.cx != editor_config.screen_cols
    then editor_config.cx <- editor_config.cx + 1
    else ()
  | 'a' ->
    if editor_config.cx != 0
    then editor_config.cx <- editor_config.cx - 1
    else editor_config.cx <- 1
  | _ -> ()
;;

let editor_process_keypresses () =
  let rec input () =
    match editor_read_key () with
    | None ->
      Out_channel.flush Out_channel.stdout;
      input ()
    | Some c ->
      Out_channel.flush Out_channel.stdout;
      if int_of_char c = ctrl_key 'q'
      then editor_refresh_screen ()
      else (
        match c with
        | 'w' ->
          editor_move_cursor 'w';
          editor_update_cursor ();
          input ()
        | 's' ->
          editor_move_cursor 's';
          editor_update_cursor ();
          input ()
        | 'a' ->
          editor_move_cursor 'a';
          editor_update_cursor ();
          input ()
        | 'd' ->
          editor_move_cursor 'd';
          editor_update_cursor ();
          input ()
        | _ -> input ())
  in
  input ()
;;

let enable_raw_mode () =
  let open Unix in
  let attr = tcgetattr Unix.stdin in
  (* missing `IEXTEN` flag, it seems to be missing in standard library. This effects C-v. *)
  attr.c_brkint <- false;
  attr.c_icrnl <- false;
  attr.c_inpck <- false;
  attr.c_istrip <- false;
  attr.c_ixon <- false;
  attr.c_icrnl <- false;
  attr.c_opost <- false;
  attr.c_csize <- 8;
  attr.c_echo <- false;
  attr.c_icanon <- false;
  attr.c_isig <- false;
  attr.c_vmin <- 0;
  attr.c_vtime <- 1;
  tcsetattr stdin TCSAFLUSH attr
;;

let disable_raw_mode () =
  let open Unix in
  tcsetattr stdin TCSAFLUSH editor_config.termios
;;

let () =
  (* Out_channel.flush Out_channel.stdout; *)
  getWindowSize ();
  enable_raw_mode ();
  editor_refresh_screen ();
  editor_process_keypresses ();
  disable_raw_mode ()
;;
