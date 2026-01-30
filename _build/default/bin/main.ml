let original_termios = Unix.tcgetattr Unix.stdin

type editor_config =
  { termios : Unix.terminal_io
  ; mutable screen_rows : int (* ; mutable screen_cols : int *)
  }

let () = Out_channel.set_buffered Out_channel.stdout true
let editor_config = { termios = original_termios; screen_rows = 0 }

let getWindowSize () =
  let stty_cmd = Unix.open_process_args_in "stty" [| "stty"; "size" |] in
  let get_stty_cmd = In_channel.input_all stty_cmd in
  let stty_cmd_split = String.split_on_char ' ' get_stty_cmd in
  let rows = int_of_string (List.nth stty_cmd_split 0) in
  (* let cols = int_of_string (List.nth stty_cmd_split 1) in *)
  editor_config.screen_rows <- rows
;;

(* editor_config.screen_cols <- cols *)

(* module Editor_config : Editor_config = struct *)

(* let is_ctrl c = *)
(*   let char_ascii = int_of_char c in *)
(*   if char_ascii <= 31 || char_ascii = 127 then true else false *)
(* ;; *)

let ctrl_key c = int_of_char c land 0x1f

let editor_read_key () =
  let stdin = In_channel.stdin in
  let byte_seq = Bytes.make 1 '0' in
  (* let init_char = Bytes.get byte_seq 0 in *)
  let input = In_channel.input stdin byte_seq 0 1 in
  if input = 0 then None else Some (Bytes.get byte_seq 0)
;;

let editor_draw_rows () =
  let rec draw y =
    let max = editor_config.screen_rows in
    if y = max
    then ()
    else if y = editor_config.screen_rows - 1
    then (
      Out_channel.output_string Out_channel.stdout "~";
      Out_channel.output_string Out_channel.stdout "\x1b[K")
    else (
      Out_channel.output_string Out_channel.stdout "~\r\n";
      Out_channel.output_string Out_channel.stdout "\x1b[K";
      draw (y + 1))
  in
  draw 0
;;

let editor_refresh_screen () =
  Out_channel.output_string Out_channel.stdout "\x1b[?25l";
  Out_channel.output_string Out_channel.stdout "\x1b[H";
  editor_draw_rows ();
  Out_channel.output_string Out_channel.stdout "\x1b[H";
  Out_channel.output_string Out_channel.stdout "\x1b[?25h"
;;

let editor_process_keypresses () =
  let rec input () =
    match editor_read_key () with
    | None ->
      (* Printf.printf "%c\r\n" '0'; *)
      Out_channel.flush Out_channel.stdout;
      input ()
    | Some c ->
      (* if is_ctrl c *)
      (* then Printf.printf "%d\r\n" (Char.code c) *)
      (* else Printf.printf "%d ('%c')\r\n" (Char.code c) c; *)
      Out_channel.flush Out_channel.stdout;
      if int_of_char c = ctrl_key 'q' then editor_refresh_screen () else input ()
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
  getWindowSize ();
  enable_raw_mode ();
  editor_refresh_screen ();
  editor_process_keypresses ();
  (* Out_channel.flush Out_channel.stdout; *)
  disable_raw_mode ()
;;
