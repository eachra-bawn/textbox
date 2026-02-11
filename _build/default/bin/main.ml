let original_termios = Unix.tcgetattr Unix.stdin
let textboxx_version = "0.0.1"

type erow =
  { size : int
  ; chars : string
  }

type editor_config =
  { termios : Unix.terminal_io
  ; mutable cx : int
  ; mutable cy : int
  ; mutable screen_rows : int
  ; mutable screen_cols : int
  ; mutable numrows : int
  ; mutable row : erow list
  }

type editor_key =
  | Char of char
  | Escape_seq
  | Arrow_left
  | Arrow_right
  | Arrow_up
  | Arrow_down
  | Del_key
  | Home_key
  | End_key
  | Page_up
  | Page_down

let editor_key_to_int = function
  | Char c ->
    (match c with
     | 'a' -> 1000
     | 'd' -> 1001
     | 'w' -> 1002
     | 's' -> 1003
     | _ -> int_of_char c)
  | Escape_seq -> int_of_char '\x1b'
  | Arrow_left -> 1000
  | Arrow_right -> 1001
  | Arrow_up -> 1002
  | Arrow_down -> 1003
  | Del_key -> 1004
  | Home_key -> 1005
  | End_key -> 1006
  | Page_up -> 1007
  | Page_down -> 1008
;;

let () = Out_channel.set_buffered Out_channel.stdout true

let editor_config =
  { termios = original_termios
  ; cx = 1
  ; cy = 1
  ; screen_rows = 0
  ; screen_cols = 0
  ; numrows = 0
  ; row = []
  }
;;

let get_window_size () =
  let stty_cmd = Unix.open_process_args_in "stty" [| "stty"; "size" |] in
  let get_stty_cmd = In_channel.input_all stty_cmd in
  let stty_cmd_split = String.split_on_char ' ' get_stty_cmd in
  let rows = int_of_string (String.trim (List.nth stty_cmd_split 0)) in
  let cols = int_of_string (String.trim (List.nth stty_cmd_split 1)) in
  editor_config.screen_rows <- rows;
  editor_config.screen_cols <- cols
;;

let editor_append_row str =
  let new_row = { size = String.length str; chars = str } in
  editor_config.row <- new_row :: editor_config.row;
  editor_config.numrows <- editor_config.numrows + 1
;;

let editor_open filename =
  let file = In_channel.open_bin filename in
  let file_lines = In_channel.input_lines file in
  let i = ref (List.length file_lines - 1) in
  while !i >= 0 do
    let current_line =
      match List.nth file_lines !i with
      | s -> s
      | exception Failure _ -> raise (Failure "Tried to read line that doesn't exist")
    in
    editor_append_row current_line;
    i := !i - 1
  done;
  In_channel.close file
;;

let ctrl_key key = key land 0x1f

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
        then Some Escape_seq
        else (
          match Bytes.get seq 0 = '[' with
          | true ->
            if Bytes.get seq 1 >= '0' && Bytes.get seq 1 <= '9'
            then
              if In_channel.input stdin seq 2 1 != 1
              then Some Escape_seq
              else if Bytes.get seq 2 = '~'
              then (
                match Bytes.get seq 1 with
                | '1' -> Some Home_key
                | '3' -> Some Del_key
                | '4' -> Some End_key
                | '5' -> Some Page_up
                | '6' -> Some Page_down
                | '7' -> Some Home_key
                | '8' -> Some End_key
                | _ -> None)
              else None
            else (
              match Bytes.get seq 1 with
              | 'A' -> Some Arrow_up
              | 'B' -> Some Arrow_down
              | 'C' -> Some Arrow_right
              | 'D' -> Some Arrow_left
              | 'H' -> Some Home_key
              | 'F' -> Some End_key
              | _ -> Some Escape_seq)
          | false ->
            (match Bytes.get seq 0 = 'O' with
             | true ->
               (match Bytes.get seq 1 with
                | 'H' -> Some Home_key
                | 'F' -> Some End_key
                | _ -> None)
             | false -> Some (Char c))))
      else Some (Char c)
    | None -> None)
;;

let editor_draw_rows () =
  let rec draw y =
    let open Out_channel in
    if y = editor_config.screen_rows
    then ()
    else if y > editor_config.numrows
    then
      if y = editor_config.screen_rows / 3 && editor_config.numrows = 0
      then (
        let welcome_str = Printf.sprintf "Textboxx <version %s>" textboxx_version in
        let welcome_str_trunc =
          match String.length welcome_str > editor_config.screen_cols with
          | true -> String.sub welcome_str 1 editor_config.screen_cols
          | false -> welcome_str
        in
        let padding =
          ref ((editor_config.screen_cols - String.length welcome_str_trunc) / 2)
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
        output_string stdout welcome_str_trunc;
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
    else (
      let current_line =
        match y with
        | 0 -> List.nth editor_config.row y
        | _ -> List.nth editor_config.row (y - 1)
      in
      let chars_trunc =
        match current_line.size > editor_config.screen_cols with
        | true -> String.sub current_line.chars 1 editor_config.screen_cols
        | false -> current_line.chars
      in
      output_string stdout chars_trunc;
      output_string stdout "\x1b[K";
      output_string stdout "\r\n";
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
  | Arrow_left ->
    if editor_config.cx != 0
    then editor_config.cx <- editor_config.cx - 1
    else editor_config.cx <- 1
  | Arrow_right ->
    if editor_config.cx != editor_config.screen_cols
    then editor_config.cx <- editor_config.cx + 1
    else ()
  | Arrow_up ->
    if editor_config.cy != 0
    then editor_config.cy <- editor_config.cy - 1
    else editor_config.cy <- 1
  | Arrow_down ->
    if editor_config.cy != editor_config.screen_rows
    then editor_config.cy <- editor_config.cy + 1
    else ()
  | _ -> ()
;;

let editor_process_keypresses () =
  let rec input () =
    match editor_read_key () with
    | None ->
      Out_channel.flush Out_channel.stdout;
      input ()
    | Some key ->
      Out_channel.flush Out_channel.stdout;
      if editor_key_to_int key = ctrl_key (editor_key_to_int key)
      then editor_refresh_screen ()
      else (
        match key with
        | Home_key ->
          editor_config.cx <- 0;
          editor_update_cursor ();
          input ()
        | End_key ->
          editor_config.cx <- editor_config.screen_cols - 1;
          editor_update_cursor ();
          input ()
        | Page_up ->
          let times = ref editor_config.screen_rows in
          while !times != 0 do
            editor_move_cursor Arrow_up;
            editor_update_cursor ();
            times := !times - 1
          done;
          input ()
        | Page_down ->
          let times = ref editor_config.screen_rows in
          while !times != 0 do
            editor_move_cursor Arrow_down;
            editor_update_cursor ();
            times := !times - 1
          done;
          input ()
        | Arrow_up ->
          editor_move_cursor Arrow_up;
          editor_update_cursor ();
          input ()
        | Arrow_down ->
          editor_move_cursor Arrow_down;
          editor_update_cursor ();
          input ()
        | Arrow_left ->
          editor_move_cursor Arrow_left;
          editor_update_cursor ();
          input ()
        | Arrow_right ->
          editor_move_cursor Arrow_right;
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
  get_window_size ();
  enable_raw_mode ();
  if Array.length Sys.argv > 1 then editor_open Sys.argv.(1) else ();
  editor_refresh_screen ();
  editor_process_keypresses ();
  disable_raw_mode ()
;;
