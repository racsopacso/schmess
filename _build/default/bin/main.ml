type players = White | Black 
type piece_types = Pawn | Rook | King | Queen | Knight | Bishop 
type piece = {player:players; piece:piece_types}

let read_chessboard () = 
  let chessboard = Array.make_matrix 8 8 None in
  let line = read_line () in
  let strlist = Str.split (Str.regexp " ") line in
  let str_to_piece str plyr =
    if String.length str > 1 then
    match str.[1] with
    | 'R' -> Some {player=plyr; piece=Rook}
    | 'K' -> Some {player=plyr; piece=King}
    | 'Q' -> Some {player=plyr; piece=Queen}
    | 'B' -> Some {player=plyr; piece=Bishop}
    | 'N' -> Some {player=plyr; piece=Knight}
    | 'P' -> Some {player=plyr; piece=Pawn}
    | _ -> None 
    else None
  in
  let str_to_player str = 
    if String.length str > 0 then
    match str.[0] with
    | 'N' -> None
    | 'W' -> str_to_piece str White
    | 'B' -> str_to_piece str Black
    | _ -> None
    else None
  in
  let rec tokener strlist idx chessboard =
    let x = idx mod 8 in
    let y = idx / 8 in
    match strlist with 
    | s :: [] -> (chessboard.(x).(y) <- str_to_player s); chessboard
    | s :: t -> (chessboard.(x).(y) <- str_to_player s); tokener t (idx+1) chessboard
    | [] -> chessboard
  in
  tokener strlist 0 chessboard;;

let generate_pawn_move chessboard x y piece =
  let other_player = 
    match piece.player with
    | Black -> White
    | White -> Black
  in
  let en_passant_y =
    match piece.player with 
    | Black -> 3
    | White -> 4
  in
  let x_to_check =
    match x with 
    | 0 -> [|1|]
    | 7 -> [|6|]
    | _ -> [|x-1; x+1|]
  in
  let examine_en_passant x = 
    match chessboard.(x).(y) with
    | Some a -> (
      match a with
        | {player = op; piece = Pawn} when op = other_player -> Some (Array.copy chessboard)
        | _ -> None)
    | _ ->  None
  in
  if y != en_passant_y then [|None|] else
    Array.map examine_en_passant x_to_check;
  ;;

let piece_checker chessboard x y = 
  match chessboard.(x).(y) with 
  | Some a when a.piece = Pawn -> generate_pawn_move chessboard x y a
  | _ -> [|None|]

let option_crawler array func =
  let doer elem =
    match elem with
    | Some a -> func a 
    | None -> ()
  in Array.iter doer array;;

let print_piece chessboard x y = 
  let decode_piece plyr_str piece =
    match piece with
    | King -> print_string (plyr_str ^ "K ")
    | Queen -> print_string (plyr_str ^ "Q ")
    | Bishop -> print_string (plyr_str ^ "B ")
    | Knight -> print_string (plyr_str ^ "N ")
    | Pawn -> print_string (plyr_str ^ "P ")
    | Rook -> print_string (plyr_str ^ "R ")
  in
  match chessboard.(x).(y) with 
  | None -> print_string "N "
  | Some a -> (
    match a.player with 
    | White -> decode_piece "W" a.piece
    | Black -> decode_piece "B" a.piece) ;;

let board_crawler func chessboard =
  for y = 0 to 7 do
    for x = 0 to 7 do
      func chessboard x y;
    done
  done;;

let () = 
  let current_board = read_chessboard ()
in 
let print_from_board_option board_option_list =
  option_crawler board_option_list (board_crawler print_piece) 
in
let combo_func chessboard x y =
  piece_checker chessboard x y |> print_from_board_option
in
board_crawler combo_func current_board
  
  
  