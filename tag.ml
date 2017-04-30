
open Core.Std
open Option
open Yojson

exception Object_not_found of string       
   
type id = string

type obj = {
    id : id;
    descr : string;
    points: int
  }

type room_exit = {
    direction: string;
    room: id;
    unlock_item: id option
  }

type room = {
    info : obj;
    items: id list;
    exits: room_exit list;
    treasure: id list
  }

type item = {
    info: obj;
    treas_msg: string
  }

type game = {
    rooms: (id, room) Bst.t;
    items: (id, item) Bst.t;
    inventory: id list;
    room: id
  }

type move =
  | Go of string
  | Take of string
  | Drop of string
  | Quit
              
(* Read JSON *)

let mk_id = Yojson.Basic.Util.to_string

let mk_exit json =
  let open Yojson.Basic.Util in
  let direction = json |> member "direction" |> to_string in
  let room = json |> member "room" |> mk_id in
  let unlock_item = json |> member "unlock_item" |> to_string_option in
  {direction; room; unlock_item}

let mk_obj json =
  let open Yojson.Basic.Util in
  let id = json |> member "id" |> mk_id in
  let descr = json |> member "description" |> to_string in
  let points = json |> member "points" |> to_int in
  {id; descr; points}
  
let mk_room json =
  let open Yojson.Basic.Util in
  let info = mk_obj json in
  let items = json |> member "items" |> to_list |> List.map ~f:mk_id in
  let exits = json |> member "exits" |> to_list |> List.map ~f:mk_exit in
  let treasure = json |> member "treasure" |> to_list |> List.map ~f:mk_id in
  {info; items; exits; treasure}
  
let mk_item json =
  let open Yojson.Basic.Util in
  let info = mk_obj json in
  let treas_msg = json |> member "trs_msg" |> to_string in
  {info; treas_msg}

let mk_game json =
  let open Yojson.Basic.Util in
  let rooms = json |> member "rooms" |> to_list |> List.map ~f:mk_room
              |> List.fold_left ~init:Bst.empty ~f:(fun t r -> Bst.insert t r.info.id r) in
  let items = json |> member "items" |> to_list |> List.map ~f:mk_item
              |> List.fold_left ~init:Bst.empty ~f:(fun t i -> Bst.insert t i.info.id i) in
  let start_room = json |> member "start_room" |> mk_id in
  let start_items = json |> member "start_items" |> to_list |> List.map ~f:mk_id in

  { rooms = rooms;
    items = items;
    room = start_room;
    inventory = start_items }

(* Game accessors *)    

let contains_all big small = List.for_all ~f:(List.mem big) small

let get_room game id = Option.value_exn (Bst.lookup game.rooms id)
let get_item game id = Option.value_exn (Bst.lookup game.items id)

let current_room game = get_room game (game.room)

let remove_item (room : room) it = {room with items = List.filter ~f:((<>) it) room.items}
let add_item (room : room) it = {room with items = it :: room.items}

type transition = game * string                                  

(* game logic *)
let do_move (game : game) = function
  | Go dir -> let room = current_room game in
     (match List.find ~f:(fun e -> e.direction = dir) room.exits with
              | Some ex -> if Option.value_map ex.unlock_item ~f:(List.mem game.inventory) ~default:true
                           then ({game with room = ex.room}, "entered room " ^ ex.room)
                           else (game, "missing items")
              | None -> (game, "can't go in that direction"))
  | Take i -> let item_exists = List.mem (current_room game).items i in
              List.iter (current_room game).items ~f:print_string;
              if item_exists
              then ({game with inventory = i :: game.inventory;
                               rooms = Bst.update game.rooms game.room
                                                  (remove_item (current_room game) i)},
                    "took " ^ i)
              else (game, "item doesn't exist")
  | Drop i -> let has_item = List.mem game.inventory i in
              if has_item
              then ({game with inventory = List.filter ~f:((<>) i) game.inventory;
                               rooms = Bst.update game.rooms game.room
                                                  (add_item (current_room game) i)},
                    "dropped " ^ i)
              else (game, "don't have this item")
  | Quit -> (game, "bye")

let show_room game = sprintf "You are in room %s. %s."
                             game.room
                             (current_room game).info.descr

let show_inventory game = match game.inventory with
  | [] -> "Your inventory is empty."
  | _ -> sprintf "Your inventory contains %s." (String.concat ~sep:", " game.inventory)              

let state_msg game = sprintf "%s\n%s"
                             (show_room game)
                             (show_inventory game)

let split_first s ~on = match String.split s ~on with
  | x :: xs -> [x; String.concat ~sep:(Char.to_string on) xs]
  | [] -> []

let parse_action s = match split_first s ~on:' ' with
  | ["go"; dir] -> Some (Go dir)
  | ["take"; i] -> Some (Take i)
  | ["drop"; i] -> Some (Drop i)
  | ["quit"] -> Some (Quit)
  | _ -> None

(* IO *)

let print s = print_string s;
              print_string "\n"           

let read_action () = parse_action (read_line ())

let rec step game =
  print (state_msg game);

  match read_action () with
  | None -> step game
  | Some Quit -> ()
  | Some act -> let (new_game, msg) = do_move game act in
                print msg;
                step new_game
  
let () = let json = Yojson.Basic.from_file "example.json" in
         let game = mk_game json in
         step game;
