open! Core

type mazeType =
  | Start
  | End
  | Path
  | Wall
[@@deriving compare, sexp, hash, equal]

type direction =
  | Up
  | Down
  | Right
  | Left
  | None
[@@deriving compare, sexp, hash, equal]

module MazePoint = struct
  type t =
    { row : int
    ; col : int
    ; pointType : mazeType
    ; directionToArrive : direction (*This is direction taken to ARRIVE AT THE POINT*)
    }
  [@@deriving compare, sexp, hash, equal]

  let _point_to_string t =
    "(" ^ Int.to_string t.row ^ ", " ^ Int.to_string t.col ^ ")"
  ;;

  let _to_string t =
    match t.directionToArrive with
    | Up -> "Up"
    | Down -> "Down"
    | Right -> "Right"
    | Left -> "Left"
    | None -> "None"
  ;;

  let row t = t.row
  let col t = t.col
  let pointType t = t.pointType
end

let rec solveRec ~pointStack ~pointList ~visited : MazePoint.t Base.Stack.t * bool =
  let currElt = Stack.pop_exn pointStack in
  Stack.push visited currElt;
  let eltRow = MazePoint.row currElt in
  let eltCol = MazePoint.col currElt in
  let upOpt =
    List.find pointList ~f:(fun point ->
      (MazePoint.row point = eltRow - 1 && MazePoint.col point = eltCol)
      && (match MazePoint.pointType point with
          | Path | End -> true
          | _ -> false)
      && not
           (List.exists (Stack.to_list visited) ~f:(fun pt ->
              MazePoint.row pt = eltRow - 1 && MazePoint.col pt = eltCol)))
  in
  let downOpt =
    List.find pointList ~f:(fun point ->
      (MazePoint.row point = eltRow + 1 && MazePoint.col point = eltCol)
      && (match MazePoint.pointType point with
          | Path | End -> true
          | _ -> false)
      && not
           (List.exists (Stack.to_list visited) ~f:(fun pt ->
              MazePoint.row pt = eltRow + 1 && MazePoint.col pt = eltCol)))
  in
  let leftOpt =
    List.find pointList ~f:(fun point ->
      (MazePoint.row point = eltRow && MazePoint.col point = eltCol - 1)
      && (match MazePoint.pointType point with
          | Path | End -> true
          | _ -> false)
      && not
           (List.exists (Stack.to_list visited) ~f:(fun pt ->
              MazePoint.row pt = eltRow && MazePoint.col pt = eltCol - 1)))
  in
  let rightOpt =
    List.find pointList ~f:(fun point ->
      (MazePoint.row point = eltRow && MazePoint.col point = eltCol + 1)
      && (match MazePoint.pointType point with
          | Path | End -> true
          | _ -> false)
      && not
           (List.exists (Stack.to_list visited) ~f:(fun pt ->
              MazePoint.row pt = eltRow && MazePoint.col pt = eltCol + 1)))
  in
  (match upOpt with
   | Some pt ->
     let newElt : MazePoint.t =
       { row = MazePoint.row pt
       ; col = MazePoint.col pt
       ; pointType = MazePoint.pointType pt
       ; directionToArrive = Up
       }
     in
     Stack.push pointStack newElt
   | None -> ());
  (match downOpt with
   | Some pt ->
     let newElt : MazePoint.t =
       { row = MazePoint.row pt
       ; col = MazePoint.col pt
       ; pointType = MazePoint.pointType pt
       ; directionToArrive = Down
       }
     in
     Stack.push pointStack newElt
   | None -> ());
  (match leftOpt with
   | Some pt ->
     let newElt : MazePoint.t =
       { row = MazePoint.row pt
       ; col = MazePoint.col pt
       ; pointType = MazePoint.pointType pt
       ; directionToArrive = Left
       }
     in
     Stack.push pointStack newElt
   | None -> ());
  (match rightOpt with
   | Some pt ->
     let newElt : MazePoint.t =
       { row = MazePoint.row pt
       ; col = MazePoint.col pt
       ; pointType = MazePoint.pointType pt
       ; directionToArrive = Right
       }
     in
     Stack.push pointStack newElt
   | None -> ());
  match MazePoint.pointType currElt with
  | End -> (visited, true)
  | _ -> (let newVisit, solBool = solveRec ~pointStack:pointStack ~pointList:pointList ~visited:visited in
    match solBool with
    | true -> newVisit, true
    | false -> (
      let _trash = Stack.pop_exn newVisit in
      newVisit, false)
  )
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let mazeList =
          In_channel.read_lines (File_path.to_string input_file)
        in
        let charList =
          List.map mazeList ~f:(fun line -> String.to_list line)
        in
        let pointList =
          List.concat
            (List.mapi charList ~f:(fun rowNum list ->
               List.mapi list ~f:(fun colNum char ->
                 let typePoint : mazeType =
                   match char with
                   | 'S' -> Start
                   | '.' -> Path
                   | 'E' -> End
                   | _ -> Wall
                 in
                 let point : MazePoint.t =
                   { row = rowNum
                   ; col = colNum
                   ; pointType = typePoint
                   ; directionToArrive = None
                   }
                 in
                 point)))
        in
        let startPoint =
          List.find_exn pointList ~f:(fun point ->
            match MazePoint.pointType point with Start -> true | _ -> false)
        in
        let pointStack = Stack.create () in
        let visitedStack = Stack.create () in
        Stack.push pointStack startPoint;
        (*SHOULD GET A STACK OF POINTS BACK THEN TAKE DIRECTIONS OUT OF IT*)
        let directionSolved, _solvedBool =
          solveRec ~pointStack ~pointList ~visited:visitedStack
        in
        let output =
          List.tl_exn (List.rev (Stack.to_list directionSolved))
        in
        print_endline "These are coordinates traveled through to get to end of maze, where (0,0) is top left corner";
        List.iter output ~f:(fun pt ->
          print_endline (MazePoint._point_to_string pt))]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
