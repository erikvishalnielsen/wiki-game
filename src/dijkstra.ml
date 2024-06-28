(* Note: This incantation allows us to tell the compiler to temporarily stop
   notifying us when we have unused functions and values. Feel free to delete
   after completing exercise 6. *)
[@@@disable_unused_warnings]

open Core

module Node_id : sig
  (** A [Node_id.t] uniquely identifies a node in a graph. We will using it later for
      looking up and setting the state of nodes in the course of our path search. *)
  type t [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t

  val create : int -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  (* Remember that this is the syntax for include modules such as [Map] and
     [Set] that are provided by [Comparable.Make] to our module. In our case,
     we use [Node_id.Map.t] in the [Nodes.t]. *)
  include T
  include Comparable.Make (T)

  let create id = id
end

module Edge = struct
  (** This type represents an edge between two nodes [a] and [b]. Note that since we are
      working with undirected graphs, the order of [a] and [b] do not matter. That is, an
      [Edge.t] { a = 1; b = 2; ... } is equivalent to { a = 2; b = 1; ... } *)

  module T = struct
    type t =
      { a : Node_id.t
      ; b : Node_id.t
      ; distance : int
      }
    [@@deriving compare, equal, sexp]

    let a t = t.a
    let b t = t.b
    let distance t = t.distance
  end

  include T
  include Comparable.Make (T)
end

module Edges = struct
  type t = Edge.t list [@@deriving sexp]

  (* Exercise 1: Given a [t] (list of edges) and a [Node_id.t], implement a
     function that returns a list of neighboring nodes with their
     corresponding distances. *)
  let neighbors t node_id : (Node_id.t * int) list =
    List.map
      (List.filter t ~f:(fun elt ->
         Node_id.equal (Edge.a elt) node_id
         || Node_id.equal (Edge.b elt) node_id))
      ~f:(fun item ->
        if Node_id.equal (Edge.a item) node_id
        then Edge.b item, Edge.distance item
        else Edge.a item, Edge.distance item)
  ;;

  (* We've left all of the tets in this file disabled. As you complete the
     exercises, please make sure to remove `[@tags "disabled"]` and run `dune
     runtest` to ensure that your implementation passes the test. *)
  let%expect_test ("neighbors") =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      Edge.
        [ { a = n0; b = n1; distance = 1 }
        ; { a = n1; b = n2; distance = 3 }
        ; { a = n2; b = n3; distance = 2 }
        ; { a = n2; b = n4; distance = 1 }
        ; { a = n3; b = n5; distance = 5 }
        ; { a = n4; b = n5; distance = 1 }
        ]
    in
    let neighbors = neighbors t n2 in
    print_s [%message (neighbors : (Node_id.t * int) list)];
    [%expect {| (neighbors ((1 3) (3 2) (4 1))) |}]
  ;;
end

module Node = struct
  module State = struct
    type t =
      | Origin (** Used to mark the node where our search starts *)
      | Unseen (** Used to mark unexplored Nodes *)
      | Todo of
          { distance : int
          ; via : Node_id.t
          }
      (** Used to mark nodes that have been encountered but have not been processed yet *)
      | Done of { via : Node_id.t }
      (** Used to mark nodes that we are finished processing *)
    [@@deriving sexp]
  end

  type t = { mutable state : State.t } [@@deriving fields ~getters, sexp]

  let init () = { state = Unseen }
  let set_state t state = t.state <- state
  let state t = t.state
end

module Nodes = struct
  (** This type represents a stateful collection of nodes in our graph. These [Node.t]s
      will be updated in the course of our graph search to keep track of progress. *)
  type t = Node.t Node_id.Map.t [@@deriving sexp]

  (* Exercise 2: Given a list of edges, create a [t] that contains all nodes
     found in the edge list. Note that you can construct [Node.t]s with the
     [Node.init] function. *)
  let of_edges (edges : Edges.t) = (*FUNCTION GOES HERE*)
    (*List.dedup_and_sort (List.concat_map edges ~f:(fun edge -> [(Edge.a edge), (Edge.b edge)]))*)
    List.fold edges ~init:(Map.empty (module Node_id)) ~f:(fun map edge ->
      let a = Node.init () in
      let b = Node.init () in
      let newMap = Map.add_exn map ~key:(Edge.a edge) ~data:a in
      Map.add_exn newMap ~key:(Edge.b edge) ~data:b
    )
  ;;
  let find = Map.find_exn
  let state t node_id = find t node_id |> Node.state

  let set_state t id state =
    let node = Map.find_exn t id in
    Node.set_state node state
  ;;

  (* Exercise 3: Given a [t], find the next node to process by selecting the
     node with the smallest distance along with its via route. *)
  let next_node t : (Node_id.t * (int * Node_id.t)) option = 
    let filterMap = Map.filter t ~f:(fun var -> (
      match (Node.state var) with
      | Todo {distance; via} -> true
      | _ -> false
    )) in
    let mapList = Map.to_alist filterMap in
    
    match(List.min_elt mapList ~compare:(fun item1 item2 -> 
      let dist1 = match Node.state (snd item1) with
      | Todo {distance; via} -> distance
      | _ -> failwith "Error"
      in
      let dist2 = match Node.state (snd item2) with
      | Todo {distance; via} -> distance
      | _ -> failwith "Error"
      in
      Int.compare dist1 dist2
    )) with
    | Some ans -> 
      let tupleAns = match Node.state (snd ans) with
      | Todo {distance; via} -> (distance, via)
      | _ -> failwith "Error"
      in
      Some ((fst ans), tupleAns)
    | None -> None
  ;;

  let%expect_test ("next_node") =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let next_node = next_node t in
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}]
  ;;

  (* Exercise 4: Given a [t] that has already been processed from some origin
     -- that is the origin has been marked as [Origin] and nodes on the
     shortest path have been marked as [Done] -- return the path from the
     origin to the given [distination]. *)

  let rec pathRec ~list ~stack ~dest = 
    let currPick = match Node.state (snd (Stack.top_exn stack)) with
    | Done {via} -> via
    | _ -> failwith "erroroneus"
  in
    match (Node_id.equal (currPick) (dest)) with
    | true -> stack
    | false -> (
      List.iter list ~f:(fun item -> (if (Node_id.equal (currPick) (fst item)) then Stack.push stack (item)));
      pathRec ~list:list ~stack:stack ~dest:dest;
    )
  ;;
  let path t destination : Node_id.t list = 
    let filterMap = Map.to_alist (Map.filter t ~f:(fun var -> (
      match (Node.state var) with
      | Done {via} -> true
      | _ -> false
    ))) in
    let originPoint = List.hd_exn (Map.to_alist (Map.filter t ~f:(fun var -> (
      match (Node.state var) with
      | Origin -> true
      | _ -> false
    )))) in
    let destPoint = List.hd_exn (Map.to_alist (Map.filter_keys t ~f:(fun key -> (
      match (Node_id.equal key destination) with
      | true -> true
      | false -> false
    )))) in
    let originVia = fst originPoint
  in
    let stackRes = Stack.create () in
    Stack.push stackRes (destPoint);

    [originVia] @ (List.map (Stack.to_list (pathRec ~list:filterMap ~stack:stackRes ~dest:originVia)) ~f:(fun item -> (fst item)))
  ;;

  (* Excercise 5: Write an expect test for the [path] function above. *)
  let%expect_test ("path") =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n3 } }
      ; n3, { Node.state = Done { via = n1 } }
      ; n4, { Node.state = Done { via = n2 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let path = path t n4 in
    print_s [%message (path : Node_id.t list)];
    [%expect {| (path (0 1 3 2 4)) |}]
end

(* Exercise 6: Using the functions and types above, implement Dijkstras graph
   search algorithm! Remember to reenable unused warnings by deleting the
   first line of this file. *)
let shortest_path ~edges ~origin ~destination : Node_id.t list = 
  let map = Nodes.of_edges edges in
  let originPoint = List.hd_exn (Map.to_alist (Map.filter map ~f:(fun var -> (
      match (Node.state var) with
      | Origin -> true
      | _ -> false
  )))) in
  let destPoint = List.hd_exn (Map.to_alist (Map.filter_keys map ~f:(fun key -> (
      match (Node_id.equal key destination) with
      | true -> true
      | false -> false
  )))) in

  (*STILL NEED TO FINISH USE HELPER FUNCTIONS ABOVE!!*)
  []
;;

let%expect_test ("shortest_path" [@tags "disabled"]) =
  let n = Node_id.create in
  let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
  let edges =
    Edge.
      [ { a = n0; b = n1; distance = 1 }
      ; { a = n1; b = n2; distance = 1 }
      ; { a = n2; b = n3; distance = 1 }
      ; { a = n2; b = n4; distance = 1 }
      ; { a = n3; b = n5; distance = 2 }
      ; { a = n4; b = n5; distance = 1 }
      ]
  in
  let origin = n0 in
  let destination = n5 in
  let path = shortest_path ~edges ~origin ~destination in
  print_s ([%sexp_of: Node_id.t list] path);
  [%expect {| (0 1 2 4 5) |}]
;;

(* Exercise 7: Add some more test cases, exploring any corner cases you can
   think of. *)
