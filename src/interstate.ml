open! Core
module City = String
module Highway = String

module Network = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module HighwayEdges = struct
    module T = struct
      type t = Highway.t * (City.t list) [@@deriving compare, sexp]
    end
    
    include T
    include Comparable.Make (T)

    let of_string s =
      let connections = String.split s ~on:',' in
      match connections with
      | [] -> None
      | h::t -> Some (Highway.of_string h, t)
      
    ;;
  end

  module Edge = struct
    module T = struct
      type t = Highway.t * (City.t * City.t) [@@deriving compare, sexp]
    end
    
    include T
    include Comparable.Make (T)
  end
  
  type t = Edge.Set.t [@@deriving sexp_of]

  let rec addSet (x : HighwayEdges.t) ~list:(list : Edge.t list) = 
    let highwayName = fst x in 
    let cityList = snd x in
    let firstCity = List.hd_exn cityList in
    let removedCityList = List.tl_exn cityList in

    match removedCityList with 
    | [] -> list
    | _ -> 
      (let newList = list @ (List.concat_map removedCityList ~f:(fun dest -> [(highwayName, (firstCity, dest)); (highwayName, (dest, firstCity))])) in
      addSet (highwayName, removedCityList) ~list:newList)
  ;;


  let of_file input_file =
    let edges : Edge.t list = [] in
    let connections =
      In_channel.read_lines (File_path.to_string input_file) 
      |> List.concat_map ~f:(fun s ->
        match HighwayEdges.of_string s with
        | Some highwayEdge -> 
          addSet ~list:edges highwayEdge;
        | None ->
          printf "ERROR: Could not parse line as connection; dropping. %s\n" s;
          [])
    in
    Edge.Set.of_list connections
end


let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing interstates and the cities they go through"
      in
      fun () -> 
        let network = Network.of_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.Concrete (City)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = sprintf !"\"%s\"" v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

  let visualize_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:
        "parse a file listing friendships and generate a graph visualizing the social \
         network"
      [%map_open
        let input_file =
          flag
            "input"
            (required File_path.arg_type)
            ~doc:"FILE a file listing all friendships"
        and output_file =
          flag
            "output"
            (required File_path.arg_type)
            ~doc:"FILE where to write generated graph"
        in
        fun () ->
          let network = Network.of_file input_file in
          let graph = G.create () in
          Set.iter network ~f:(fun (_highway, (source, dest)) ->
            print_endline source;
            print_endline dest;
            (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
               they don't already exist. *)
            G.add_edge graph source dest);
          Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;
          printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
  ;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
