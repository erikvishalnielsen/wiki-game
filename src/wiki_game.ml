open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  let namespaceFilter (n:string) : bool = 
    (let opt = Wikipedia_namespace.namespace n in
    match opt with
    | None -> true
    | Some _namespace -> false) in 

  parse contents
  $$ "a[href*=/wiki/]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:namespaceFilter
  |> List.dedup_and_sort ~compare:String.compare
;;

let _get_title contents : string list =
  let open Soup in
  parse contents
  $$ "title"
  |> to_list
  |> List.map ~f:(fun b -> texts b |> String.concat ~sep:"" |> String.strip)
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)

module WebsiteVis = struct 
  type t = 
    { link : string;
      name : string;
      depth : int; 
    }
    [@@deriving compare, sexp, hash, equal]

    let to_string t = t.name
    let _link t = t.link
    let _name t = t.name
    let _depth t = t.depth
end

let correct_url url (how_to_fetch : File_fetcher.How_to_fetch.t) = 
  match how_to_fetch with 
  | Local _ -> url
  | Remote -> if not (String.is_prefix ~prefix:"https://" url) then "https://en.wikipedia.org" ^ url else url

(*MAX DEPTH DECREASES UNTIL IT REACHES ZERO DURING THIS FUNCTION*)
let rec vizRec howToFetch ~resource:article ~websiteList ~websiteQueue ~d  = 
  let correctUrl = correct_url article howToFetch in
  print_endline correctUrl;
  let contents : string = File_fetcher.fetch_exn howToFetch ~resource:correctUrl in
  let links = get_linked_articles contents in
  let slashPos = List.map links ~f:(fun f -> String.rindex f '/') in
  let _titleInd = 
    (match String.rindex article '/' with
    | Some index -> index
    | None -> -1)
  in
  (*let _articleTitle = get_title contents in*)

  let articleRec : WebsiteVis.t = {link = article; name = List.last_exn (String.split article ~on:'/'); depth = d} in
  

  List.iter2_exn slashPos links ~f:(fun opt linked -> 
    match opt with
    | Some _index -> (
      if d > 0 then 
        let newItem : WebsiteVis.t = {link = linked; name = List.last_exn (String.split linked ~on:'/'); depth = d - 1} in
        Queue.enqueue websiteQueue newItem;
        Queue.enqueue websiteList (articleRec, newItem);
        ) (*MAY NEED TO DO >= HERE*)
    | None -> print_endline "this shouldn't have happened"
  );

  match (Queue.dequeue websiteQueue) with 
  | Some website -> (
    vizRec howToFetch ~resource:(WebsiteVis._link website) ~websiteList:websiteList ~websiteQueue:websiteQueue ~d:(d-1);)
  | None -> websiteList 
;;


let rec vizRec2 howToFetch ~resource:article ~websiteList ~websiteQueue ~d ~dest  = 
  let correctUrl = correct_url article howToFetch in
  let contents : string = File_fetcher.fetch_exn howToFetch ~resource:correctUrl in
  let links = get_linked_articles contents in
  let slashPos = List.map links ~f:(fun f -> String.rindex f '/') in
  let _titleInd = 
    (match String.rindex article '/' with
    | Some index -> index
    | None -> -1)
  in
  (*let _articleTitle = get_title contents in*)

  let articleRec : WebsiteVis.t = {link = correctUrl; name = List.last_exn (String.split article ~on:'/'); depth = d} in
  
  (match (List.exists (Queue.to_list websiteList) ~f:(fun item -> String.equal (WebsiteVis._name (fst item)) (WebsiteVis._name articleRec))) with
  | true -> ()
  | false -> (List.iter2_exn slashPos links ~f:(fun opt linked -> 
    match opt with
    | Some _index -> (
      let newItem : WebsiteVis.t = {link = (correct_url linked howToFetch); name = List.last_exn (String.split linked ~on:'/'); depth = d - 1} in
      if d > 0 then (
        Queue.enqueue websiteQueue newItem;
        Queue.enqueue websiteList (articleRec, newItem);
      )) 
    | None -> print_endline "this shouldn't have happened"
    ;
    )
  ));

  match (String.equal dest correctUrl) with 
  | true -> websiteList
  | false -> (
    match (Queue.dequeue websiteQueue) with 
  | Some website -> (
    vizRec2 howToFetch ~resource:(WebsiteVis._link website) ~websiteList:websiteList ~websiteQueue:websiteQueue ~d:(d-1) ~dest:dest;)
  | None -> websiteList 
  )
;;

module G = Graph.Imperative.Graph.Concrete (WebsiteVis)

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
    let vertex_attributes v = [ `Shape `Box; `Label (WebsiteVis.to_string v); `Fillcolor 1000 ]
    let vertex_name v = sprintf !"\"%s\"" (WebsiteVis.to_string v)
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let websiteList = Queue.create() in
  let websiteQueue = Queue.create() in

  let results = vizRec how_to_fetch ~resource:origin ~websiteList:websiteList ~websiteQueue:websiteQueue ~d:max_depth in
  let resultsList = Queue.to_list results in
  let graph = G.create () in
  List.iter resultsList ~f:(fun (websiteVis1, websiteVis2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
    G.add_edge graph websiteVis1 websiteVis2);
    Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)

(*module WikiRace = struct 
    type t = 
      { curr : WebsiteVis.t;
        prev : WebsiteVis.t;
      }
      [@@deriving compare, sexp, hash, equal]
  
      let curr t = t.curr 
      let prev t = t.prev
  end*)

let rec pathRec ~destination ~graphEdges ~queue ~list = 
  let cur = Queue.dequeue queue in
  match cur with 
  | None -> None
  | Some (web1, web2) -> (
     
  let curr = web2 in
  let prev = web1 in
  let visitedAlready = List.exists list ~f:(fun elt -> String.equal (WebsiteVis._link (fst elt)) (WebsiteVis._link curr)) in
  
  (match visitedAlready with
  | true -> pathRec ~destination:destination ~graphEdges:graphEdges ~queue:queue ~list:list
  | false -> (

  let currStr = WebsiteVis._link curr in
  let newList = (list @ [(curr, prev)]) in

  (match String.equal currStr destination with
  | true -> Some newList
  | false -> (
    let posPaths = List.map (List.filter graphEdges ~f:(fun (from, _to) -> (String.equal (WebsiteVis._link from) currStr))) ~f:(fun (_a,b) -> b) in
    
    let isempty = List.is_empty posPaths in
    let itemDepth = WebsiteVis._depth curr in
    match (isempty || (Int.equal itemDepth 1)) with
    | true -> pathRec ~destination:destination ~graphEdges:graphEdges ~queue:queue ~list:list
    | false -> (
    List.iter posPaths ~f:(fun path -> Queue.enqueue queue (curr, path));
    pathRec ~destination:destination ~graphEdges:graphEdges ~queue:queue ~list:newList)
  )))))
;;

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let websiteList = Queue.create() in
  let websiteQueue = Queue.create() in
  let pathQueue = Queue.create() in
  let path = [] in

  let results = Queue.to_list (vizRec2 how_to_fetch ~resource:origin ~websiteList:websiteList ~websiteQueue:websiteQueue ~d:max_depth ~dest:destination) in
  let startingPoint = fst (List.find_exn results ~f:(fun elt -> String.equal origin (WebsiteVis._link (fst elt)))) in
  Queue.enqueue pathQueue (startingPoint,startingPoint);
  print_endline "got results list";
  pathRec ~destination:destination ~graphEdges:results ~queue:pathQueue ~list:path
;;

let rec recPrint ~resultsList ~start ~list : WebsiteVis.t list = 
  let prevItem = (List.find_exn resultsList ~f:(fun item -> String.equal (WebsiteVis._link start) (WebsiteVis._link (fst item)))) in
  match (String.equal (WebsiteVis._link (fst prevItem)) (WebsiteVis._link (snd prevItem))) with
  | true -> list
  | false -> (
  let prevThing = snd prevItem in
  recPrint ~resultsList:resultsList ~start:prevThing ~list:([prevThing] @ list))
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> 
          print_endline "Program has finished";
          let list = [fst (List.last_exn trace)] in
          let printList = recPrint ~resultsList:trace ~start:(fst (List.last_exn trace)) ~list:list in
          List.iter printList ~f:(fun web -> print_endline (WebsiteVis.to_string web));
          ]
;;
(*List.iter trace ~f:(fun web -> print_endline (WebsiteVis.to_string web))*)

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
