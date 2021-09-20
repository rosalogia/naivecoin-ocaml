open Blockchain

type message =
    (* | QueryLatest *)
    | QueryAll
    | ResponseBlockchain of string
    [@@deriving yojson]

type peer =
    { socket: Dream.websocket
    ; connection_time: float
    }

type state =
    { mutable peers: peer list
    ; mutable chain: block
    }

(* type message = *)
(*     { mtype: message_type *)
(*     ; data: string *)
(*     } [@@deriving yojson] *)

let write peer message =
    message
    |> yojson_of_message
    |> Yojson.Safe.to_string
    |> Dream.send peer.socket

let broadcast peers message =
    List.iter (fun peer -> write peer message |> ignore) peers

let chain_message c =
    let data =
        yojson_of_block c
        |> Yojson.Safe.to_string in
    ResponseBlockchain data

let handle_blockchain_response state received =
    if length received > length state.chain then
        if (previous_hash_of received) = (hash_of state.chain) then
            (received
            |> chain_message
            |> broadcast state.peers;
            received)
        else
            replace received state.chain
    else
        state.chain

let init_message_handler state peer =
    match%lwt Dream.receive peer.socket with
    | Some m ->
            let message =
                m
                |> Yojson.Safe.from_string
                |> message_of_yojson in
            (match message with
            | QueryAll ->
                    state.chain
                    |> chain_message
                    |> write peer
            | ResponseBlockchain chain_string ->
                    state.chain <-
                        chain_string
                        |> Yojson.Safe.from_string
                        |> block_of_yojson
                        |> handle_blockchain_response state;
                        Lwt.return_unit)
    | None ->
            state.peers <- List.filter
                (fun { socket = _; connection_time } -> connection_time <> peer.connection_time) state.peers;
            Dream.close_websocket peer.socket

let init_connection state peer =
    state.peers <- peer :: state.peers;
    state.chain
    |> chain_message
    |> write peer
    |> ignore;
    init_message_handler state peer

let init_p2p_server state =
    Dream.websocket (fun ws ->
        { socket = ws; connection_time = Unix.time () }
        |> init_connection state)


let init_server state port =
    Dream.run ~port
    @@ Dream.logger
    @@ Dream.router [
        Dream.get "/blocks" (fun _ ->
            yojson_of_block state.chain
            |> Yojson.Safe.to_string
            |> Dream.json
        );

        Dream.post "/mineBlock" (fun req ->
            let%lwt body = Dream.body req in
            state.chain <- add body state.chain;
            state.chain
            |> yojson_of_block
            |> Yojson.Safe.to_string
            |> Dream.json
        );

        (* I'm unfortunately quite lost on how to implement this with dream
         * since AFAICT Dream doesn't ?? store any information about the websocket
         * connection, and the websocket type is ... empty? I'd LOVE to talk about
         * this with someone at some point, it's quite perplexing to me and I'd like
         * to overcome it. For the moment, I'm uniquely identifying peers by the time
         * at which they established a connection, but it would be nice if we could ID
         * peers by their address instead. *)
        
        (* Dream.get "/peers" (fun _ -> 

           ) *)

        Dream.get "/addPeer" (fun _ ->
            init_p2p_server state
        )
    ]
    @@ Dream.not_found
