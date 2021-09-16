open Blockchain

type message_type =
    | QueryLatest
    | QueryAll
    | ResponseBlockchain
    [@@deriving yojson]

type message =
    { mtype: message_type
    ; data: string
    } [@@deriving yojson]

let write peer message =
    message
    |> yojson_of_message
    |> Yojson.Safe.to_string
    |> Dream.send peer

let broadcast peers message =
    List.iter (fun peer -> write peer message |> ignore) peers

let chain_message c =
    let data =
        yojson_of_t c
        |> Yojson.Safe.to_string in
    { mtype = ResponseBlockchain; data }
    

let handle_blockchain_response peers rc cc =
    match (rc, cc) with
    | ([], _) -> cc
    | (_::_, _) when not (validate_top rc) -> print_endline "Invalid block received"; []
    | ([GenesisBlock _], []) -> rc
    | ([GenesisBlock _], _) -> cc
    | (Block rb :: _, Block cb :: _) when rb.index > cb.index ->
            Printf.printf "Blockchain possibly behind. We got %d. Peer got: %d" cb.index rb.index;
            if cb.hash = rb.previous_hash then
                (let cc' = Block rb :: cc in
                chain_message cc'
                |> broadcast peers;
                cc')
            else
                (print_endline "Received longer chain, replacing";
                replace rc cc)
    | _ -> print_endline "Invalid chain received"; cc

let init_server port c =
    let current_chain = ref c in
    Dream.run ~port
    @@ Dream.logger
    @@ Dream.router [
        Dream.get "/blocks" (fun _ ->
            yojson_of_t !current_chain
            |> Yojson.Safe.to_string
            |> Dream.json
        );

        Dream.post "/mineBlock" (fun req ->
            let%lwt body = Dream.body req in
            current_chain := add body !current_chain;
            List.hd !current_chain
            |> yojson_of_block
            |> Yojson.Safe.to_string
            |> Dream.json
        );

    ]
    @@ Dream.not_found
