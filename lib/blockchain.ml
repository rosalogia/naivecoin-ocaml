type timestamp = [%import: Unix.tm] [@@deriving yojson]

type block =
    | GenesisBlock of
        { index: int
        ; hash: string
        ; timestamp: timestamp
        ; data: string
        }
    | Block of
        { index: int
        ; hash: string
        ; previous: block
        ; timestamp: timestamp
        ; data: string
        }
    [@@deriving yojson]

(* Set of accessor functions for common properties
 * of both types of blocks. Feel free to let me know
 * how you feel about this sort of thing. It seems to
 * reduce a lot of code repetition later on, but the
 * definitions themselves of course involve a lot of
 * repetition. If this is at all useful anywhere else,
 * maybe a ppx_deriving would be interesting or useful? *)
let index_of block =
    match block with
    | GenesisBlock b -> b.index
    | Block b -> b.index

let hash_of block =
    match block with
    | GenesisBlock b -> b.hash
    | Block b -> b.hash

let timestamp_of block =
    match block with
    | GenesisBlock b -> b.timestamp
    | Block b -> b.timestamp

let data_of block =
    match block with
    | GenesisBlock b -> b.data
    | Block b -> b.data

let previous_hash_of block =
    match block with
    | GenesisBlock _    ->  failwith "Error: tried to obtain previous hash of genesis block"
    | Block b           ->  hash_of b.previous 

let length chain =
    let rec aux c' t =
        match c' with
        | GenesisBlock _ -> t + 1
        | Block b -> aux b.previous (t + 1) in
    aux chain 0

let hash_block index timestamp ?(previous_hash = "") data =
    (* We collect the various components of the block into a string list
     * then concatenate the resulting strings before hashing the result. *)
    [ string_of_int index
    ; previous_hash
    ; Unix.mktime timestamp
        |> fst
        |> string_of_float
    ; data ]
    |> String.concat ""
    |> Sha256.string
    |> Sha256.to_hex

let add data previous =
    let timestamp =
        Unix.time ()
        |> Unix.gmtime in
    let index = (index_of previous) + 1 in
    Block
        { index
        ; hash = hash_block index timestamp ~previous_hash:(hash_of previous) data
        ; previous
        ; timestamp
        ; data
        }

let validate_top chain =
    match chain with
    | GenesisBlock _ -> true
    | Block b ->
            let previous_hash = previous_hash_of chain in
            let computed_hash = hash_block b.index b.timestamp ~previous_hash b.data in
            let (previous_index) =
                match b.previous with
                | GenesisBlock p -> p.index
                | Block p -> p.index in
                
            b.index = previous_index + 1
            && computed_hash = b.hash

let validate_chain chain =
    let rec aux chain' result =
        match (chain', result) with
        | (_, false) -> false
        | (GenesisBlock _, _) -> true 
        | (Block b, _) -> aux b.previous (validate_top chain') in
    aux chain true

let replace new_chain chain =
    if length new_chain > length chain && validate_chain new_chain then
        new_chain
    else
        chain
