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
        ; previous_hash: string
        ; timestamp: timestamp
        ; data: string
        }
    [@@deriving yojson]

type t = block list [@@deriving yojson]

let hash_of i t p d =
    (match p with
    | Some previous_hash -> 
        (string_of_int i) ^ (previous_hash) ^ (Unix.mktime t |> fst |> string_of_float) ^ d
    | None -> 
        (string_of_int i) ^ (Unix.mktime t |> fst |> string_of_float) ^ d)
    |> Sha256.string
    |> Sha256.to_hex

let new_block d p =
    let time =
        Unix.time ()
        |> Unix.gmtime in
    let (index, previous_hash) =
        match p with
        | Some (GenesisBlock b) -> (1, Some b.hash)
        | Some (Block b) -> (b.index + 1, Some b.hash)
        | None -> (0, None) in
    match previous_hash with
    | None ->
            GenesisBlock
                { index = index
                ; hash = hash_of 0 time None d
                ; timestamp = time
                ; data = d
                }
    | Some h ->
            Block
                { index = index
                ; hash = hash_of index time previous_hash d
                ; previous_hash = h
                ; timestamp = time
                ; data = d
                }

let add d c =
    match c with
    | [] -> [new_block d None]
    | [GenesisBlock b] -> (new_block d (Some (GenesisBlock b))) :: c
    | Block b :: _ -> (new_block d (Some (Block b))) :: c
    | _ -> c

let validate_top c =
    match c with
    | [GenesisBlock _] -> true
    | Block b :: previous :: _ ->
            let (previous_index, previous_hash) =
                match previous with
                | GenesisBlock p -> p.index, p.hash
                | Block p -> p.index, p.hash in
            [
                b.index = previous_index + 1;
                b.previous_hash = previous_hash;
                (hash_of b.index b.timestamp (Some b.previous_hash) b.data) = b.hash
            ] |> List.for_all ((=) true)
    | _ -> false

let rec validate_chain c =
    match c with
    | _ :: t -> if validate_top c then validate_chain t else false
    | _ -> false

let replace n c =
    if List.length n > List.length c && validate_chain n then
        n
    else
        c

