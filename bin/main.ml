open Naivecoin

let () =
    ["one"; "two"; "three"]
    |> List.fold_left (fun c d -> Blockchain.add d c) []
    |> Networking.init_server 5000
