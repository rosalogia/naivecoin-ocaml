open Naivecoin

let () =
    Blockchain.init "a"
    |> Blockchain.add "b"
    |> Blockchain.add "c"
    |> (fun c -> Networking.init_server { peers = []; chain = c } 5000)
