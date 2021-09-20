open Naivecoin

let () =
    Blockchain.init "a"
    |> Blockchain.add "b"
    |> Blockchain.add "c"
    |> Networking.init_server 5000
