namespace FSharpBio 

module Util = 

    let frequencies xs = 
        xs
        |> Seq.groupBy (fun x -> x)
        |> Seq.map (fun (elem, elems) -> (elem, Seq.length elems))
        |> Map.ofSeq
    
    let combineOptions f x y = 
        match (x, y) with
        | (Some(a), Some(b)) -> Some(f a b)
        | _ -> None
     
    let combineSequenceOfOptions seq = 
        seq
        |> Seq.fold (combineOptions (fun x y -> y :: x))  (Some([])) 

    let mkStringOfSequence seq = 
        seq
        |> Seq.map (fun x -> x.ToString())
        |> Seq.fold(+) ""

    type MaybeBuilder() = 

        member this.Return(x) = Some(x)

        member this.Bind(x, f) = 
            match x with
            | Some(y) -> f(y)
            | None -> None
        
        member this.ReturnFrom(x) = x

        member this.Zero = None