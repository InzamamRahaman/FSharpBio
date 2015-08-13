

namespace FSharpBio 




module Nucleotide = 

    open FSharpBio.Util


    type Nucleotide =
        | A 
        | C
        | G 
        | T 
        | U
        with 
            override this.ToString() = 
                match this with
                | A -> "A"
                | C -> "C"
                | G -> "G"
                | T -> "T"
                | U -> "U"
                  
   

    let complement nucleotide = 
        match nucleotide with
        | A -> Some(G)
        | C -> Some(T)
        | G -> Some(A)
        | T -> Some(C)
        | U -> None

    let transcribe nucleotide = 
        match nucleotide with
        | A -> Some(A)
        | C -> Some(C)
        | G -> Some(G)
        | T -> Some(U)
        | U -> None



    let complementSequence seq = 
        seq 
        |> Seq.map complement
        |> combineSequenceOfOptions 


    let transcribeSequence seq = 
        seq
        |> Seq.map transcribe
        |> combineSequenceOfOptions

    let computeGCContent seq = 
        let extractGCCount = Seq.length << Seq.filter (fun x -> x = G || x = C)
        let count = extractGCCount seq
        let length = Seq.length seq
        (float count) / (float length)
       
