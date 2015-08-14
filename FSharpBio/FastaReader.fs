namespace FSharpBio

module FastaReader = 

    open FSharpBio.Nucleotide
    open FSharpBio.FastaRecord

    let isNotMarker (s : string) = 
        s.StartsWith(">") = false

    let rec splitByFastaMarker (xs : seq<string>) = 
        seq {
            let nameOfSequence = Seq.take 1 xs
            let ys = Seq.skip 1 xs
            let sequence = Seq.takeWhile isNotMarker ys
            let zs = Seq.skipWhile isNotMarker ys
            yield {sequence = stringToNucleotideSequence (Seq.reduce (+)sequence); 
                name = Seq.reduce (+) nameOfSequence}
            yield! splitByFastaMarker zs
        }

    let read filepath = 
        let lines = System.IO.File.ReadLines(filepath)
        lines |> splitByFastaMarker