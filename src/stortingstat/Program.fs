open FSharp.Data

type Cases = XmlProvider<"https://data.stortinget.no/eksport/saker?sesjonid=2018-2019">
type Votes = XmlProvider<"https://data.stortinget.no/eksport/voteringer?sakid=63033">
type VoteResult = XmlProvider<"https://data.stortinget.no/eksport/voteringsresultat?voteringid=7523">

type Sessions = XmlProvider<"https://data.stortinget.no/eksport/sesjoner">

[<Literal>]
let voteStatisticsCsvSample = """
Navn,Parti,Totalt,Tilstede,Tilstede(%)
"Et Navn","Et Parti",100,90,0.90
"""
type VoteStatistics = CsvProvider<voteStatisticsCsvSample, HasHeaders = true, Schema = "string,string,int,int,float">

let getVoteResult voteId =
    VoteResult.Load (sprintf "https://data.stortinget.no/eksport/voteringsresultat?voteringid=%d" voteId)

// sessiondId = "2019-2020"
let getCases sessionId =
    Cases.Load (sprintf "https://data.stortinget.no/eksport/saker?sesjonid=%s" sessionId)

let getSessions =
    Sessions.GetSample

let getVotesFromStortingetApi sessionId =
    getCases sessionId
    |> (fun s -> s.SakerListes)
    |> Array.filter (fun s -> s.Status = "behandlet")
    |> Array.map (fun s -> Votes.Load (sprintf "https://data.stortinget.no/eksport/voteringer?sakid=%d" s.Id))
    |> Array.map (fun s -> s.SakVoteringListes)
    |> Array.concat
    |> Array.filter (fun i -> i.AntallFor <> -1)

type Vote = {
    id: string;
    name: string
    party: string
    substituteFor: string option
    present: bool;
}

let notPresent = "ikke_tilstede"

let getVote voteId =
    getVoteResult voteId
    |> (fun res -> res.VoteringsresultatListe.RepresentantVoteringsresultats)
    |> Array.map (fun rep -> {
        id = rep.Representant.Id
        name = sprintf "%s, %s" rep.Representant.Etternavn rep.Representant.Fornavn
        party = rep.Representant.Parti.Navn
        substituteFor =
            if rep.Representant.VaraRepresentant
            then
                Some (sprintf "%s, %s" rep.VaraFor.Etternavn.Value rep.VaraFor.Fornavn.Value)
            else
                None
        present = rep.Votering <> notPresent;
    })

type VoteAggregated = {
    id: string;
    name: string
    party: string
    substituteFor: string option
    total: int
    present: int
    presentPercentage: float
}

let fromVote (v: Vote) =
    {
        id = v.id
        name = v.name
        party = v.party
        substituteFor = v.substituteFor
        total = 0
        present = 0
        presentPercentage = 0.
    }

let folder (s: VoteAggregated) (n: Vote) =
    let newTotal = s.total + 1
    let newPresent = s.present + if n.present then 1 else 0
    let newPresentPercentage = float newPresent / float newTotal
    { s
      with
        total = newTotal
        present = newPresent
        presentPercentage = newPresentPercentage}

let toPercentage f =
    System.Math.Round (f * 100., 1)

[<EntryPoint>]
let main argv =
    let sessionId = argv.[0]
    let exists =
        getSessions ()
        |> (fun o -> o.SesjonerListes)
        |> Array.exists (fun s -> s.Id = sessionId)
    if not exists
    then
        1
    else
        getVotesFromStortingetApi sessionId
        |> Array.map (fun i -> getVote i.VoteringId)
        |> Array.concat
        |> Array.groupBy (fun v -> v.id)
        |> Array.map (fun (_, votes) -> votes |> Array.fold folder (fromVote votes.[0]))
        |> Array.sortBy (fun v -> v.presentPercentage)
        |> Array.map (fun v -> VoteStatistics.Row (v.name, v.party, v.total, v.present, toPercentage v.presentPercentage))
        |> fun rows -> new VoteStatistics (rows)
        |> fun statistics -> printfn $"%s{statistics.SaveToString ()}"
        0 // return an integer exit code