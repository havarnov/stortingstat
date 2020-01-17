module Stortingsskam.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.Extensions.Hosting
open System.Threading.Tasks
open System.Threading

open FSharp.Data

open FSharpx.Control

open Microsoft.Data.Sqlite

open FSharp.Control.Tasks.V2.ContextInsensitive

type Saker = XmlProvider<"https://data.stortinget.no/eksport/saker?sesjonid=2018-2019">
type Sak = XmlProvider<"https://data.stortinget.no/eksport/sak?sakid=47163">
type Votering = XmlProvider<"https://data.stortinget.no/eksport/voteringer?sakid=63033">
type VotingResult = XmlProvider<"https://data.stortinget.no/eksport/voteringsresultat?voteringid=7523">

let createsql = """
CREATE TABLE votes (
    voteId INT NOT NULL,
    voteDate TEXT NOT NULL,
    repId TEXT NOT NULL,
    name TEXT NOT NULL,
    party TEXT NOT NULL,
    vote TEXT NOT NULL,
    varaId TEXT NULL,
    varaName TEXT NULL,
    UNIQUE (voteId, repId)
);
"""

let getVotingResult votingId = async {
    return! VotingResult.AsyncLoad(sprintf "https://data.stortinget.no/eksport/voteringsresultat?voteringid=%d" votingId)
}

let getVotesFromStortingetApi sessionId = async {
    try
        let! saker =
            Saker.AsyncLoad(
                sprintf "https://data.stortinget.no/eksport/saker?sesjonid=%s" sessionId) // 2018-2019");
        let! votes =
            saker.SakerListes
            |> Array.filter (fun s -> s.Status = "behandlet")
            |> Array.take 10
            |> Array.map
                (fun s -> async {
                    printfn "Fetching votes for case with id = %d" s.Id
                    let! vortering = Votering.AsyncLoad(sprintf "https://data.stortinget.no/eksport/voteringer?sakid=%d" s.Id)
                    let res =
                        vortering.SakVoteringListes
                        |> Array.sortByDescending (fun v -> v.VoteringTid)
                    return res
                })
            |> Async.ParallelWithThrottle 10
            |> Async.map Array.concat
            |> Async.map (Array.filter (fun i -> i.AntallFor <> -1))

        return votes
    with
    | e ->
        printfn "Error: %s" e.Message
        return [||]
}

let getAndStoreVoteStats (conn: SqliteConnection) = async {

    let currentYear = DateTime.UtcNow.Year
    let lastYear = currentYear - 1
    let nextYear = currentYear + 1

    let! votes = 
        [|
            (sprintf "%d-%d" lastYear currentYear)
            (sprintf "%d-%d" currentYear nextYear)
        |]
        |> Array.map getVotesFromStortingetApi
        |> Async.ParallelWithThrottle 1
        |> Async.map (Array.concat)

    let! _ =
        votes
        |> Array.map (fun i -> async {
                try
                printfn "Fetching vote results for vote with id = %d" i.VoteringId
                let! res = getVotingResult i.VoteringId
                for voteRes in res.VoteringsresultatListe.RepresentantVoteringsresultats do
                    let vote = voteRes.Votering
                    let repId = voteRes.Representant.Id
                    let name = sprintf "%s, %s" voteRes.Representant.Etternavn voteRes.Representant.Fornavn
                    let party = voteRes.Representant.Parti
                    let (varaId, varaName) =
                        if voteRes.Representant.VaraRepresentant then
                            (voteRes.VaraFor.Id.Value, sprintf "%s, %s" voteRes.VaraFor.Etternavn.Value voteRes.VaraFor.Fornavn.Value)
                        else
                            (null, null)
                    use cmd = conn.CreateCommand()
                    cmd.CommandText <- """
                        INSERT OR IGNORE
                        INTO votes (
                            voteId,
                            voteDate,
                            repId,
                            name,
                            party,
                            vote,
                            varaId,
                            varaName
                        )
                        VALUES (
                            @voteId,
                            @voteDate,
                            @repId,
                            @name,
                            @party,
                            @vote,
                            @varaId,
                            @varaName
                        );
                    """
                    cmd.Parameters.AddWithValue("@voteId", i.VoteringId) |> ignore

                    let norwegianTz = TimeZoneInfo.FindSystemTimeZoneById("Europe/Oslo");
                    let d =
                        DateTime (
                            i.VoteringTid.Year,
                            i.VoteringTid.Month,
                            i.VoteringTid.Day,
                            i.VoteringTid.Hour,
                            i.VoteringTid.Minute,
                            i.VoteringTid.Second,
                            DateTimeKind.Unspecified)
                    let d = TimeZoneInfo.ConvertTimeToUtc(d, norwegianTz)
                    cmd.Parameters.AddWithValue("@voteDate", d.ToString("yyyy-MM-ddTHH:mm:ss.fff")) |> ignore
                    cmd.Parameters.AddWithValue("@repId", repId) |> ignore
                    cmd.Parameters.AddWithValue("@name", name) |> ignore
                    cmd.Parameters.AddWithValue("@party", party.Navn) |> ignore
                    cmd.Parameters.AddWithValue("@vote", vote) |> ignore
                    if isNotNull varaId then
                        cmd.Parameters.AddWithValue("@varaId", varaId) |> ignore
                    else
                        cmd.Parameters.AddWithValue("@varaId", DBNull.Value) |> ignore
                    if isNotNull varaName then
                        cmd.Parameters.AddWithValue("@varaName", varaName) |> ignore
                    else
                        cmd.Parameters.AddWithValue("@varaName", DBNull.Value) |> ignore
                    let! _ = cmd.ExecuteNonQueryAsync() |> Async.AwaitTask
                    ()
                return ()
                with
                | e ->
                    printfn "error fetching or inserting vote stats: %s" e.Message
                    printfn "%s" e.StackTrace
            })
        |> Async.ParallelWithThrottle 10
    
    return ()
}


[<AbstractClass>]
type BackgroundService () =

    let mutable _executingTask = null
    let _stoppingCts = new CancellationTokenSource()

    abstract member Execute: CancellationToken -> Task

    interface IDisposable with
        member this.Dispose() =
            if isNotNull _stoppingCts then
                _stoppingCts.Dispose()

    interface IHostedService with
        member this.StartAsync ct =
            _executingTask <- (this.Execute _stoppingCts.Token)
            if _executingTask.IsCompleted then
                _executingTask
            else
                Task.CompletedTask

        member this.StopAsync ct =
            async {
                if isNull _executingTask then
                    return ()
                else
                    try
                        _stoppingCts.Cancel()
                    with
                        | e -> printfn "%s" e.Message

                    let! _ = Task.WhenAny(_executingTask, Task.Delay(Timeout.Infinite, ct)) |> Async.AwaitTask
                    return ()
            } |> Async.StartAsTask :> Task

type VotingsFetchService () =
    inherit BackgroundService()

    override this.Execute ct =
        async {
            try
            use connection = new SqliteConnection("Data Source=./data.db;")
            do! connection.OpenAsync() |> Async.AwaitTask
            while not ct.IsCancellationRequested do
                let! res = getAndStoreVoteStats connection
                do! Task.Delay(TimeSpan.FromHours 30.0, ct) |> Async.AwaitTask
            return ()
            with
            | e -> printfn "error: %s" e.Message
        } |> Async.StartAsTask :> Task

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open GiraffeViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "#stortingsskam" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let partial =
        h1 [] [ encodedText "#stortingsskam - toppliste over representanter som _ikke_ er til stede på voteringer (2018/2019)" ]

    let list (model: (string * string * int * int * float) seq) =
        let item name party total notPresent percentage =
            tr
                []
                [
                    td [] [encodedText name]
                    td [] [encodedText party]
                    td [] [encodedText (sprintf "%d" total)]
                    td [] [encodedText (sprintf "%d" notPresent)]
                    td [] [encodedText (sprintf "%.1f" (percentage * 100.0))]
                ]
        let header =
            tr
                []
                [
                    th [] [encodedText "navn"]
                    th [] [encodedText "parti"]
                    th [] [encodedText "total"]
                    th [] [encodedText "ikke tilstede"]
                    th [] [encodedText "%"]
                ]
        let items =
            model
            |> Seq.map (fun (n, p, t, np, pr) -> item n p t np pr)
            |> List.ofSeq
        let tableItems =
            List.append [header] items
        [
            partial
            table
                []
                tableItems
        ] |> layout

    let index () =
        [
            partial
            p [] [ encodedText "hello" ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let query = """
SELECT name,
       party,
       Count(vote) AS total,
       Count(CASE vote
               WHEN 'ikke_tilstede' THEN 1
               ELSE NULL
             END) AS notPresent,
       Count(CASE vote
               WHEN 'ikke_tilstede' THEN 1
               ELSE NULL
             END) * 1.0 / Count(vote) AS percentage
FROM  votes
WHERE voteDate BETWEEN @from and @to
GROUP  BY repid
ORDER  BY percentage DESC
LIMIT 100;
"""

let rec collector (reader: SqliteDataReader) (result: (string * string * int * int * float) seq) = task {
        let repname = reader.GetString (reader.GetOrdinal "name")
        let party = reader.GetString (reader.GetOrdinal "party")
        let tot = reader.GetInt32 (reader.GetOrdinal "total")
        let notPresent = reader.GetInt32 (reader.GetOrdinal "notPresent")
        let percentage = reader.GetDouble (reader.GetOrdinal "percentage")
        let result = Seq.append result [| (repname, party, tot, notPresent, percentage ) |]

        let! moreData = reader.ReadAsync()
        if moreData then
            return! collector reader result
        else
        return result
    }

let indexHandler (f: DateTime) (t: DateTime) : HttpHandler =
    fun n ctx -> task {
        let f =
            match ctx.TryGetQueryStringValue "from" with
            | Some v -> 
                match DateTime.TryParse(v) with
                | (true, f) -> f
                | _ -> f
            | None -> f
        let t =
            match ctx.TryGetQueryStringValue "to" with
            | Some v -> 
                match DateTime.TryParse(v) with
                | (true, t) -> t
                | _ -> t
            | None -> t
        use connection = new SqliteConnection("Data Source=./data.db;")
        do! connection.OpenAsync() |> Async.AwaitTask
        let cmd = connection.CreateCommand()
        cmd.CommandText <- query
        cmd.Parameters.AddWithValue("@from", f.ToString("yyyy-MM-ddTHH:mm:ss.fff")) |> ignore
        cmd.Parameters.AddWithValue("@to", t.ToString("yyyy-MM-ddTHH:mm:ss.fff")) |> ignore
        let! reader = cmd.ExecuteReaderAsync()
        let! cont = reader.ReadAsync()
        let! result = 
            if cont then
                task {
                    return! collector reader [||]
                }
            else
                task {
                    return Seq.empty
                }

        let view      = Views.list result
        return! htmlView view n ctx
    }

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> warbler (fun _ -> indexHandler (DateTime.UtcNow.AddYears -1) (DateTime.UtcNow))
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseHttpsRedirection()
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddHostedService<VotingsFetchService>() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l.Equals LogLevel.Error)
           .AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0