//--- Snippets from http://blogs.msdn.com/b/chrsmith/archive/2008/09/12/scripting-in-f.aspx

/// Get all files under a given folder
open System.IO

let rec allFilesUnder baseFolder = 
    seq {
        yield! Directory.GetFiles(baseFolder)
        for subDir in Directory.GetDirectories(baseFolder) do
            yield! allFilesUnder subDir 
        }
    
/// Active Pattern for determining file extension
let (|EndsWith|_|) extension (file : string) = 
    if file.EndsWith(extension) 
    then Some() 
    else None

//--- ConvertLine Functions

let unQuote        : string -> string = 
    fun s -> s.Trim([|'"'|])

let commasToPoints : string -> string = 
    fun s -> s.Replace(',', '.')

let (|Negative|Positive|) (c:char) = if c = '-' then Negative else Positive

let asFlows        : string -> string * string = 
    fun s -> 
        let flow = s.[1..]
        match s.[0] with
            | Negative -> (flow, "")
            | Positive -> ("" , flow)
        

//--- ConvertLines Functions

let convertLine : string -> string = 
    fun s -> 
        let ss = 
            s.Split [|';'|]

        let date = 
            ss.[0]

        let payee =
            ss.[1] 
            |> unQuote
            
        let (category,memo) = 
            ("","")

        let (outflow,inflow) = 
            ss.[2] 
            |> commasToPoints 
            |> asFlows
            
        String.concat "," [ date ; payee ; category ; memo ; outflow ; inflow ]

//--- Main Funtions

let getFileName  : string []  -> string     = 
    fun argv ->
        if 
            argv.Length <> 1 
        then
            failwith "Usage: AL2YNAB <filename>"
        else
            argv.[0]

let readLines    : string     -> string seq = 
    File.ReadLines

let skipHeader   : string seq -> string seq = 
    Seq.skip 1

let convertLines : string seq -> string seq = 
    Seq.map convertLine

let toText       : string seq -> string     = 
    String.concat "\n"

let addHeader    : string     -> string     = 
    (+) "Date,Payee,Category,Memo,Outflow,Inflow\n"

let writeToFile  : string     -> string     -> Unit = 
    fun filename text -> File.WriteAllText (filename,text)

let convertFile : string -> unit =
    fun file -> 
        let name = Path.ChangeExtension(file, ".ynab.csv")

        file
        |> readLines
        |> skipHeader
        |> convertLines
        |> toText
        |> addHeader
        |> writeToFile name

let getCSVs : string seq -> string seq = 
    Seq.filter 
        (function 
            | EndsWith ".ynab.csv" _ -> false
            | EndsWith ".csv" _      -> true
            | _                      -> false
        )

let convertFiles = 
    Seq.iter convertFile


let givenCSVs = getCSVs <|  System.Environment.GetCommandLineArgs ()

[<EntryPoint>]
let main argv = 
    
    let givenCSVs = getCSVs argv

    if
        Seq.isEmpty givenCSVs
    then
        allFilesUnder System.Environment.CurrentDirectory |> getCSVs
    else
        givenCSVs
    |> convertFiles

    0 // return an integer exit code
