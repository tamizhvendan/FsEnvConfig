#load "./paket-files/eiriktsarpalis/TypeShape/src/TypeShape/TypeShape.fs"
open TypeShape
open System
open System.Text.RegularExpressions

type EnvVarParseResult<'T> =
| Success of 'T
| BadValue of (string * string)
| NotFound of string
| NotSupported of string

// string -> string option
let getEnv name =
  let v = Environment.GetEnvironmentVariable name
  if v = null then None else Some v

// (string -> bool * 'a) -> name ->  EnvVarParseResult<'a>
let tryParseWith tryParseFunc name = 
  match getEnv name with
  | None -> NotFound name
  | Some value ->
    match tryParseFunc value with
    | true, v -> Success v
    | _ -> BadValue (name, value)


let parseInt = tryParseWith Int32.TryParse
let parseBool = tryParseWith Boolean.TryParse
let parseString = tryParseWith (fun s -> (true,s))


// string -> EnvVarParseResult<'T>
let parsePrimitive<'T> (envVarName : string) : EnvVarParseResult<'T> =
  let wrap(p : string -> 'a) = 
    envVarName
    |> unbox<string -> EnvVarParseResult<'T>> p 
    
  match shapeof<'T> with
  | Shape.Int32 -> wrap parseInt
  | Shape.String -> wrap parseString
  | Shape.Bool -> wrap parseBool
  | _ -> NotSupported "unknown target type"


let envVarNameRegEx = 
  Regex("([^A-Z]+|[A-Z][^A-Z]+|[A-Z]+)", RegexOptions.Compiled)

let canonicalizeEnvVarName name =
  let subStrings =
    envVarNameRegEx.Matches name
    |> Seq.cast
    |> Seq.map (fun (m : Match) -> m.Value.ToUpperInvariant())
    |> Seq.toArray
  String.Join("_", subStrings)
  
let parseRecord<'T> () =
  match shapeof<'T> with
  | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) -> 
    shape.Fields
    |> Seq.iter (fun f -> 
      let envVarName = canonicalizeEnvVarName f.Label
      printfn "%s, %s" envVarName f.Member.Type.Name)
  | _ -> failwith "not supported"

type Config = {
  ConnectionString : string
  Port : int
  EnableDebug : bool
  Environment : string
}