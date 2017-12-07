#load "./paket-files/eiriktsarpalis/TypeShape/src/TypeShape/TypeShape.fs"
open TypeShape
open System

type EnvParseResult<'T> =
| Success of 'T
| BadValue of (string * string)
| NotFound of string
| NotSupported of string

// string -> string option
let getEnv name =
  let v = Environment.GetEnvironmentVariable name
  if v = null then None else Some v

// (string -> bool * 'a) -> name ->  EnvParseResult<'a>
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


// string -> EnvParseResult<'T>
let parsePrimitive<'T> (envVarName : string) : EnvParseResult<'T> =
  let wrap(p : string -> 'a) = 
    envVarName
    |> unbox<string -> EnvParseResult<'T>> p 
    
  match shapeof<'T> with
  | Shape.Int32 -> wrap parseInt
  | Shape.String -> wrap parseString
  | Shape.Bool -> wrap parseBool
  | _ -> NotSupported "unknown target type"