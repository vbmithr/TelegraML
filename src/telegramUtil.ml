open Yojson.Safe

exception ApiException of string

let (>>=) x f = match x with
  | Some x -> f x
  | None -> None

let (<$>) f x = match x with
  | Some x -> Some (f x)
  | None -> None

let the_string = function
  | `String string -> string
  | _ -> raise (ApiException "Type assertion failed!")

let this_string x = `String x

let the_int = function
  | `Int int -> int
  | _ -> raise (ApiException "Type assertion failed!")

let this_int x = `Int x

let the_bool = function
  | `Bool bool -> bool
  | _ -> raise (ApiException "Type assertion failed!")

let this_bool x = `Bool x

let the_float = function
  | `Float float -> float
  | _ -> raise (ApiException "Type assertion failed!")

let this_float x = `Float x

let the_list = function
  | `List list -> list
  | _ -> raise (ApiException "Type assertion failed!")

let this_list xs = `List xs

let the_assoc = function
  | `Assoc assoc -> assoc
  | _ -> raise (ApiException "Type assertion failed!")

let this_assoc x = `Assoc x

let (+?) xs = function
  | (_, None) -> xs
  | (name, Some y) -> xs @ [name, y]
