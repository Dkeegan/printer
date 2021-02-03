(* printer definitions DO NOT MODIFY THESE *)
type printable = B of bool   | U |
                 S of string | L of int list | P of printable * printable

let ex0 = B true
let ex1 = S "Schrute bucks"
let ex2 = U
let ex3 = L [1; 8; 5; 0; 3]
let ex4 = P (P (U, P (P (L [1; -5; 13], U), P (L [0], B true))), S "Hello")
let ex5 = P (P (B false, P (P (L [-21; 53; 12], S "c"), P (L [0], B false))), S "Hello")
let ex6 = P (P (U, P (P (L [15; -15; 213], S "c"), P (P (U, U), S "false"))), S "Hello")
let ex7 = P (S "Stanley nickels", L [1000000])

(* Your code begins here *)


let rec count_u (p: printable) : int = 
    match p with 
    | B a -> 0
    | S q -> 0
    | L i -> 0
    | U   -> 1
    | P (j,x) -> count_u (j) + count_u(x)



let rec global_or (p : printable) : bool option = 
    match p with 

    | B myBool -> Some myBool
    | S n1 -> None
    | L n2 -> None
    | U  -> None
    | P (j,x) -> 
        (match global_or(j) with 
        | Some myBool -> 
            (match global_or(x) with
            | Some nextBool -> Some (myBool || nextBool)
            | None -> Some myBool
            )
        | None -> 
             global_or(x)     
            )
            
let rec f_on_int_list (f : int-> int) (p : printable) : printable = 

    match p with 

    | B myBool -> B myBool
    | S string -> S string
    | U        -> U
    | L intList-> L (List.map f (intList)) 
    | P (a,b)  -> P (f_on_int_list f(a) , f_on_int_list f(b))
        



let rec sum_all_ints (p : printable) : int option = 
    match p with

    | B myBool -> None
    | S string -> None
    | U        -> None
    | L []     -> None
    | L intList-> Some (List.fold_left (+) 0 (intList)) 
    
    | P (a,b)  -> 
        (match sum_all_ints(a) with 
          | Some intList -> 
              (match sum_all_ints(b) with
              | Some nextInt -> Some (intList + nextInt)
              | None -> Some intList
                )
           | None -> 
               sum_all_ints(b)     
               )




let rec tostring (p : printable) : string = 


    let rec string_of_intlist ( l : int list) : string = 
        match l with
            | [] -> ""
            | head::tail -> 
                string_of_int (head)^string_of_intlist (tail)
    in
    match p with 

    | B myBool -> 
        if myBool == true then "true"
        else "false"
    | U        -> "U"
    | L intList-> string_of_intlist (intList)
    | S newString -> newString
    | P(a,b)   -> (tostring (a)^tostring(b))
(*
folding
*)
