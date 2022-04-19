(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 15997                     *)
(* Fonctions, modules et librairies utiles pour le TP2               *)
(* Implanter un syst�me d'indicateurs de d�veloppement               *)
(* en utilisant les donn�es ouvertes de la banque mondiale           *)
(*********************************************************************)

#load "unix.cma";; (* Charger le module unix *)
#load "str.cma";;  (* Charger le module Str  *)
#directory "/home/etudiant/.opam/4.08.0/lib/labltk";;
#load "labltk.cma";;  (* Charger le module labltk  *)

(* Module permettant d'utiliser un map dont les cl�s sont des paires de cha�nes de caract�res *)
module PaireCles =
    struct
       type t = string * string
       (* Les cl�s dans le map doivent �tre ordonn�es (donc comparables) *)
       let compare (x0,y0) (x1,y1) =
           match String.compare x0 x1 with
             | 0 -> String.compare y0 y1
             | c -> c
     end

(* Map utilisant un arbre binaire de recherche *)
module IndicateursMap = Map.Make(PaireCles);;

open List
open Str
open Tk

(******************************************************************)
(* Fonctions fournies (vous pouvez en ajouter au besoin ...)      *)
(* ****************************************************************)

(* appartient : 'a -> 'a list -> bool                   *)
(* Retourner si un �l�ment existe ou non dans une liste *)

let appartient e l = exists (fun x -> x = e) l

(* enlever : 'a -> 'a list -> 'a list *)
(* Enlever un �l�ment dans une liste  *)

let enlever e l =
  let (l1, l2) = partition (fun x -> x = e) l
  in l2

(* remplacer : 'a -> 'a -> 'a list -> 'a list       *)
(* Remplacer un �l�ment par un autre dans une liste *)

let remplacer e e' l =
  map (fun x -> (if (x = e) then e' else x)) l

(* uniques : string list -> string list                         *)
(* Retourner une liste ne contenant que des �l�ments uniques    *)
(* Les cha�nes vides sont �galement enlev�es de la liste        *)
(* ainsi que les espaces inutiles avant et/ou apr�s les cha�nes *)

let uniques liste =
  let ltrim = map (fun ch -> String.trim ch) liste in
  let res = ref [] in
  let rec fct l = match l with
   | [] -> !res
   | x::xs -> if (not (mem x !res) && (x <> "")) then res := (!res)@[x]; fct xs
  in fct ltrim

(* decouper_chaine : string -> string -> string list                          *)
(* Retourner une liste en d�coupant une cha�ne selon un s�parateur (p.ex "|") *)

let decouper_chaine chaine separateur = split (regexp separateur) chaine

(* timeRun : ('a -> 'b) -> 'a -> 'b * float                                     *)
(* Permet  d'estimer la dur�e d'ex�cution d'une fonction pass�e en argument;    *)
(* Elle prend en argument la fonction � �valuer et un param�tre, et retourne le *)
(* r�sultat de cette application ainsi que la dur�e de cette application        *)

let timeRun f x =
  let time1 = Unix.gettimeofday() in
  let r = f x in
  let time2 = Unix.gettimeofday() in
  (r,time2 -. time1)

(* read_line : in_channel -> string                        *)
(* Permet de lire une ligne dans un fichier                *)
(* Elle retourne une cha�ne vide si le fichier est termin� *)

let lire_ligne ic =
   try
     input_line ic (* Lire une ligne *)
   with End_of_file -> ""

(* lire_fichier : in_channel -> string -> string list list                     *)
(* Lire un fichier CSV et retourne une lite de listes de cha�nes de caract�res *)
(* en sp�cifiant le s�parateur qu'il faut utiliser pour d�limiter les cha�nes  *)

let rec lire_fichier (flux:in_channel) (separateur:string) =
   let ligne =lire_ligne flux in
   match ligne with
    | "" -> []
    | s -> (decouper_chaine (String.trim s) separateur)::(lire_fichier flux separateur)

(* Cette fonction servira a trier les valeurs_annee de maniere decroissante en fonction de l'annee *)
let sort_tuple_desc (tuples: (int*float) list) =
    sort_uniq (fun (x1,y1) (x2,y2) -> compare x2 x1) tuples;