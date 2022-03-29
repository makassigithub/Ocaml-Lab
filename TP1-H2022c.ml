(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 15997                     *)
(* Corrigé TP1 HIVER 2022.                                           *)
(* Implanter un système d'indicateurs de développement               *)
(* en utilisant les données ouvertes de la banque mondiale           *)
(*********************************************************************)

#load "str.cma";;  (* Charger le module Str *)
#load "unix.cma";; (* Charger le module Unix *)

(* Charger la signature du système d'indicateurs *)
 #use "TP1-SIG-H2022.mli";;

(********************************************************************)
(* Implantation du système en utilisant un map,                     *)
(* un map, les listes et les enregistrements                 	    *)
(********************************************************************)

(* Module permettant d'utiliser un map dont les clés sont des paires de chaînes de caractères *)
module PaireCles =
    struct
       type t = string * string
       (* Les clés dans le map doivent être ordonnées (donc comparables) *)
       let compare (x0,y0) (x1,y1) =
           match String.compare x0 x1 with
             | 0 -> String.compare y0 y1
             | c -> c
     end

(* Map utilisant un arbre binaire de recherche *)
module IndicateursMap = Map.Make(PaireCles);;

(* Module du TP *)

module SysIndicateurs: SYSINDICATEURS = struct

  open List
  open Str

(* *****************************************************************)
(* Déclarations d'exceptions et de types *)
(* *****************************************************************)

  exception Err of string

  type indicateur = {
      nom_pays : string;
      code_pays : string;
      nom_indicateur : string;
      code_indicateur : string;
      valeurs_annees : (int*float) list;
    }

  type indicateurspays = indicateur list

(******************************************************************)
(* Fonctions fournies (vous pouvez en ajouter au besoin ...)      *)
(* ****************************************************************)

(* Fonctions manipulant les listes, les chaînes de caractères et/ou les fichiers *)

  (* appartient : 'a -> 'a list -> bool                   *)
  (* Retourner si un élément existe ou non dans une liste *)

  let appartient e l = exists (fun x -> x = e) l

  (* enlever : 'a -> 'a list -> 'a list *)
  (* Enlever un élément dans une liste  *)

  let enlever e l =
    let (l1, l2) = partition (fun x -> x = e) l
    in l2

  (* remplacer : 'a -> 'a -> 'a list -> 'a list       *)
  (* Remplacer un élément par un autre dans une liste *)

  let remplacer e e' l =
    map (fun x -> (if (x = e) then e' else x)) l

  (* uniques : string list -> string list                         *)
  (* Retourner une liste ne contenant que des éléments uniques    *)
  (* Les chaînes vides sont également enlevées de la liste        *)
  (* ainsi que les espaces inutiles avant et/ou après les chaînes *)

  let uniques liste =
    let res = ref [] in
    let rec fct l = match l with
     | [] -> !res
     | x::xs -> if (not (mem x !res) && (x <> "")) then res := (!res)@[String.trim x]; fct xs
    in fct liste

  (* decouper_chaine : string -> string -> string list                          *)
  (* Retourner une liste en découpant une chaîne selon un séparateur (p.ex "|") *)

  let decouper_chaine chaine separateur = split (regexp separateur) chaine

  (* timeRun : ('a -> 'b) -> 'a -> 'b * float                                     *)
  (* Permet  d'estimer la durée d'exécution d'une fonction passée en argument;    *)
  (* Elle prend en argument la fonction à évaluer et un paramètre, et retourne le *)
  (* résultat de cette application ainsi que la durée de cette application        *)

  let timeRun f x =
    let	time1 = Unix.gettimeofday() in
    let r = f x in
    let time2 = Unix.gettimeofday() in
    (r,time2 -. time1)

  (* read_line : in_channel -> string                       *)
  (* Permet de lire une ligne dans  fichier                 *)
  (* Elle retourne une chaîne vide si le fichier est terminé *)

   let lire_ligne ic =
     try
       input_line ic (* Lire une ligne *)
     with End_of_file -> ""

   (* lire_fichier : in_channel -> string -> string list list                     *)
   (* Lire un fichier CSV et retourne une lite de listes de chaînes de caractères *)
   (* en spécifiant le séparateur qu'il faut utiliser pour délimiter les chaînes  *)

   let rec lire_fichier (flux:in_channel) (separateur:string) =
       let ligne =lire_ligne flux in
       match ligne with
	 | "" -> []
	 | s -> (decouper_chaine (String.trim s) separateur)::(lire_fichier flux separateur)

(* Fonctions manipulant les indicateurs du système dans un map *)

   (* creer_indicateur : string list -> indicateur                       *)
   (* Retourner un indicateur selon une liste de chaînes de caractères   *)

  let creer_indicateur (lch:string list) =
     if (length lch) < 4 then raise (Err "La longueur de la liste est incorrecte") else
     {
      nom_pays = nth lch 0;
      code_pays = nth lch 1;
      nom_indicateur = nth lch 2;
      code_indicateur = nth lch 3;
      valeurs_annees = let rl = tl(tl(tl(tl lch))) in
                       mapi (fun i x -> if (x="") then (i+1960,-1.0) else (i+1960,float_of_string x)) rl 
     }

   (* Permettant d'instancier un map (une référence) ainsi que les types de ses données *)
   (* Chaque clè (code_pays * code_indicateur) va être associée à un indicateur         *)

   let m = ref (IndicateursMap.empty)
   let _ = m := IndicateursMap.add ("","") (creer_indicateur ["";"";"";"";""]) !m; 
          m := IndicateursMap.remove ("","") !m

   (* ajouter_indicateur : string list -> unit       *)
   (* Permet d'ajouter un indicateur dans le map     *)
   (* en utilisant une liste de chaînes de caractères *)

   let ajouter_indicateur (lch: string list) =
       m := IndicateursMap.add (nth lch 1,nth lch 3) (creer_indicateur lch) !m

   (* ajouter_liste_indicateurs : string list list -> unit      *)
   (* Permet d'ajouter une liste d'indicateurs                  *)
   (* en utilisant une liste de listes de chaînes de caractères *)

   let ajouter_liste_indicateurs (llch: string list list) =
       iter (fun lch -> ajouter_indicateur lch) llch

   (* charger_donnees : string -> unit          *)
   (* Permet de charger les données dans le map *)

   let charger_donnees (fichier:string) =
       let ic =  try open_in fichier with _ -> raise (Err "Fichier inacessible") in
       let _ = input_line ic in (* ignorer la premiere ligne *)
       let liste_lignes = lire_fichier ic ";" in
       close_in ic; m := IndicateursMap.empty; ajouter_liste_indicateurs liste_lignes

   (* retourner_donnees : unit -> indicateurspays list *)
   (* Permet de retourner les données sous la forme de listes à partir du map *)

   let retourner_donnees (): indicateurspays list =
       let li = IndicateursMap.bindings !m in
       let lp = uniques(map (fun (k,i) -> fst k) li) in
       map (fun p -> IndicateursMap.fold (fun _ i acc -> i::acc) 
           (IndicateursMap.filter (fun (x,y) ind -> x = p) !m) []) lp
       
   (* indicateur_existe : indicateur -> bool         *)
   (* Retourner si un indicateur existe dans le map  *)

   let indicateur_existe (ind:indicateur)  =
       IndicateursMap.exists (fun k d -> d = ind) !m

   (* retourner_nbr_indicateurs : unit -> int *)
   (* Retourner le nombre d'indicateurs dans le map *)

   let retourner_nbr_indicateurs () =
       IndicateursMap.cardinal !m

(******************************************************************)
(* Fonctions à implanter				          *)
(* ****************************************************************)

   (* retourner_indicateur : string * string -> indicateur *)
   (* Permet de retourner un indicateur se trouvant dans le map *)
   (* lance l'exeption Notfound si l'indicateur n'existe pas *)

   let retourner_indicateur (cle:string*string) =
      IndicateursMap.find cle !m

   (* supprimer_indicateur : string * string -> unit *)
   (* Permet de supprimer un indicateur dans le map (ne doit rien faire si l'indicateur n'existe pas) *)
   let supprimer_indicateur (cle:string*string) =
      m := IndicateursMap.remove cle !m

   (* supprimer_liste_indicateurs : (string * string) list -> unit *)
   (* Permet de supprimer une liste d'indicateurs dans le map *)

   let supprimer_liste_indicateurs (lcles:(string*string) list) =
      iter (fun cle -> supprimer_indicateur cle) lcles

   (* afficher_indicateur : indicateur -> unit                     *)
   (* Permet d'afficher un indicateur selon un certain formatage   *)

   let afficher_indicateur (ind:indicateur) =
     print_string ("Code de l'indicateur: " ^ ind.code_indicateur ^ ".\n");
     print_string ("Nom de l'indicateur: " ^ ind.nom_indicateur ^ ".\n");
     print_string ("Code du pays: " ^ ind.code_pays ^ ".\n");
     print_string ("Nom du pays: " ^ ind.nom_pays ^ ".\n")


   (* val afficher_indicateurspays : indicateurspays -> unit *)
   (* Permet d'afficher les indicateurs d'un pays            *)

   let afficher_indicateurspays (ip:indicateurspays) = 
      iter (fun ind -> afficher_indicateur ind; print_newline()) ip

   (* afficher_valeur_indicateur : indicateur -> int -> unit *)
   (* Permet d'afficher la valeur d'un indicateur pour une année donnée  *)
   (* Doit afficher: "donnee manquante" si la valeur n'existe pas (= -1.0) *)

   let afficher_valeur_indicateur (ind:indicateur)(annee:int)=
     print_string ("Valeur pour l'annee " ^ (string_of_int annee ) ^ ": " 
                   ^ (let v = (assoc annee ind.valeurs_annees) in 
                      if (v = -1.0) then "Donnee manquante." else (string_of_float v)) ^ "\n")

   (* retourner_indicateurs_pays : string -> indicateurspays *)
   (* Permet de retourner les données d'un pays sous la forme *)
   (* de liste d'indicateurs à partir du map *)

   let retourner_indicateurs_pays (cp:string): indicateurspays =
       IndicateursMap.fold (fun _ i acc -> acc @ [i]) 
           (IndicateursMap.filter (fun (x,y) ind -> x = cp) !m) []

   (* lancer_systeme_indicateurs : string -> unit                                            *)
   (* Lancer le système d'indicateurs afin de trouver les indicateurs qui nous intéressent *)
   
    let lancer_systeme_indicateurs (fichier:string) =
       print_string "Outil de recherche d'indicateurs\n";
       print_string "Chargement des donnees en cours ...\n";
       flush stdout;
       let _,t = timeRun charger_donnees fichier in
       print_string ("Chargement termine dans un temps de: " ^ (string_of_float t) ^ " secondes\n");
       print_string "Veuillez entrer le code du pays qui vous interesse:\n";
       flush stdout;
       let pch = read_line () in
       let lip = retourner_indicateurs_pays pch in
       if (lip = []) then print_string "Aucune donnee trouvee, veuillez verifier le code du pays" else
       let nb = string_of_int (length lip) in
       print_string ("Nombre d'indicateurs trouves pour ce pays: " ^ nb  ^ "\n");
       print_string ("Veuillez entrer le numero de l'indicateur dont vous voulez afficher (de 1 a " ^ nb ^ ")\n");
       ignore (read_line ());
       flush stdout;
       let i = read_int () in
       let ind = nth lip (i-1) in
       print_string "Voici les informations concernant cet indicateur:\n";      
       afficher_indicateur ind;
       print_string "Veuillez entrer l'annee de la valeur de cet indicateur dont vous voulez afficher:\n";
       ignore (read_line ());
       flush stdout;
       let annee = read_int () in
       print_string "Voici la valeur demandee si elle existe:\n";
       afficher_valeur_indicateur ind annee;
       print_string "\nMerci est au revoir!"

end
