(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 15997                     *)
(* TP1 HIVER 2022. Date limite: Mardi 19 avril � 17h                 *)
(* Implanter un syst�me d'indicateurs de d�veloppement               *)
(* en utilisant les donn�es ouvertes de la banque mondiale           *)
(*********************************************************************)
(*********************************************************************)
(* �tudiant(e):                                                      *)
(* NOM: _______________________ PR�NOM:_____________________________ *)
(* MATRICULE: _________________ PROGRAMME: _________________________ *)
(*                                                                   *)
(*********************************************************************)

(* Chargement de modules, fonctions et librairies utiles pour le TP2 *)
#use "utiles.ml";;

(* Chargement de la signature du TP2 *)
#use "TP2-SIG-H2022.mli";;

(********************************************************************)
(* Implantation du syst�me en utilisant                             *)
(* la programmation orient�e objet                       	    *)
(********************************************************************)

(* Module du TP *)

module Tp2h22 : TP2H22 = struct

  (* Classes du TP *)

  class indicateur (lch:string list) =
    object(self)
      val nom_pays : string = nth lch 0
      val code_pays : string = nth lch 1
      val nom_indicateur : string = nth lch 2
      val valeurs_annees : (int * float) list =
        if (length lch) < 4 then failwith "La longueur de la liste est incorrecte"
        else let rl = tl(tl(tl(tl lch))) in
          mapi (fun i x -> if (x="") then (i+1960,-1.0) else (i+1960,float_of_string x)) rl
      val code_indicateur : string = nth lch 3

      method get_nom_pays = nom_pays
      method get_code_pays = code_pays
      method get_nom_indicateur = nom_indicateur
      method get_valeurs_annees = valeurs_annees
      method get_code_indicateur = code_indicateur

      (* M�thode � implanter *)

      (* afficher_indicateur : unit *)
      (* -- � IMPLANTER (6 PTS) ------------------------------------------*)
      (* @M�thode : afficher_indicateur : unit                            *)
      (* @Description : afficher un indicateur selon un certain formatage *)
      method afficher_indicateur = 
      (* Afficher les champs de l'indicateur*)
       print_string ("Code de l'indicateur: " ^ code_indicateur ^ ".\n");
       print_string ("Nom de l'indicateur: " ^ nom_indicateur ^ ".\n");
       print_string ("Code du pays: " ^ code_pays ^ ".\n");
       print_string ("Nom du pays: " ^ nom_pays ^ ".\n")
    end


  class sysindicateurs (od:string) (td:string) =
    object
	val origine_donnees : string = od
	val theme_donnees : string = td
	method get_origine_donnees = origine_donnees
	method get_theme_donnees = theme_donnees
    end


  class sysind_education (od:string) (td:string) =
    object(self)
      inherit sysindicateurs od td as parent
      val mutable map_indicateurs : indicateur IndicateursMap.t ref = ref (IndicateursMap.empty)
      method get_map_indicateurs = map_indicateurs
      method set_map_indicateurs (mi: indicateur IndicateursMap.t ref) = map_indicateurs <- mi

      method ajouter_indicateur (lch: string list) =
        map_indicateurs := IndicateursMap.add (nth lch 1,nth lch 3) (new indicateur lch) !map_indicateurs

      method ajouter_liste_indicateurs (llch:string list list) =
        iter (fun lch -> (self#ajouter_indicateur lch)) llch

      method charger_donnees (fichier:string) =
        let ic =  try open_in fichier with _ -> failwith "Fichier inacessible" in
        let _ = input_line ic in (* ignorer la premi�re ligne *)
        let liste_lignes = lire_fichier ic ";" in
        close_in ic; map_indicateurs := IndicateursMap.empty; self#ajouter_liste_indicateurs liste_lignes

      method retourner_donnees : indicateur list list =
        let li = IndicateursMap.bindings !map_indicateurs in
        let lp = uniques(map (fun (k,i) -> fst k) li) in
        map (fun p -> IndicateursMap.fold (fun _ i acc -> acc@[i])
           (IndicateursMap.filter (fun (x,y) ind -> x = p) !map_indicateurs) []) lp

      method indicateur_existe (ind:indicateur)  =
        IndicateursMap.exists (fun k d -> d = ind) !map_indicateurs

      method retourner_nbr_indicateurs =
        IndicateursMap.cardinal !map_indicateurs

      (* M�thodes � implanter *)

      (* -- � IMPLANTER (6 PTS) --------------------------------------------*)
      (* @M�thode : retourner_indicateur : string * string -> indicateur    *)
      (* @Description : retourner un indicateur se trouvant dans le map     *)

      method retourner_indicateur (cle:string*string) =
       (*Parcourir le map des indicateurs et retourner l'indicateur dont le code pays
       et et le code indicateur correspond a la clef *)
       IndicateursMap.find cle !map_indicateurs

      (* -- � IMPLANTER (6 PTS) -----------------------------------------*)
      (* @M�thode : supprimer_indicateur : string * string -> unit       *)
      (* @Description : supprimer un indicateur dans le map              *)

      method supprimer_indicateur (cle:string*string) =
       (*Parcourir le map des indicateurs et supprimer l'indicateur dont le code pays
       et et le code indicateur correspond a la clef *)
       map_indicateurs := IndicateursMap.remove cle !map_indicateurs

      (* -- � IMPLANTER (6 PTS) -------------------------------------------------*)
      (* @M�thode : supprimer_liste_indicateurs : (string * string) list -> unit *)
      (* @Description : supprimer une liste d'indicateurs dans le map            *)
      method supprimer_liste_indicateurs (lcles:(string*string) list) =
       (*Parcourir le map des indicateurs et supprimer l'indicateur dont le code pays
       et et le code indicateur correspond aux clef *)
       iter (fun cle -> self#supprimer_indicateur cle) lcles

      (* -- � IMPLANTER (6 PTS) ------------------------------------------*)
      (* @M�thode : afficher_indicateurspays : indicateur list -> unit    *)
      (* @Description : afficher les indicateurs d'un pays                *)

      method afficher_indicateurspays (ip:indicateur list) =
       (*Parcourir la list des indicateurs et afficher l'indicateurs dans la list *)
       iter (fun ind -> ind#afficher_indicateur; print_newline()) ip

      (* -- � IMPLANTER (7 PTS) -------------------------------------------------*)
      (* @M�thode : afficher_valeur_indicateur : indicateur -> int -> unit       *)
      (* @Description : afficher la valeur d'un indicateur pour une ann�e donn�e *)
      (* Doit afficher: "donnee manquante" si la valeur n'existe pas (= -1.0)    *)

      method afficher_valeur_indicateur (ind:indicateur)(annee:int)=
       print_string ("Valeur pour l'annee " ^ (string_of_int annee ) ^ ": " 
       (* Afficher la valeur d'un indicateur associé a l'annee dans la liste des valeurs*)
                   ^ (let v = (assoc annee ind#get_valeurs_annees) in 
                      if (v = -1.0) then "Donnee manquante." else (string_of_float v)) ^ "\n")

      (* -- � IMPLANTER (7 PTS) --------------------------------------------*)
      (* @M�thode : retourner_indicateurs_pays : string -> indicateur list  *)
      (* @Description : retourner les donn�es d'un pays sous la forme       *)
      (*                de liste d'indicateurs � partir du Map              *)

      method retourner_indicateurs_pays (cp:string): indicateur list =
      (* Parcourir le map et retourner une liste d'indicateur associees a un pays donne*)
       IndicateursMap.fold (fun _ i acc -> acc @ [i]) 
           (IndicateursMap.filter (fun (x,y) ind -> x = cp) !map_indicateurs) []

      initializer print_string ("Recherche dans " ^ (parent#get_origine_donnees) ^
				" pour le theme de " ^ (self#get_theme_donnees) ^ ".");
				print_newline();

    end

  class app_sysindicateurs (nf:string) (i:bool) =
    object(self)
      val nom_fichier = nf
      val interface = i

      method private retourner_chi (i:indicateur) =
        let s = "Code de l'indicateur: " ^ i#get_code_indicateur ^ ".\n" in
        let s = s ^ "Nom de l'indicateur: " ^ i#get_nom_indicateur ^ ".\n" in
        let s = s ^ "Code du pays: " ^ i#get_code_pays ^ ".\n" in
        let s = s ^ "Nom du pays: " ^ i#get_nom_pays ^ ".\n" in
        let s = s ^ "\nValeurs de l'indicateur selon les ann�es:\n\n" in
        let vi = i#get_valeurs_annees in
        let c = fold_left (fun accu paire -> (string_of_int (fst paire)) ^ ": "
                                             ^ (string_of_float (snd paire)) ^ "\n" ^ accu) "" vi in
        s ^ c

      (* M�thodes � implanter *)

      (* -- � IMPLANTER (6 PTS) ------------------------------------------------*)
      (* @M�thode : sauvegarder_indicateur : indicateur -> out_channel -> unit  *)
      (* @Description : �crire les informations d'un indicateur sur un flux     *)

      method sauvegarder_indicateur (i:indicateur) (flux:out_channel) =
      (* sauvegarder un indicateur dans un fichier a travers un flux ouvert*)
            let chaine = self#retourner_chi i in
            output_string flux chaine

      (* -- � IMPLANTER (15 PTS) -----------------------------------------------*)
      (* @M�thode : lancer_systeme_indicateurs : unit                           *)
      (* @Description : Lancer le syteme d'indicateurs                          *)

      method lancer_systeme_indicateurs =
          print_string "Outil de recherche d'indicateurs\n";
          print_string "Chargement des donnees en cours ...\n";
          flush stdout;
          (* Instancier un systeme d'indicateur pour charger les donnees *)
          let si = new sysind_education "" "" in
          let _,t = timeRun si#charger_donnees nom_fichier in
          print_string ("Chargement termine dans un temps de: " ^ (string_of_float t) ^ " secondes\n");
          print_string "Veuillez entrer le code du pays qui vous interesse:\n";
          flush stdout;
          (* Recuperer le code pays et rechercher les indicateurs associes*)
          let pch = read_line () in
          let lip = si#retourner_indicateurs_pays pch in
          if (lip = []) then print_string "Aucune donnee trouvee, veuillez verifier le code du pays" else
          let nb = string_of_int (length lip) in
          print_string ("Nombre d'indicateurs trouves pour ce pays: " ^ nb  ^ "\n");
          print_string ("Veuillez entrer le numero de l'indicateur dont vous voulez afficher (de 1 a " ^ nb ^ ")\n");
          ignore (read_line ());
          flush stdout;
          (* Recuperer la position d'un indicateur et l'enregistrer dans un fichier*)
          let i = read_int () in
          let ind = nth lip (i-1) in
          let flux = open_out "Resultats.txt" in 
            self#sauvegarder_indicateur ind flux;
            close_out flux;
            print_string "Veuillez consulter le fichier 'Resultats.txt' dans votre repertoire courant!";
            print_string "\nMerci est au revoir!"

      (* -- � IMPLANTER (25 PTS) -----------------------------------------------*)
      (* @M�thode : lancer_interface_sindicateurs : unit                        *)
      (* @Description : affiche une interface graphique                         *)

      method lancer_interface_sindicateurs =
	(* Instancier uns systeme d'indicateur et charger les donnees*)
	let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education" in
        print_string "Chargement des donnees en cours ...\n";
        flush stdout;
        let _,t = timeRun si#charger_donnees nom_fichier in
        print_string ("Chargement termine dans un temps de: " ^ (string_of_float t) ^ " secondes\n");
        flush stdout;

  let top = openTk () in                     (*  creer fenetre principale *)
	  Wm.title_set top "Systeme d'indicateurs";
    Wm.geometry_set top "680x580";
    let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'indicateurs" top in
    pack [l1];
		let input_indicateur_selectionne = Textvariable.create () in
		Textvariable.set input_indicateur_selectionne "";
		let input_pays_selectionne = Textvariable.create () in
		Textvariable.set input_pays_selectionne "";
		let text_var_resultat = Textvariable.create () in
		Textvariable.set text_var_resultat "";
		let list_pays = Listbox.create ~background:(`Color "#55aa99")
    	top in
		pack[list_pays]~side:`Top ~fill:`X;      (*  creation de liste box pour afficher les pays *)
		let _ = Listbox.insert
    ~index:`End
		~texts: (map(fun indic_list->
    let indic = (nth indic_list 0) in
      indic#get_code_pays ^ ": "^indic#get_nom_pays) si#retourner_donnees) list_pays in
		let list_indicateur = Listbox.create ~background:(`Color "#c0f6e0")
   	top in
		pack[list_indicateur]~side:`Top ~fill:`X;  (*  creation de liste box pour afficher des indicateurs *)
		let _ = Listbox.insert
    ~index:`End
		~texts:(map(fun indic -> 
    indic#get_code_indicateur ^ ": " ^indic#get_nom_indicateur)
      (nth si#retourner_donnees 0))
    list_indicateur in
		let label_pays_selectionne = Label.create ~text:"Pays selectionne" top in
		let valeur_pays_selectionne = Label.create ~background:(`Color "#55aa99") ~textvariable:input_pays_selectionne
    top in
    let label_indicteur_selectionne = Label.create ~text:"Indicateur selectionne" top in
		let valeur_indicteur_selectionne = Label.create ~background:(`Color "#c0f6e0") ~textvariable:input_indicateur_selectionne
    top in
		let afficher_pays = Button.create  (*  creation des buttons pour affcicher le pays , l'indicateur et le resultat *)
    	~text:"Afficher le pays"
   		~command:(fun () ->
      	try                                                          
          let n = match (List.hd (Listbox.curselection list_pays)) with
            | `Num y -> y
            | _ -> failwith "Erreur: Veuillez selectionner un pays"
          in

		        Textvariable.set input_pays_selectionne(List.nth (map(fun indic_list->
    let indic = (nth indic_list 0) in
      indic#get_code_pays ^ ": "^indic#get_nom_pays) si#retourner_donnees) n)
      	with _ -> (print_endline "pas de selection"; flush stdout)
	     ) top in
          let afficher_indicateur = Button.create
            ~text:"Afficher l'indicateur"
            ~command:(fun () ->
              try
          let n = match (List.hd (Listbox.curselection list_indicateur)) with
              | `Num y -> y
              | _ -> failwith "Erreur: Veuillez selectionner un indicateur"
		        in
          Textvariable.set input_indicateur_selectionne (List.nth (map(fun indic -> 
    indic#get_code_indicateur ^ ": " ^indic#get_nom_indicateur)
      (nth si#retourner_donnees 0)) n)
     	    with _ -> (print_endline "pas de selection"; flush stdout)
	     ) top in
		let afficher_resultats = Button.create
	    ~text:"Afficher le resultat"
	   	~command:(fun () ->
	  	let d = Toplevel.create top in  (*  creation de la pop-up pour afficher le resultat *)
	  	begin
        Wm.title_set d "Resultat de la recherche";
        Wm.geometry_set d "600x450";
        let scry = Scrollbar.create d in  
        let txt = Text.create ~width:20 ~height:25
          ~yscrollcommand:(Scrollbar.set scry)
          ~background:(`Color "#ACDF84")
          d in
        pack[txt]~side:`Top ~fill:`X;
        let nom_code_pays = (Textvariable.get input_pays_selectionne) in 
        let code_pays = nth (decouper_chaine nom_code_pays ":" ) 0 in
        let nom_code_indicateur =  (Textvariable.get input_indicateur_selectionne) in
        let code_indicateur = nth (decouper_chaine nom_code_indicateur ":") 0 in
        let indicateur = si#retourner_indicateur (code_pays,code_indicateur) in 
        Text.insert (`End,[])  ("Indicateur selectionnes:\n\n") txt;	 
        Text.insert (`End,[])  ("Code de l'indicateur: "^(indicateur#get_code_indicateur) ^ "\n")  txt;	 
        Text.insert (`End,[])  ("Nom de l'indicateur: "^(indicateur#get_nom_indicateur) ^ "\n") txt;
        Text.insert (`End,[])  ("Code du pays: "^(indicateur#get_code_pays) ^ "\n") txt;
        Text.insert (`End,[])  ("Nom du pays: "^(indicateur#get_nom_pays) ^ "\n\n") txt;
        Text.insert (`End,[])  ("Valeurs de l'indicateur selon les annees:\n\n") txt;
        iter (fun (x,y) -> Text.insert (`End,[])  (string_of_int x ^ ": " ^ string_of_float y ^ "\n") txt ) indicateur#get_valeurs_annees;                                  																	
		  end)
      top in
      (*  afficher les widgets dans la fenetre principale *)
		pack[label_pays_selectionne]~side:`Top;		
		pack[valeur_pays_selectionne]~side:`Top  ~fill:`X;
    pack[label_indicteur_selectionne]~side:`Top;
    pack[valeur_indicteur_selectionne]~side:`Top ~fill:`X;
    pack [afficher_pays;afficher_indicateur;afficher_resultats]~side:`Left ~expand:true;
        let _ = Printexc.print mainLoop () in
	print_endline "Merci et au revoir!\n"
      initializer if interface then self#lancer_interface_sindicateurs else self#lancer_systeme_indicateurs
    end
end
