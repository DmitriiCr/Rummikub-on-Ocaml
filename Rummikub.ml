(* Projet - Le jeu du Rummikub
********************************************************************
*  Etudiant : Crivoi Dmitrii                                    *
*  Groupe     : INF-2                                              *
********************************************************************)

(* petit Commentaire :  Les fonctions extraire extraction_suite , jouer_1_coup , jouer_1er_coup ne sont pas faites dans ce code *)
(* Tout les tests j'ai fait sur le site de tryocaml et ça va ecrire warning the match is not exhaustive à cause de Joker mais les matchs sont adapté pour un ou deux Joker dans la combinaison et donc si les arguments pour les fonctions 
 est_groupe est_suite sont compatible les fonctions travailleront correctement *) 
(* Pour certain fonctions il y aura pas d'assert car ils extraitent les cartes aux hasard *)  


(*type multielement *)
type nat = int ;; (* >=0*)
type 'a multielement = 'a * nat

type 'a multiensemble = 'a multielement list
(* Calcule le nombre total des element dans un reservoir de type generique a*)
(* Pour tester les fonction cardinal ,... , difference *) 
let ex_ens1 : 'a multiensemble = [(3,2);(4,1);(6,3);(5,1);(1,1);(9,1)]
and ex_ens2 : 'a multiensemble = [(1,2);(7,1);(6,1)]


let ex_ens = (3,2)::(4,1)::(6,3)::[] ;;

let cardinal (m:'a multiensemble):int =
	List.fold_left(fun acc (a,n) -> acc + n) 0 m
;;
assert (cardinal ex_ens2 = 4  ) ;;

let nbocc (a:'a) (mens : 'a multiensemble) : nat =
     (List.assoc a mens);; 

assert ( nbocc 6 ex_ens1 = 3) ;;

let  appartient (p:'a) (m:'a multiensemble) : bool =	
		List.exists (fun  (x,n) -> p=x ) m ;;

assert ( appartient 8 ex_ens1 = false );;
	

let inclus (mens1:'a multiensemble)(mens2:'a multiensemble):bool=
  List.for_all (fun (b,a) -> ( appartient b mens2 && a <= nbocc b mens2)) mens1
;;

assert ( inclus [(1,1)] ex_ens2 = true ) 	;;

let ajoute ( (e,n):'a multielement)(mens:'a multiensemble):'a multiensemble=
  if (appartient e mens)
  then
    List.map (fun (x,y) -> if (e = x) then (x,y+n)  else (x,y)) mens
  else (e,n)::mens
;; 		

assert (cardinal(ajoute (8,2) ex_ens1) = (cardinal ex_ens1)+2 ) ;;

let rec supprime (a,n:'a multielement) (m :'a multiensemble) : 'a multiensemble =
	if n < nbocc a m then
	match m with (*on veut pas le supprimer entierement on rajoute (a,nbocc a b-n *)
	[] -> failwith "liste peut pas etre vide"
	| (pr,k)::fin-> if pr=a then (pr,k-n)::fin else (pr,k) :: (supprime (a,n) fin )
	else List.filter (fun (pr,k)-> not(pr=a)) m
	;;
	
assert (cardinal (supprime (6,3) ex_ens1) = (cardinal ex_ens1)-3) ;;
	
let egaux (ens1:'a multiensemble)(ens2:'a multiensemble):bool=
  (inclus ens2 ens1) && (inclus ens1 ens2)
;;	
assert (egaux ex_ens1 ex_ens2 = false );;
assert (egaux ex_ens1 ex_ens1 = true );;

let rec intersection (l1:'a multiensemble) (l2:'a multiensemble) : 'a multiensemble = 
	let a = List.filter (fun (pr,k)-> (appartient pr l2) ) l1 in 
	( List.map (fun (pr,k) -> if nbocc pr l1 >= nbocc pr l2 then (pr , (nbocc pr l2)) else  (pr , (nbocc pr l1))) a );;

assert ( intersection ex_ens1 ex_ens2 = [(6,1);(1,1)] ) ;;

let difference (mens1 : 'a multiensemble) (mens2 : 'a multiensemble) : 'a multiensemble= 
   let a =  List.filter ( fun (pr,k) -> not(appartient pr mens2) || nbocc pr mens1 > nbocc pr mens2) mens1  in 
   
   (List.map( fun (pr,k)-> if appartient pr mens2 then (pr , (nbocc pr a) - (nbocc pr mens2)) else (pr,k+0)) a) ;;
   
assert ( difference ex_ens1 ex_ens2  = [(3, 2); (4, 1); (6, 2); (5, 1); (9, 1)]) ;;



		
		
(*Partie 6 ---------------------------------------------------------------------------*)	
(*6.1 Les Tuiles

Q4*)
type couleur = Bleu|Rouge|Jaune|Noir;;
type valeur = nat ;; (*restreint dans  [1;13] *) 
type tuile = Joker |T of valeur*couleur ;;

 assert ( difference [ (Joker ,1)  ; T(5,Bleu),3 ; T(4,Rouge),3] [ (Joker,1) ; T(5,Bleu),2 ; T(4,Rouge),4] = [ T(5,Bleu),1] ) ;;
(*6.2 	Combinaisons, tables et poses 
    Q5 ----------------------------------------------------------------------------------------------------------------*)
	
type combinaison = tuile list ;;
type table = combinaison list  ;;
type pose = combinaison list	;; 


(*6.3 Mains et pioches ----------------------------------------------------------------------------------------------------------------*)
type main = tuile multiensemble ;; (*tuile desormais sera 'a et nomb occurences est nat*)

type pioche = tuile multiensemble ;;  (*tuile desormais sera 'a et nomb occurences est nat*)

let ex_main : main = [  T(5,Noir),2 ;  T(2,Bleu),2 ; T(3,Bleu),2 ;(Joker,2);T(5,Rouge),2];;

let cst_PIOCHE_INIT : pioche = [ (Joker, 2) ;
T(1,Rouge), 2 ; T(2,Rouge), 2 ; T(3,Rouge), 2 ; T(4,Rouge), 2 ; T(5,Rouge), 2 ;
T(6,Rouge), 2 ; T(7,Rouge), 2 ; T(8,Rouge), 2 ; T(9,Rouge), 2 ; T(10,Rouge), 2 ;
T(11,Rouge), 2 ; T(12,Rouge), 2 ; T(13,Rouge), 2 ;
T(1,Bleu), 2 ; T(2,Bleu), 2 ; T(3,Bleu), 2 ; T(4,Bleu), 2 ; T(5,Bleu), 2 ;
T(6,Bleu), 2 ; T(7,Bleu), 2 ; T(8,Bleu), 2 ; T(9,Bleu), 2 ; T(10,Bleu), 2 ;
T(11,Bleu), 2 ; T(12,Bleu), 2 ; T(13,Bleu), 2 ;
T(1,Jaune), 2 ; T(2,Jaune), 2 ; T(3,Jaune), 2 ; T(4,Jaune), 2 ; T(5,Jaune), 2 ;
T(6,Jaune), 2 ; T(7,Jaune), 2 ; T(8,Jaune), 2 ; T(9,Jaune), 2 ; T(10,Jaune), 2 ;
T(11,Jaune), 2 ; T(12,Jaune), 2 ; T(13,Jaune), 2 ;
T(1,Noir), 2 ; T(2,Noir), 2 ; T(3,Noir), 2 ; T(4,Noir), 2 ; T(5,Noir), 2 ;
T(6,Noir), 2 ; T(7,Noir), 2 ; T(8,Noir), 2 ; T(9,Noir), 2 ; T(10,Noir), 2 ;
T(11,Noir), 2 ; T(12,Noir), 2 ; T(13,Noir), 2
]
	
let cst_PIOCHE_INIT2 : pioche = [ 
T(1,Rouge), 2 ; T(7,Rouge), 2 ; T(8,Rouge), 2 ; T(4,Rouge), 2 ; T(5,Rouge), 2 ;
T(6,Rouge), 2 ; T(7,Rouge), 2 ; T(8,Rouge), 2 ; T(9,Rouge), 2 ; T(10,Rouge), 2 ;
T(11,Rouge), 2 ; T(12,Rouge), 2 ; T(13,Rouge), 2 ;
T(1,Bleu), 2 ; T(2,Bleu), 2 ; T(3,Bleu), 2 ; T(4,Bleu), 2 ; T(5,Bleu), 2 ;
T(6,Bleu), 2 ; T(7,Bleu), 2 ; T(8,Bleu), 2 ; (Joker ,2); T(9,Bleu), 2 ; T(10,Bleu), 2 ;
T(11,Bleu), 2 ; T(12,Bleu), 2 ; T(13,Bleu), 2 ;
T(1,Jaune), 2 ; T(2,Jaune), 2 ; T(3,Jaune), 2 ; T(4,Jaune), 2 ; T(5,Jaune), 2 ;
T(6,Jaune), 2 ; T(7,Jaune), 2 ; T(8,Jaune), 2 ; T(9,Jaune), 2 ; T(10,Jaune), 2 ;
T(11,Jaune), 2 ; T(12,Jaune), 2 ; T(13,Jaune), 2 ;
T(1,Noir), 2 ; T(2,Noir), 2 ; T(3,Noir), 2 ; T(4,Noir), 2 ; T(5,Noir), 2 ;
T(6,Noir), 2 ; T(7,Noir), 2 ; T(8,Noir), 2 ; T(9,Noir), 2 ; T(10,Noir), 2 ;
T(11,Noir), 2 ; T(12,Noir), 2 ; T(13,Noir), 2
] ;;


   (* Q7  ----------------------------------------------------------------------------------------------------------------*)
	
	
let en_ordre (mens : tuile multiensemble) : tuile multiensemble =

	(*Je trie la liste en ordre croissant et apres je trie la liste par rapport aux couleurs*)
	let liste = List.sort compare mens in  
    
	List.sort (fun (tuile1:tuile*nat) (tuile2:tuile*nat)-> 
	match tuile1 , tuile2 with 
	(Joker,n1) ,(T(pr,col),n)-> (-1)
	|(Joker, _), (Joker, _)-> (-1)
	|(T (_, _), _), (Joker, _)-> (1) (*Joker en priorité *)
	|(T(pr1,couleur1),n1), (T(pr2,couleur2),n2)-> compare couleur1 couleur2) liste;; 

assert ( en_ordre ex_main = [(Joker, 2); (T (2, Bleu), 2); (T (3, Bleu), 2); (T (5, Rouge), 2);
 (T (5, Noir), 2)] ) ;;
(* 6.4 Les joueurs ----------------------------------------------------------------------------------------------------------------*)

type joueur = J1 | J2;;

type statut = joueur * bool * main;;



 (* 6.5 État d’une partie  *)
type etat = (statut * statut) * table * pioche * joueur	;;




let rec ieme(n:nat)(ens:'a multiensemble):'a=
  match ens with
  |[] -> failwith "ensemble vide"
  | (b,a) :: c -> if n <= a then b else ieme (n-a) c	;;
  
let    un_dans ( ens: 'a multiensemble ) : 'a = 
		let n = 1+Random.int(cardinal ens) in ieme (n) ens 
;;

let rec extraire(n:int) (p:pioche) : main*pioche = 
	match n,p with
	_,[]->failwith "pioche peut pas etre vide "
	|1,(pr,k)::fin -> let result = un_dans p in ( ajoute (result,1) [] ,(supprime (result,1) (p)))
	|_,(pr,k)::fin-> let (result,supprime_dans_pioche) = (extraire (n-1) p) in let a = un_dans supprime_dans_pioche in ( (ajoute (a,1) result) , (supprime (a,1) supprime_dans_pioche)) ;;


let distrib() : main*main*pioche =
	let (joueur1,pioche1) = extraire 14 cst_PIOCHE_INIT  in let (joueur2 , pioche2) = extraire 14 pioche1 in (joueur1 , joueur2 , pioche2) ;;

let(a,b,c) = distrib() in assert(cardinal a = cardinal b) ;;

let init_partie() : etat =
	let (main_joueur1 , main_joueur2 , pioche) = distrib() in
	( ((J1 , false , en_ordre(main_joueur1)) ,(J2 , false , en_ordre(main_joueur2))) , [[]] , en_ordre(pioche) , J1) 
;;

(*Q9*)

let joueur_courant ( a : etat ) : joueur =

		let ((statue_joueur_1 , statue_joueur_2), c , d , e) = a in e;;
		
let joueur_suivant ( a : etat ) : joueur =	
		let ((statue_joueur_1 , statue_joueur_2), c , d , e) = a in if e=J1 then J2 else J1;;
		

(*test joueur_courant *)		
assert ( joueur_courant (init_partie()) = J1  )	;;

let la_table ( a : etat ) : table = 
	let ((statue_joueur_1 , statue_joueur_2), table , d , e) = a in table ;;
	
(* test la_table *)
	
assert ( la_table (init_partie()) = [ [] ]);;	

let la_pioche ( a : etat ) : pioche  =
	let ((statue_joueur_1 , statue_joueur_2), table , pioche , e) = a in pioche;;
	

let le_statut ( joueur : joueur ) ( a : etat ) : statut =
	let ((statue_joueur_1 , statue_joueur_2), table , pioche , e) = a in
	match joueur with 
	J1 -> statue_joueur_1
	|J2-> statue_joueur_2 ;;
	


let la_main ( joueur : joueur ) ( a : etat ) : main =

		let ((statue_joueur_1 , statue_joueur_2), table , pioche , e) = a in 
		let (( joueur1 , boool1 , main1) , ( joueur2 , boool2 , main2) ) = (statue_joueur_1 , statue_joueur_2) in
		match joueur with 
		J1 ->en_ordre main1
		|J2 -> en_ordre main2;;
(* fin Q9 *) 		


(* Partie 7.1 ----------------------------------------------------------------------------------------------------------------
	Q10 *)
(* pour le test *)
let cst_comb1 = [T(4,Rouge); T(5,Rouge); T(6,Rouge)]
let cst_comb2 = [Joker; T(4,Bleu); T(5,Bleu)]
let cst_comb3 = [T(6,Jaune); T(7,Noir); T(8, Jaune)]


let cst_comb5 = [T(4,Rouge); T(4,Bleu); T(4,Jaune)]
let cst_comb6 = [T(3,Rouge); Joker; T(3,Noir)]	

let cst_comb_list1 = [cst_comb1; cst_comb2; cst_comb5; cst_comb6]
let cst_comb_list2 = [cst_comb1; cst_comb2; cst_comb3; cst_comb5]


(*1*)let  rec est_suite ( c : combinaison ) : bool =
		let carte = (List.find (fun x -> match x with Joker->false | T(valeur,couleur)->true ) c) in let T(valeur_verif , couleur_verif) = carte 
		in if (List.for_all (fun x -> match x with Joker->true |T(valeur,couleur) -> couleur_verif = couleur) c) = false then false 
		
		else 
		match c with
		[]->true
		|Joker ::[] -> true 
		| T(valeur,couleur)::[]-> true
		| T(valeur1,couleur1) :: Joker :: Joker :: T(valeur2,couleur2) :: fin -> valeur2-valeur1 = 2 && couleur1 = couleur2 && est_suite fin
		| T(valeur,couleur)::Joker::[]->true
		|T(valeur,couleur)::Joker::Joker::[]->true
		| pr :: fin -> if pr = Joker then est_suite  fin
						else let T(value, color) = pr in if List.hd fin = Joker then  let T(value2,color2) = ( List.hd(List.tl fin) ) in  (value2-value-1 = 1 && color=color2 && ( est_suite (List.tl fin) )) 
						else  let T(value3,color3) =  List.hd fin in  ( value3-value = 1 && color=color3) && est_suite fin ;;
		
assert ( est_suite [Joker;T(2,Bleu);T(3,Bleu);T(4,Bleu);T(5,Bleu);Joker] = true ) ;;

(*2*) let rec est_groupe ( c : combinaison ) : bool= 
	 
	 if List.length c = 3 || List.length c = 4 then
			let liste_sans_Joker = List.filter (fun x -> x<>Joker) c in
			let T(a,b) = List.find( fun x-> match  x with T(t,b) -> t>0 && t<=13 ) liste_sans_Joker in (* a c'est la valeur pour comparer que le reste de la combinaison = a *)
			let longueur_actuel = List.length (liste_sans_Joker) in let liste_egal_a = List.filter ( fun x-> match x with Joker->true |T(x,y)-> x = a ) liste_sans_Joker 
			in if List.length liste_egal_a = longueur_actuel then 
				match liste_sans_Joker with 
				pr ::[]->true
				|T(_,couleur1)::T(_,couleur2)::[]->couleur1 <> couleur2
				|T(_,couleur1)::T(_,couleur2)::T(_,couleur3)::[]-> (couleur1 <>couleur2 && couleur1<>couleur3 && couleur2<>couleur3)  
				|T(_,couleur1)::T(_,couleur2)::T(_,couleur3)::T(_,couleur4)::[] -> (couleur1<>couleur2 && couleur1<>couleur3 && couleur1<>couleur4 && couleur2<>couleur3 && couleur2<>couleur4 && couleur3<>couleur4)
		
		
			else false 
	 else false;;	

assert( est_groupe  [T(7,Rouge);T(7,Jaune);T(7,Bleu);T(7,Noir)] = true ) ;;
assert ( est_groupe [T(7,Rouge);T(5,Jaune);Joker;Joker] = false ) ;;		
		
		
		
		
		
		(*let liste_sans_Joker = List.filter (fun x -> x!=Joker) c in 
		match liste_sans_Joker with 
		pr :: [] -> true
		| T(valeur,couleur) :: fin  -> let  T(valeur_suiv,couleur_suiv) = List.hd fin   in valeur = valeur_suiv && not(couleur = couleur_suiv) && est_groupe fin *)
	
(*3*) let combinaison_valide (c:combinaison) : bool =
			(est_suite c) || ( est_groupe c) ;;
assert (combinaison_valide [Joker;T(2,Bleu);T(3,Bleu);T(4,Bleu);Joker;T(6,Bleu);Joker] = true  ) ;;

(*4*) let rec combinaisons_valides (  l : combinaison list) : bool =
		match l with
		[]->true
		| pr :: fin -> combinaison_valide pr && combinaisons_valides fin ;;
		
assert ( combinaisons_valides ([[Joker;T(2,Bleu);T(3,Bleu);T(4,Bleu);Joker;T(6,Bleu);Joker];[T(7,Jaune);T(7,Noir);T(7,Rouge) ; T(7,Bleu)] ]) = true ) 	;;	
     
	 

(* Partie 7.2 ----------------------------------------------------------------------------------------------------------------
	Q11 *)

(*1*) let rec  points_suite ( l : combinaison ) : int = 
		
		(* rajouter la verification *)
		match l with 
		[]-> 0
		|T(valeur,couleur)::[]-> valeur
		|Joker::[] -> 0 
		| Joker :: pr :: Joker :: fin -> let T(valeur,couleur) = pr in (valeur-1) + valeur + (valeur+1) + points_suite fin
		| Joker::Joker:: pr :: fin -> let T(valeur,couleur) = pr in (valeur-2) + (valeur-1) + valeur + points_suite fin 
		| pr :: Joker::Joker ::fin-> let T(valeur,couleur) = pr in valeur + valeur+1 + valeur+2 +(points_suite fin)
		| pr :: Joker :: fin -> let T(valeur,couleur)=pr in valeur + valeur +1 + (points_suite fin)
		| pr::fin -> if pr = Joker then let T(valeur,couleur) = List.hd fin in (valeur-1) +  (points_suite fin)   else  let T(valeur,couleur)=pr in valeur+ (points_suite fin ) ;;
		(*variant joker :: T(_,_) ou T(_,_) :: joker *)
		
		
(assert (points_suite [Joker;T(2,Bleu);T(3,Bleu);T(4,Bleu);T(5,Bleu);Joker] = 21)) ;;		
		
(*2*)let points_groupe ( l : combinaison ) : int =
		
		if combinaison_valide l then let length = List.length l  in let carte = ( List.find (fun x -> match x with Joker->false | T(valeur,couleur)->true ) l ) in let T(valeur,couleur) = carte in length*valeur
		else failwith "combinaison n'est pas valide";;

assert( points_groupe [Joker;T(7,Jaune) ; T(7, Noir) ] != 28 ) ;;

let rec points_pose ( p : pose ) : int = 
    if combinaisons_valides p then  List.fold_left (fun x y -> if est_suite y then x+points_suite y else x+points_groupe y) 0 p
	else failwith " pose n'est pas valides " ;;

assert ( points_pose [[Joker;T(2,Bleu);T(3,Bleu);T(4,Bleu);Joker;T(6,Bleu);Joker];[T(7,Jaune);T(7,Noir);T(7,Rouge) ; T(7,Bleu)] ] = 56	) ;;


(* Partie 8 Q12-Q17 ----------------------------------------------------------------------------------------------------------------*)

(* Q12 *)
(*Profil   tableVlist : table -> tuile list
  Semantique tableVlist t renvoie a partir de liste de liste une seule liste avec tous les elements de "t" *)
let rec tableVlist ( t : table) : tuile list =
	match t with 
	[]->[]
	|pr::fin -> pr @ tableVlist fin ;;

(*Profil listVlist : tuile list -> tuile multiensemble
  Semantique listVlist t ajoute  chaque element dans []  en tenant compte de nombre d'oocurrence qu'on fait dans la fonction ajoute *) 
let rec listVmens (t:tuile list):tuile multiensemble =
	match t with
	[]-> []
	| elt::suite -> ajoute (elt,1) (listVmens suite);;
 
let tableVmens (t:table):tuile multiensemble = 
		listVmens (tableVlist t);;
		
assert( tableVmens [[T(6,Jaune); T(7,Noir); T(8, Jaune)]] = [T(6,Jaune),1 ;T(7,Noir),1 ; T(8, Jaune),1 ] ) ;;

(* Q13 Validité des coups ---------------------------------------------------------------------------------------------------------------- *) 
let cst_main0 = [T(3,Bleu), 1; T(4,Bleu), 2; T(5,Bleu), 2; T(3,Rouge), 1; T(4,Rouge), 2; T(5,Rouge), 1;
T(6,Rouge), 1; T(4,Jaune), 1; T(3,Noir), 1; T(7,Noir), 2; Joker, 2]
let cst_main1 = [T(3,Bleu), 1; T(5,Bleu), 1; T(7,Noir), 2]
let cst_main2 = [T(5,Bleu), 1; T(7,Noir), 2]


let premier_coup_ok (m0:main) (p0:pose) (m1:main) : bool =
  let ens0 = (tableVmens p0) in
  (difference m0 ens0) = m1
  && (inclus m1 m0)
  && not(inclus m0 m1)
  && (combinaisons_valides p0)
  && (points_pose p0) >= 30
;;	
assert (premier_coup_ok cst_PIOCHE_INIT [[T(8,Rouge);T(8,Jaune);T(8,Bleu);T(8,Noir)]] (difference cst_PIOCHE_INIT (tableVmens [[T(8,Rouge);T(8,Jaune);T(8,Bleu);T(8,Noir)]])) = true) ;; 

let coup_ok (t0:table) (m0:main) (t1:table) (m1:main) : bool =
  let ens0 = (tableVmens t0) and ens1 = (tableVmens t1) in
  (difference ens1 ens0) = (difference m0 m1)
  && (inclus ens0 ens1)
  && not(inclus ens1 ens0)
  && (inclus m1 m0)
  && not(inclus m0 m1)
  && (combinaisons_valides t1)
;;

assert (coup_ok cst_comb_list1 cst_main1
[cst_comb1; [T(3,Bleu); T(4,Bleu); T(5,Bleu); Joker]; cst_comb5; cst_comb6] cst_main2 = true) ;;
assert (coup_ok [cst_comb1; [T(3,Bleu); T(4,Bleu); T(5,Bleu); Joker]; cst_comb5; cst_comb6]
cst_main2 cst_comb_list1 cst_main1 = false) ;;


(* Q14 Ajout d’une tuile sur la table ----------------------------------------------------------------------------------------------------------------*)

let rec ajouter_tuile (t:table) (el : tuile ) : table =
		match t with 
		[]-> [[]]
		| pr :: fin -> let a = List.sort compare (el::pr) in if combinaison_valide a then a::fin else pr::ajouter_tuile fin el ;;

assert ( ajouter_tuile  [cst_comb1; cst_comb6] (T(3,Bleu)) = [[T (4, Rouge); T (5, Rouge); T (6, Rouge)];
 [Joker; T (3, Bleu); T (3, Rouge); T (3, Noir)]]) ;;		
 

(* Q15 *)

		
		
let extraction_groupe ( m : main) : combinaison =
	let rec f = function
		0,_-> failwith " aucune combinaison trouvee "
		| n,_-> let (main,pioche) =( extraire (3+Random.int(2)) m) in let combinaison = tableVlist (List.map (fun (x,y)->if y>1 then [x]@[x] else [x]) main) in
				if est_groupe combinaison then combinaison else (f ((n-1),m) ) 
		in f (10000,m)
(*au lieu de faire assert  je verifie avec des exemples si la fonction est correcte bien sur *)	
 
 (* Q16 *)
 
 let  piocher ( (*(statue_1,statue_2),table,pioche,joueur*) e : etat ) : etat =
	 match la_pioche e with 
	 []-> e 
	 |_-> let joueur_couran = joueur_courant e  in let (carte_extrait , pioche_restee) = extraire 1 (la_pioche e) in 
	     let main_joueur_courant_plus_une_carte = en_ordre( (la_main (joueur_couran) e) @ carte_extrait ) in let (a,b,c) = le_statut (joueur_couran) e in if joueur_couran = J1 
		 then ( ((a,b,main_joueur_courant_plus_une_carte),le_statut J2 e) , la_table e , pioche_restee , joueur_couran) 
		 else ( ( le_statut J1 e,(a,b,main_joueur_courant_plus_une_carte) ) , la_table e , pioche_restee , joueur_couran ) ;;
		 
 assert ( cardinal(la_pioche (piocher(init_partie()))) = 77 )	;;	 
 
 
(* Q17  -----------------------------------------------------------------------Scenario---------------------------------------------------------------------------------*) 

(* Cette scenario est organisé d'apres avoir testé une partie de jeu avec les fonctions que je possede *)

(* Debut du partie *)
(* let etat_init : etat = init_partie() ;;


1. J1 pioche carte 

let e1 = piocher etat_init

2.J2 pioche
let e2 = piocher e2 

3.J1 joue son premier coup 

let e3 = jouer_1er_coup e2 [ [T(7,Rouge);T(8,Rouge);T(9,Rouge);T(10,Rouge) ] ] 

4. J2 pioche 

let e4 = piocher e3 

5. J1 pose des tuiles sur la table 

let e5 = jouer_1_coup e4 [ [T(7,Rouge);T(8,Rouge);T(9,Rouge);T(10,Rouge) ];
							[T(5,Bleu);T(5,Jaune);T(5,Noir) ]] 

6. J2 pioche 

let e6 = piocher e5

7.J1 ajoute T(5,Rouge) a la combinaison [ T(5,Bleu);T(5,Jaune);T(5,Noir) ]

let table_nouvelle = ajouter_tuile (la_table e6) T(5,Rouge) 

8. J2 pioche 

let e7 = piocher e6

9. J1  pose des tuiles sur la table 

let e8 = jouer_1_coup e7 [ [T(7,Rouge);T(8,Rouge);T(9,Rouge);T(10,Rouge) ];
							[T(5,Bleu);T(5,Jaune);T(5,Noir) ] ;
							[T(2,Rouge);T(2,Jaune);T(2,Noir)] ] 
							
10. J2 pioche

let e9 = piocher e8

11. J1 piuoche

let e10 = piocher e9

12. J1  pose des tuiles sur la table 							

let e11 = jouer_1_coup e10 [ [T(7,Rouge);T(8,Rouge);T(9,Rouge);T(10,Rouge) ];
							[T(5,Bleu);T(5,Jaune);T(5,Noir) ] ;
							[T(2,Rouge);T(2,Jaune);T(2,Noir)] ;
							[ T(10,Jaune);T(11,Jaune);T(12,Jaune)] ]
							
13. J2 pioche 

let e12 = piocher e11

14.J1 pioche 

let e13 = piocher e12

15. J2 pioche 
let e14 = piocher e13

16... *)							








