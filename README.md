Veuillez m'excuser pour gagner un peu de temps je n'utilise pas des accents .

C'est un projet qui permet d'utiliser les connaissances de la theorie de l'ensemble et de langage fonctionnel ocaml appliqués dans le contexte de jeu des cartes . 
Ce projet n'a pas de la partie automatiser et pour l'utiliser il faut un terminal et utiliser la commande " ocaml nom_fichier ". Soit vous pouvez copier le projet et de le coller dans la fenetre du terminal du site https://try.ocamlpro.com et d'executer 

Voici un scenario qui vous permet de comprendre comment utiliser le programme :
(* *) signifie commentaire de plusieurs lignes 
(* Debut du partie *)
 let etat_init : etat = init_partie() ;;

J1 et J2 sont respectivement joueur J1 et J2 
1. J1 pioche carte 

let e1 = piocher etat_init

2.J2 pioche
let e2 = piocher e2 

3.J1 joue son premier coup 

let e3 = jouer_1er_coup e2 [ [T(7,Rouge);T(8,Rouge);T(9,Rouge);T(10,Rouge) ] ]  (* T(7,Rouge) produit cartesien de nombre et string .C'est une carte de valeur 7 et couleur Rouge *)

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
 
 
