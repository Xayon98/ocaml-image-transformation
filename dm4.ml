(* Author : Xayon *)

(* Imports *)
open Debut;;


(* Fonctions affichage simple *)

let print_array a =
  print_string "[| ";
  for i=0 to Array.length a-1 do
    Printf.printf "%d " a.(i)
  done;
  print_string "|]";;

let print_matrice m =
  for i=0 to Array.length m-1 do
    print_array m.(i);
    print_newline()
  done;;

let print_array_float a =
  print_string "[| ";
  for i=0 to Array.length a-1 do
    Printf.printf "%.1f " a.(i)
  done;
  print_string "|]";;

let print_matrice_float m =
  for i=0 to Array.length m-1 do
    print_array_float m.(i);
    print_newline()
  done;;


(* Definition du type energy qui est une tableau de tableau *)
type energy = float array array;;
let foi = float_of_int;;
let iof = int_of_float;;


(* Fonctions de tranformation d'images *)

(* O(wh) *)
(* Fonction qui seuil les pixels , tout ce qui est plus grand que s prend la
valeur 255, ce qui est plus petit strictement prend la valeur 0 *)
let seuil s (mat:img):img =
  let w,h = dim mat in
  let newmat = Array.make_matrix h w 0 in
  for i=0 to h-1 do
    for j=0 to w-1 do
      if mat.(i).(j)<s
      then newmat.(i).(j) <- 0
      else newmat.(i).(j) <- 255
    done;
  done;
  newmat;;

(* O(wh) *)
(* Fonction qui effectue une symétrie horizontale de l’image *)
let symh (mat:img):img = 
  let w,h = dim mat in
  let newmat = Array.make_matrix h w 0 in
  for j=0 to w-1 do
    for i=0 to h-1 do
      newmat.(i).(j) <- mat.(h-1-i).(j)
    done;
  done;
  newmat;;

(* O(wh) *)
(* Fonction qui effectue une symétrie verticale de l’image *)
let symv (mat:img):img = 
  let w,h = dim mat in
  let newmat = Array.make_matrix h w 0 in
  for i=0 to h-1 do
    for j=0 to w-1 do
      newmat.(i).(j) <- mat.(i).(w-1-j)
    done;
  done;
  newmat;;
  

(* O(n)*)
(* Fais la moyenne des pixels 2 à 2 et les ajoutes dans un nouvel array *)
let reduction_moitie_ligne l =
  let l_length = Array.length l in (* n + 1 *)
  let lf = Array.make (l_length/2) 0 in(* n/2 + 2 *)
  for i=0 to (l_length/2)-1 do (* n/2 + 2*)
    lf.(i) <- (l.(2*i)+l.(2*i+1))/2 (* 0(1) *)
  done;
  lf;;
    

(* O(wh)*)
(* Applique reduction_moitie_ligne sur chaque ligne d'une image *)
let reduction_moitie_image (img:img) =
  let len = (Array.length img)-1 in
  for i=0 to len do
      img.(i) <- reduction_moitie_ligne img.(i)
  done;;


(* O(wh) *)
(* On applique la formule de calcul de l'energie - e(i,j) - d'un pixel a tout les cas 
possibles dans une image en parcourant la matrice de taille w*h

(e(i,j) = abs(I(i+1,j) - I(i-1,j)/2) + abs(I(i,j+1) - I(i,j-1))/2)
 avec I(i,j) le niveau de gris d'un pixel de coordonnée (i,j)) *)
let energie (mat:img):energy =
  let w,h = dim mat in
  let newmat = Array.make_matrix h w 0.0 in (* O(wh) *)
  for i=0 to h-1 do (* O(hw) *)
    for j=0 to w-1 do 
      match i,j with 
      | 0,0 -> newmat.(i).(j) <- abs ((foi (mat.(i+1).(j)-mat.(i).(j)))/.2.) +. abs((foi (mat.(i).(j+1)-mat.(i).(j)))/.2.)
      | 0,y -> if y=w-1 
        then newmat.(i).(j) <- abs ((foi (mat.(i+1).(j)-mat.(i).(j)))/.2.) +. abs ((foi (mat.(i).(j)-mat.(i).(j-1)))/.2.)
        else newmat.(i).(j) <- abs ((foi (mat.(i+1).(j)-mat.(i).(j)))/.2.) +. abs ((foi (mat.(i).(j+1)-mat.(i).(j-1)))/.2.)
      | x,0 -> if x=h-1 
        then newmat.(i).(j) <- abs ((foi (mat.(i).(j)-mat.(i-1).(j)))/.2.) +. abs ((foi (mat.(i).(j+1)-mat.(i).(j)))/.2.)
        else newmat.(i).(j) <- abs ((foi (mat.(i+1).(j)-mat.(i-1).(j)))/.2.) +. abs ((foi (mat.(i).(j+1)-mat.(i).(j)))/.2.)
      | x,y -> if x=h-1 
        then if y=w-1 
          then newmat.(i).(j) <- abs ((foi (mat.(i).(j)-mat.(i-1).(j)))/.2.) +. abs ((foi (mat.(i).(j)-mat.(i).(j-1)))/.2.)
          else newmat.(i).(j) <- abs ((foi (mat.(i).(j)-mat.(i-1).(j)))/.2.) +. abs ((foi (mat.(i).(j+1)-mat.(i).(j-1)))/.2.)
        else if y=w-1 
          then newmat.(i).(j) <- abs ((foi (mat.(i+1).(j)-mat.(i-1).(j)))/.2.) +. abs ((foi (mat.(i).(j)-mat.(i).(j-1)))/.2.)
          else newmat.(i).(j) <- abs ((foi (mat.(i+1).(j)-mat.(i-1).(j)))/.2.) +. abs ((foi (mat.(i).(j+1)-mat.(i).(j-1)))/.2.)
    done;
  done;
  newmat;;


(* O(wh)*)
(* Convertie une matrice de float(type energy) en matrice de int (type img) *)
let energie_to_image (mat:energy):img =
  let w,h = dim mat in (* O(1) *)
  let newmat = Array.make_matrix h w 0 in (* O(wh) *)
  for i=0 to h-1 do(* O(wh) *)
    for j=0 to w-1 do
      newmat.(i).(j) <- int_of_float mat.(i).(j) (* O(1) *)
    done;
  done;
  newmat;;


(* O(w)*)
(* Ajoute tout les éléments de l dans newl sauf l'element à l'indice i *)
let enlever l i =
  let len = Array.length l in (* O(w) *)
  let newl = Array.make (len-1) 0 in (* O(w) *)
  for j=0 to i-1 do (* O(i) *)
    newl.(j) <- l.(j) (* O(1) *)
  done;
  for j=i to len-2 do (* O(w-i) *)
    newl.(j) <- l.(j+1) (* O(1) *)
  done;
  newl;;


(* O(w) *)
(* Renvoie l'indice auquel ce trouve la valeur minimun d'un array *)
let indice_min l =
  let len = Array.length l in (* O(w) *)
  let min = ref l.(0) in (* O(1) *)
  let indice = ref 0 in (* O(1) *)
  for i=1 to len-1 do (* O(w) *)
    if l.(i) < !min then (* O(1) *)
      begin
        min := l.(i); (* O(1) *)
        indice := i (* O(1) *)
      end
  done;
  !indice;;


(* O(wh)*)
(* Enleve par effet de bord les elements avec l'energie la moins importante
de chaque ligne *)
let reduction_ligne_par_ligne (mat:img) =
  let w,h = dim mat in (* O(1) *)
  let img_energie = energie mat in (* O(wh) *)
  for i=0 to h-1 do (* O(hw) *)
    mat.(i) <- enlever mat.(i) (indice_min img_energie.(i));
  done;;


(* O(nwh)*)
(* Des distortions sont observées due à la méthode utilisée *)
let itere_reduction_ligne_par_ligne (mat:img) n =
  for i=0 to n-1 do (* O(nwh) *)
    reduction_ligne_par_ligne mat (* O(wh) *)
  done;;


(* O(wh)*)
(* Fait la somme des energies de chaque colonne et la compare avec la colonne
avec le moins d'energie pour trouver la colonne avec la plus petite energie *)
let meilleure_colonne (mat:energy) =
  let w,h = dim mat in (* O(1) *)
  let min = ref ((foi h)*.255.) in (* O(1) *)
  let indice = ref 0 in (* O(1) *)
  let sum = ref 0. in (* O(1) *)

  for j=0 to w-1 do (* O(wh) *)
    for i=0 to h-1 do
      sum := !sum +. mat.(i).(j) (* O(1) *)
    done;
    if !sum < !min then (* O(1) *)
      begin
        min := !sum; (* O(1) *)
        indice := j (* O(1) *)
      end;
    sum := 0. (* O(1) *)
  done;
  !indice;;


(* O(wh)*)
(* Enleve la colonne avec le moins d'energie par effet de bord *)
let reduction_meilleure_colonne (mat:img) =
  let w,h = dim mat in (* O(1) *)
  let img_energie = energie mat in (* O(wh) *)
  let m_c = meilleure_colonne img_energie in (* O(wh) *)
  for i=0 to h-1 do (* O(hw) *)
    mat.(i) <- enlever mat.(i) (m_c) (* O(w) *)
  done;;


(* O(nwh)*)
(* applique reduction_meilleur_colonne n-fois*)
let itere_reduction_meilleure_colonne (mat:img) n =
  for i=0 to n-1 do (* O(nwh) *)
    reduction_meilleure_colonne mat; (* O(wh) *)
  done;;


(* O(w) *)
(* calcul la ligne i de la matrice des énergies minimales en utilisant les
 lignes déjà renseignées de em et la matrice des énergies e en utilisant des
 effets de bord. *)
let ajouter_ligne (e:energy) (em:energy) i =
  let w,h = dim e in (* O(1) *)
  let arr = Array.make w 0. in (* O(w) *)
  if i=0 then (* O(w) *)
    for j=0 to w-1 do (* O(w) *)
      arr.(j) <- e.(i).(j) (* O(1) *)
    done
  else (* O(w) *)
    for j=0 to w-1 do (* O(w) *)
      if j=0 then (* O(1) *)
        arr.(j) <- e.(i).(j) +. min em.(i-1).(j) em.(i-1).(j+1) (* O(1) *)
      else if j=w-1 then (* O(1) *)
        arr.(j) <- e.(i).(j) +. min em.(i-1).(j-1) em.(i-1).(j) (* O(1) *)
      else
        arr.(j) <- e.(i).(j) +. min (min em.(i-1).(j-1) em.(i-1).(j)) em.(i-1).(j+1) (* O(1) *)
    done;
    em.(i) <- arr;; (* O(w) *)


(* O(wh)*)
(* Renvoie la matrice des energies minimales d'une matrice d'energie en
appliquant ajouter_ligne pour toute les lignes de e *)
let construire_E (e:energy) =
  let w,h = dim e in (* O(1) *)
  let em = Array.make_matrix h w 0. in (* O(wh) *)
  for i=0 to h-1 do (* O(hw) *)
    ajouter_ligne e em i (* O(w) *)
  done;
  em;;


(* O(wh) *)
(* Renvoie un chemin d'energie minimal sous forme d'un tableau des coordonées
des pixels du chemin en parcourant ligne par ligne en partant de la derniere
ligne et en remontant le tableau en cherchant le minimum entre pixel situé au
dessus de la case et si ils existent les pixels en diagonales de la case*)
let chemin_minimal (em:energy) =
  let w,h = dim em in (* O(1) *)
  let chemin = Array.make_matrix h 2 0 in (* O(2h) *)
  let j = ref (indice_min em.(h-1)) in (* O(w) *)
  chemin.(h-1).(0) <- h-1; (* O(1) *)
  chemin.(h-1).(1) <- !j; (* O(1) *)
  for i=h-2 downto 0 do (* O(h) *)
    chemin.(i).(0) <- i; (* O(1) *)
    if !j=0 then (* O(1) *)
      chemin.(i).(1) <- (if em.(i).(!j)<em.(i).(!j+1) then !j else !j+1) (* O(1) *)
    else if !j=w-1 then (* O(1) *)
      chemin.(i).(1) <- (if em.(i).(!j-1)<em.(i).(!j) then !j-1 else !j) (* O(1) *)
    else (* O(1) *)
      chemin.(i).(1) <- (!j-1 +(indice_min [|em.(i).(!j-1); em.(i).(!j); em.(i).(!j+1)|])); (* O(1) *)
    j := chemin.(i).(1); (* O(1) *)
  done;
  chemin;;


(* O(hw) *)
(* Suprimme les pixels appartenant au chemin d'energie minimal de l'image par
  effet de bord*)
let reduction_meilleure_energie (mat:img) =
  let w,h = dim mat in (* O(1) *)
  let e = energie mat in (* O(wh) *)
  let em = construire_E e in (* O(wh) *)
  let chemin = chemin_minimal em in (* O(wh) *)
  let j = ref 0 in (* O(1) *)
  for i=0 to h-1 do (* O(hw) *)
    j := chemin.(i).(1); (* O(1) *)
    mat.(i) <- enlever mat.(i) !j; (* O(w) *)    
  done;;


(* O(nhw) *)
(* Applique reduction_meilleure_energie n-fois a l'image *)
let itere_reduction_meilleure_energie (mat:img) n =
  for i=0 to n-1 do (* O(nhw) *)
    reduction_meilleure_energie mat (* O(hw) *)
  done;;



  