(* Author : Xayon *)

(* Imports *)
open Debut;;
open Dm4;;

(* Liste des fonctions de test *)


let test_seuil () =
  Printf.printf "----------debut_test_seuil----------\n";
  display Sys.argv.(1) (seuil 100);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_seuil-----------\n";
  print_newline();;

let test_symh () =
  Printf.printf "----------debut_test_symh----------\n";
  display Sys.argv.(1) symh;
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_symh-----------\n";
  print_newline();;

let test_symv () =
  Printf.printf "----------debut_test_symv----------\n";
  display Sys.argv.(1) symv;
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_symv-----------\n";
  print_newline();;


 
Printf.printf "----------debut_test_reduction_moitie_ligne----------\n";
print_array (reduction_moitie_ligne [|1;3;4;6|]);
print_newline();
Printf.printf "-----------fin_test_reduction_moitie_ligne-----------\n";
print_newline();;


let test_reduction_moitie_image () =
  Printf.printf "----------debut_test_reduction_moitie_image----------\n";
  let mat = [| [|1;3;4;6|]; [|1;4;7;6|]; [|1;2;4;9|]; [|1;3;4;6|] |] in
    reduction_moitie_image mat;
    for i=0 to Array.length mat-1 do
      print_array mat.(i)
    done;

  display Sys.argv.(1) (fun img -> reduction_moitie_image img; img);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_reduction_moitie_image-----------\n";
  print_newline();;


 
Printf.printf "----------debut_test_energie----------\n";
let img = [|[|192; 157; 86; 25; 215; 44; 40|];
            [|236; 17; 226; 209; 69; 205; 118|];
            [|159; 92; 65; 238; 84; 165; 121|];
            [|121; 62; 15; 107; 32; 240; 131|];
            [|52; 194; 129; 60; 19; 67; 229|];
            [|253; 169; 92; 39; 195; 240; 143|];
            [|111; 2; 84; 251; 20; 90; 186|]|] in
print_matrice_float (energie img);; 
Printf.printf "-----------fin_test_energie-----------\n";
print_newline();;



let test_energie_to_image () =
  Printf.printf "----------debut_test_energie_to_image----------\n";
  display Sys.argv.(1) (fun img -> energie_to_image (energie img));
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_energie_to_image-----------\n";
  print_newline();;


Printf.printf "----------debut_test_enlever----------\n";
print_array (enlever [|10;20;30;40|] 2);
print_newline();
Printf.printf "-----------fin_test_enlever-----------\n";
print_newline();;


Printf.printf "----------debut_test_indice_min----------\n";
Printf.printf "%d\n" (indice_min [|100;23;89;12;89|]);
print_newline();
Printf.printf "-----------fin_test_indice_min-----------\n";
print_newline();;


let test_reduction_ligne_par_ligne () =
  Printf.printf "----------debut_test_reduction_ligne_par_ligne----------\n";
  display Sys.argv.(1) (fun img -> reduction_ligne_par_ligne img; img);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_reduction_ligne_par_ligne-----------\n";
  print_newline();;


let test_itere_reduction_ligne_par_ligne () =
  Printf.printf "----------debut_test_itere_reduction_ligne_par_ligne----------\n";
  display Sys.argv.(1) (fun img -> itere_reduction_ligne_par_ligne img 200; img);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_itere_reduction_ligne_par_ligne-----------\n";
  print_newline();;


Printf.printf "----------debut_test_meilleure_colonne----------\n";
Printf.printf "%d\n" (meilleure_colonne (energie (load_matrix Sys.argv.(1))));
print_newline();
Printf.printf "-----------fin_test_meilleure_colonne-----------\n";
print_newline();;


let test_reduction_meilleure_colonne () =
  Printf.printf "----------debut_test_reduction_meilleure_colonne----------\n";
  display Sys.argv.(1) (fun img -> reduction_meilleure_colonne img; img);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_reduction_meilleure_colonne-----------";
  print_newline();;


let test_itere_reduction_meilleure_colonne () =
  Printf.printf "----------debut_test_itere_reduction_meilleure_colonne----------\n";
  display Sys.argv.(1) (fun img -> itere_reduction_meilleure_colonne img 200; img);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_itere_reduction_meilleure_colonne-----------\n";
  print_newline();;


Printf.printf "----------debut_test_ajouter_ligne----------\n";
let e = [|[|1.;1.;0.;3.|];
          [|4.;1.;2.;4.|];
          [|1.;2.;2.;1.|];
          [|4.;1.;1.;0.|]|] and

  em = [|[|1.;1.;0.;3.|];
        [||];
        [||];
        [||]|] in
  ajouter_ligne e em 1;
  print_matrice_float em ;;
Printf.printf "-----------fin_test_ajouter_ligne-----------\n";
print_newline();;


Printf.printf "----------debut_test_construire_E----------\n";
let e = [|[|1.;1.;0.;3.|];
          [|4.;1.;2.;4.|];
          [|1.;2.;2.;1.|];
          [|4.;1.;1.;0.|]|] in
  print_matrice_float (construire_E e) ;;
Printf.printf "-----------fin_test_construire_E-----------\n";
print_newline();;



Printf.printf "----------debut_test_chemin_minimal----------\n";
let em = [|[|1.; 1.; 0.; 3.|];
          [|5.; 1.; 2.; 4.|];
          [|2.; 3.; 3.; 3.|];
          [|6.; 3.; 4.; 3.|]|]
  in print_matrice (chemin_minimal em) ;;
Printf.printf "-----------fin_test_chemin_minimal-----------\n";
print_newline();;



let test_reduction_meilleure_energie () =
  Printf.printf "----------debut_test_reduction_meilleure_energie----------\n";
  display Sys.argv.(1) (fun img -> reduction_meilleure_energie img; img);
  Graphics.draw_string "Chargement de l'image suivante en cours";
  Printf.printf "-----------fin_test_reduction_meilleure_energie-----------\n";
  print_newline();;


let test_itere_reduction_meilleure_energie () =
  Printf.printf "----------debut_test_itere_reduction_meilleure_energie----------\n";
  display Sys.argv.(1) (fun img -> itere_reduction_meilleure_energie img 100; img);
  Printf.printf "-----------fin_test_itere_reduction_meilleure_energie-----------\n";
  print_newline();;
  
  
(* Pour fermer la fenetre graphique a l'appel de la fonction *)
let quitter () =
  Graphics.clear_graph ();
  Graphics.close_graph();;
  


(* Fonction d'affichage graphique d'un menu pour afficher les différentes
sorties des fonctions *)
let menu () =
  let dimensions = " 690x460" in
  let mat = load_matrix "menu.png" in
  Graphics.open_graph dimensions;
  (* Affichage de l'écran de chagement avant l'appel de chaques fonctions *)
  let charg () = Graphics.draw_image (to_graphics (load_matrix "chargement.png")) 0 0 in
  let flag = ref false in
  while not !flag do
    Graphics.draw_image (to_graphics mat) 0 0;    
    let key = Graphics.read_key () in
      if key = 'a' then begin charg (); test_seuil (); end
      else if key = 'b' then begin charg (); test_symh () end
      else if key = 'c' then begin charg (); test_symv () end
      else if key = 'd' then begin charg (); test_reduction_moitie_image () end
      else if key = 'e' then begin charg (); test_energie_to_image () end
      else if key = 'f' then begin charg (); test_reduction_ligne_par_ligne () end
      else if key = 'g' then begin charg (); test_itere_reduction_ligne_par_ligne () end
      else if key = 'h' then begin charg (); test_reduction_meilleure_colonne () end
      else if key = 'i' then begin charg (); test_itere_reduction_meilleure_colonne () end
      else if key = 'j' then begin charg (); test_reduction_meilleure_energie () end
      else if key = 'k' then begin charg (); test_itere_reduction_meilleure_energie () end
      else if key = 'q' then begin quitter (); end
      else ();
    (* Effacage de l'affichage *)
    Graphics.clear_graph (); 
  done;;


(* Appelle la fonction menu *)
menu ();;