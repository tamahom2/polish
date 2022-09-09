Rapport
=======

### Identifiants ###  

BADAOUI Ismail @badaoui 21964109    
NGUYEN Thuy Vi Emilie @nguyenth 21953967  

### Fonctionnalités ###  

Le sujet minimal a été implémenté. Notre projet permet l'évaluation d'un programme Polish avec les 
commandes `-reprint` et `-eval`, ainsi que son analyse statique avec les commandes `-simpl`, `-vars` et `-sign`.  

### Compilation et exécution ###

Le projet doit être compilé sur un terminal depuis le répertoire `pf5-projet` avec la commande suivante :

```
make
```

On peut ensuite l'exécuter avec la commande :

```
./run -arg path_file
```

où `arg` doit être l'une des options suivantes :

 - `reprint` pour afficher le code du programme dans le terminal sans les commentaires
 - `eval` pour évaluer le programme
 - `simpl` pour afficher le code simplifié du programme
 - `vars` pour afficher la liste de toutes les variables et celles qui ne sont pas initialisées
 - `sign` pour afficher les signes possibles de chaque variable du programme.

 et `path_file` doit être le chemin menant au fichier contenant le programme Polish à traiter. Par exemple :

```
./run -reprint exemples/mult_russe.p
```

Penser à 
```
make clean
```
à la fin des tests.

Nous avons utilisés trois bibliothèques externes : [List](https://ocaml.org/api/List.html), [Set.S](https://ocaml.org/api/Set.S.html) et [Hashtbl](https://ocaml.org/api/Hashtbl.html).

### Découpage modulaire ###

Notre projet possède une architecture modulaire permettant un ajout facile de fonctionnalité supplémentaire.
Il est découpé en sept modules de la manière suivante : 

1. `polish.ml`, le fichier principal contenant le main
2. les modules nécessaire à l'évaluation d'un programme Polish 
    - `types.ml` définissant la syntaxe abstraite du langage Polish
    - `auxilary.ml` comportant les fonctions principales et auxiliaires du main : lire (`read_file`), afficher (`print_blk`) et évaluer (`eval_blk`)
    - `convertor.ml` regroupant les fonctions de conversion nécessaire pour faire les transitions du langage Polish à l'Ocaml
3. les modules d'option fonctionnelles
    - `simpl.ml` pour l'option simpl
    - `sign.ml` pour l'option sign
    - `vars.ml` pour l'option vars

### Organisation du travail ###

Nous avons essayé de nous répartir au mieux le travail en fonction du temps de chacun et de son aisance avec le langage utilisé.
Plusieurs sessions de travail où nous programmions ensemble via l'application Discord ont été réalisé.  
Ismail s'est occupé des fonctions auxiliaires et celles de conversion. Emilie a peaufiné le module de conversion
et ajouté la documentation du projet. Ensemble, nous avons programmés les options simpl et vars. 
Par la suite, Ismail a fait l'option sign et a débug pendant qu'Emilie a modulé le projet en fixant l'architecture et a rédigé le rapport.

### Misc ###
