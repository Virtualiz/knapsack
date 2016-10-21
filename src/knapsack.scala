/** Probleme du sac a dos */

object knapsack extends App { 

// Diverses fonctions d'aide
////////////////////////////

/** Calcule le gain d'un sac a dos donne' */
def valeurTotale(objets:Array[Boolean], poids:Array[Int]):Int = {
  var total = 0
  for (i <- 0 to objets.length -1)
    if (objets(i)) 
      total += poids(i)
  return total
}

/** Affiche si les objets sont pris ou non dans une solution partielle (en cours de construction)
 *
 * Le second argument est la taille deja construite.
 */
def affiche(objets:Array[Boolean], objetMax:Int) {
	      
  // on utilise un min pour se proteger du cas (frequent) ou cette
  // fonction est appellee avec un second argument superieur a la
  // taille du tableau.
  for (i <- 0 to math.min(objetMax, objets.length-1))
    if (objets(i))
      print(" O ")
    else
      print(" N ")
  if (objetMax < objets.length-1)
    for (i <- objetMax to objets.length-2)
      print("...")
  print(";")
}


/** Dupplique un sac dans un autre, pour se souvenir de la meilleure solution connue */
def dupplique(src:Array[Boolean], dst:Array[Boolean]) {
  for (i <- 0 to src.length-1) 
    dst(i) = src(i)
}

/** indique dans le parametre que l'objet specifie est maintenant pris
    (cette fonction est juste la pour se simplifier la vie) */
def mettreDansSac(objets:Array[Boolean], obj:Int) {
    if (objets(obj)) 
        println("L'objet "+obj+" est deja pris; ignore la requete.");
    objets(obj) = true
}
/** indique dans le parametre que l'objet specifie est maintenant pose' */
def retireDuSac(objets:Array[Boolean], obj:Int) {
    if (!objets(obj)) 
        println("L'objet "+obj+" est deja pose'; ignore la requete.");
     objets(obj) = false
}

// La fonction principale
/////////////////////////

/** La fonction publique, pour chercher la meilleure solution */
def cherche(poids:Array[Int] , capacite:Int) {

  // on va beaucoup utiliser cette valeur, alors on fait un alias 
  // pour simplifier les ecritures suivantes
  val len = poids.length
  
  // Affiche l'instance du probleme
  print("Poids des objets: ")
  for (i <- 0 to poids.length-1) 
    print(" "+poids(i)+" ")
  println("; Capacite: "+capacite)
  
  // variable locale pour sauvegarder la meilleure solution connue a tout moment
  var meilleure:Array[Boolean] = Array.fill(len)(false)



//////////////////////////////////////////////////////////////////////////////
// TODO: Placez ici l'appel recursif, avec les valeurs initiales des parametres
//////////////////////////////////////////////////////////////////////////////
  chercheRec(0,Array.fill(len)(false))

  // Affiche la meilleure solution trouvee
  println
  print("Meilleure solution trouvee:")
  for (i <- 0 to meilleure.length-1)
    print(" "+poids(i)+":"+(if (meilleure(i)) "O" else "N")+";")
  println(" Valeur:"+valeurTotale(meilleure,poids)+" (la capacite etait "+capacite+")")


////////////////////////////////////////////////////////////////
// TODO: Definissez ici la fonction recursive a proprement parler
////////////////////////////////////////////////////////////////
  def chercheRec(profondeur : Int, sac_courant : Array[Boolean]){
    if (profondeur ==  sac_courant.length) {
      if (valeurTotale(sac_courant, poids) > valeurTotale(meilleure, poids)) {
        meilleure = sac_courant;
      }
      return
    }
    else {
      sac_pris = Array[Boolean].fill(len)(false)
      dupplique(sac_courant,sac_pris)
      mettreDansSac(sac_pris, profondeur)
      if (valeurTotale(sac_pris, poids) <= capacite) {
        chercheRec(profondeur+1, sac_pris)
      }
      chercheRec(profondeur+1, sac_courant)
    }
  }

} // Fin de la fonction cherche() principale



// Le code de test, qui appelle la fonction publique
////////////////////////////////////////////////////
cherche(Array(5,4,3,2), 10)

}
