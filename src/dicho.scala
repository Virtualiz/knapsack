

class dicho {
  def dicho(tab: Array[Int], elm: Int) : Int = {
    var min = 0
    var max = tab.length -1 
    while( max-min > 1){
      
      val milieu = (min+max)/2
      
      if(elm < tab(milieu)){
        
        max = milieu
        
      }else{       
        min = milieu
      }
      
      if(tab(min) == elm){
        return min
      }else if(tab(max)==elm){
        
        return max
        
      }else return -1
    }
  }
  
  def dichoRec(tab:Array[Int], elm:Int):Int = {
    def aux(tab:Array[Int], elm:Int,min:Int,max:Int):Int = {
      if(max-min <=1){
        if(tab(min) == elm){
          return min
        }
        else if(tab(max)==elm){
          return max
        }else return -1
      }else{
        val milieu = (min+max)/2
        if(elm<tab(milieu)){
          return aux(tab,elm,min,milieu)
        }else{
          return aux(tab,elm,milieu,max)
        }
      }
    }
    return aux(tab,elm,0,tab.length-1)
  }
  
  def test(tab:Array[Int]){
    def cherche(searched:Int, expected:Int){
      val found = dichoRec(tab, searched)
      if(found != expected)
        println("ERREUR : elm "+ searched + " trouvé en "+found+" (attendu en "+expected+").")
      else
        println("elm "+searched+" trouvé en "+found+" comme attendu.")
    }
    cherche(0,-1)
    for(cpt <- 0 to tab.length-1){
      cherche(tab(cpt),cpt)
    }
    cherche(5,-1)
    cherche(11,-1)
  }
  
}