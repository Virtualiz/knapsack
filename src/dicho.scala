

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
}