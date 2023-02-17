#' Create a Bag of Marbles
#' 
#' This function creates a vector 'bag' of black and white marbles.
#' 
#' @param numMarbles total number of marbles
#' 
#' @param whiteMarbs number of white marbles in the bag
#' 
#' @param blackMarbs number of black marbles in the bag
#' 
#' @return a vector of 1's and 0's representing marbles in a bag
#' @export
#' 




bagomarbles = function(numMarbles, whiteMarbs, blackMarbs){
  marbles <- vector(mode="integer", length = numMarbles)
  blackMarbs = blackMarbs + whiteMarbs
  for(i in 1:length(marbles))
  {
    if (i<=whiteMarbs)
      marbles[i] = 1
    if(i>whiteMarbs & i<=blackMarbs)
      marbles[i] = 0
  }
  print("1 represents white marbles, 0 represents black marbles")
  print("Bag of marbles:")
  print(marbles)
  return(marbles)
}