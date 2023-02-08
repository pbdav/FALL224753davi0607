#' Title function for z
#'
#' @param x quantitative vector
#'
#' @importFrom stats sd
#'
#' @return a list containing z and x
#' @export
#'
#' @examples
z<-function(x){
  z <-(x-mean(x))/sd(x)
  list(z = z, x=x)

}
