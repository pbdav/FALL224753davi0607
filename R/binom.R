#' Binomial Plot
#'
#' This function plots the percentages of occurences of a binomial variable,
#' based on a probability
#'
#' @param iter amount of times to take samples
#' @param n size of distribution/sample values
#' @param p probability of success/getting the binomial variable
#'
#' @return Both a plot and a table of the percentages of successes
#' @export
#'
#' @examples
binom=function(iter=100,n=10, p=0.5){
  print(p)
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)

  succ=c()


  for( i in 1:iter){


    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    succ[i]=sum(sam.mat[,i])
  }

  succ.tab=table(factor(succ,levels=0:n))

  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
