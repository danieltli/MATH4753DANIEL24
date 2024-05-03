#' A function for graphing samples
#'
#' @param n number of samples
#'
#'@param iter number of iterations
#' @return a numeric vector
#' @export
#'
#' @examples
#' myctl(3,21)
#'
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
  sm
}
w=myclt(n=10,iter=10000) #D
