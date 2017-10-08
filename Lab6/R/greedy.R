#' Greedy Algorithm 
#'
#' Knapsack problem is solved using greedy algorithm.
#' @param x is a data frame containing weights and values of elements.
#' @param W is the highest weight capacity.
#'
#' @return the output is the maximum value the knapsack can holf for the give weight capacity.
#' @export
#'
#' @examples greedy_knapsack(x = knapsack_objects[1:100,], W = 250)
#' 
#' @references \href{https://en.wikipedia.org/wiki/Knapsack_problem}{Dynamic Algorithm}
#'


greedy_knapsack<-function(x,W){

  stopifnot(is.data.frame(x)== TRUE && is.numeric(W)== TRUE)  
  stopifnot(x[[1]]>0 && x[[2]]>0 && W>0)
  
  
dummy <- 0
weights <- x$w
value <- x$v
x$d <- x$v/x$w
density <- x$d
x <- x[order(x$d,decreasing = TRUE),]
n <- nrow(x)

ks <- data.frame(w=c(),v=c(),tw=c(),ben=c())


for(i in 1:n){
if(x[i,1]+ sum(x$w[1:i-1]) <= W){
  ks[i,1]<- x[i,1]
  ks[i,2]<- x[i,2]
  
  ks[i,3]<- sum(x$w[1:i])
  
  ks[i,4]<- dummy + (ks[i,1]*(ks[i,2]/ks[i,1]))
 
  dummy <- ks[i,4]
}
  else{
  if(ks$V3[i-1]<=W){
  ks[i,1] <- W-ks$V3[i-1]
  ks[i,2] <- ks[i,1]/x$w[i]
  ks[i,3] <- ks[i-1,3]+ks[i,1]
  ks[i,4] <- ks[i-1,4]+ ks[i,2]
  break()
  }
  
}

}  

ks
}

set.seed(42)
n <- 2000 
knapsack_objects <-data.frame( w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000) )

start.time <- Sys.time()
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
