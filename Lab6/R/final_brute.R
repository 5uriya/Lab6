
#' Brute Force Algorithm 
#'
#' Knapsack problem is solved using brute force algorithm.
#'
#' @param x is a data frame containing weights and values of the elements.
#' @param W is the highest weight capacity.
#' 
#'
#' @return output is the maximum value the knapsack can hold for the given weight capacity
#' @export
#'
#' @examples brute_force(x = knapsack_objects[1:5,], W = 150)
#' 
#' @references \href{https://en.wikipedia.org/wiki/Knapsack_problem}{Dynamic Algorithm}
#'  









brute_force_knapsack<-function(x,W)
{
  stopifnot(is.data.frame(x)== TRUE && is.numeric(W)== TRUE)  
  stopifnot(x[[1]]>0 && x[[2]]>0 && W>0)
  
    A <- rep(FALSE, n)
    B <- rep(FALSE, n)
    n<-nrow(x)
    highest_value<-0
    corresponding_elements<-c()
    highest_value1<-0
    corresponding_elements1<-c()
    for (i in 1:(2^n))
    {
      j<- 1
      tw<-0
      tv<-0
      
      while (A[j]!=FALSE && j < n)
      {
        A[j]<-FALSE
        j <- j+1
      }
      A[j]<-TRUE
      for(k in 1:n)
      {
        if(A[k] == TRUE)
        {
          tw<-tw+x$w[k]
          tv<-tv+x$v[k]
        }
      }
      if((tv > highest_value )&& (tw <= W))
      {
        highest_value<-tv
        corresponding_elements<-A
      }
      
      
      
      j1<- n
      tw1<-0
      tv1<-0
      
      while (B[j1]!=FALSE && j1 > 0)
      {
        B[j1]<-FALSE
        j1<- j1-1
      }
      B[j1]<-TRUE
      for(k in 1:n)
      {
        if(B[k] == TRUE)
        {
          tw1<-tw1+x$w[k]
          tv1<-tv1+x$v[k]
        }
      }
      if((tv1 > highest_value1 )&& (tw1 <= W))
      {
        highest_value1<-tv1
        corresponding_elements1<-B
      }
      
     
      
      
      
      
    }
    if(tw1<tw)
      return(list(value=highest_value, elements=(1:n)[corresponding_elements]))
    else
      return(list(value=highest_value, elements=(1:n)[corresponding_elements]))
    
    
    
}
set.seed(42)
n <- 2000 
knapsack_objects <-data.frame( w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000) )

start.time <- Sys.time()
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


