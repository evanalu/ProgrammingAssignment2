## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  do<-function(z){
    x<<-z
    a<<-NULL
  }
  obtain<-function() x
  doinv<-function(solve) a<<-solve
  obtaininv<-function() a
  q<-list(do=do,obtain=obtain,doinv=doinv,obtaininv=obtaininv)
  print(x)
  print(a)
  print(q)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 w<-x$obtaininv()
 if(!is.null(w)){
   message("getting cached data")
   return(w)
 }
 newdata<-x$obtain()
 w<-solve(newdata,...)
 x$doinv(w)
 print(w)
}

