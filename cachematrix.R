#cache the inverse of matrix to avoid unnecessarily long computations

## a) get,set values of vector(x matrix in this case)
## b) get,set values of values of inverse of vector(x matrix in this case)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  seti<-function(solve) m<<- solve
  geti<-function() m
  list(set=set, get=get,
       seti=seti,
       geti=geti)

}


## returns inverse of matrix required. if already computed, return the value(saving time) else computes and returns.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$geti()
    if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$seti(m)
  m
}
