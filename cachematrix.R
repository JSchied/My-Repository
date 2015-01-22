## makeCacheMatrix is a function that creates a matrix that can cache its inverse.

## this function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() {
    x
  }
  
  setmatrix<-function(solve){ 
    m<<- solve
  }
  
  getmatrix<-function() {
    m
  }
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve is a function that tests to see if m has already been cached, and if it has not, it solves its inverse and caches it.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  return(m)
}