
##This function creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
      x<<-y
      m<<-NULL
  }   ##Set the values of the matrix
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix) ##Get the values of the matrix inverse
}

##Computes the inverse of the matrix returned by the previous function. cacheSove retrives the inverse from the makeCacheMatrix if it has already been calculated
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("cached data being retrieved")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
