## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix creates a matrix object that chaches its own inverse
##cacheSolve computes the inverse of the special matrix returned by the fucntion above
##If the inverse was already claculated with an unchanged matrix 
## the cacheSolve function will retrieve the existing inverse from the cache

## Creates a matrix that can cache its own inverse 

makeCacheMatrix <- function(x = matrix()) {
  special<-NULL
  setmatrix<-function(y){     ##set value of matrix
    x<<- y
    special<<-NULL
  }
  
  getmatrix<-function()x  ## Get value of matrix
  
  setinv<-function(inverse) special<<-special ##Set value of Inverse
  getinv<-function() special                  ##Get Value of Inverse
  
  list(setmatrix= setmatrix, setinv= setinv, getinv=getinv)

}

##Function computes the inverse of the matrix returned by the makeCache Matrix function
##If the inverse was already calc. and remains unchaged, then cacheSolve will return the inverse from the cache 
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  special<-x$getinv()
  
  if(!is.null(special)){
    message("getting cache data")
    return (special)
  }
  data<-x$getinv()
    special<-solve(data, ...)
    
    x$setinv(special)
    special
        ## Return a matrix that is the inverse of 'x'
}
