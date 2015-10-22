## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  
  invMatrix <- NULL
  
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  ##Output Function
  getMatrix <- function() {
    x
  }
  #setting inverse Matrix
  setInverseMatrix <- function(inv){
    invMatrix <<- inv
  }
  ## Get Inverse Matrix
  getInverseMatrix <- function() {
     invMatrix
  }
 
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m1 <- x$getInverseMatrix()
 
  if(!is.null(m1) ) {
    message("Getting Matrix from the Cache...")
    return(m1)
  }
  m2 <- x$getMatrix()
  m1 <- solve(m2)
  x$setInverseMatrix(m1)
  return(m1)
}
