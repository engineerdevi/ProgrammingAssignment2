## makeCacheMatrix function creates a special "matrix" 
##object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     
     invMatrix <- NULL
     
     ## a function to set invMatrix to Null, if inversing the matrix is happening first time
     setMatrix <- function(y){
          x <<- y
          invMatrix <<- NULL
     }
     
     ##getting X matrix Function
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
     
     ##Special list of functions
     list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, 
          getInverseMatrix = getInverseMatrix )
}


## cacheSolve function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     m1 <- x$getInverseMatrix()
     
     ##if not first time get it from the cache
     if(!is.null(m1) ) {
          message("Getting Matrix from the Cache...")
          return(m1)
     }
     
     ##otherwise do inverse and return
     m2 <- x$getMatrix() ##get new matrix
     m1 <- solve(m2) ##inverse function applied
     x$setInverseMatrix(m1) ##setting the list's invMatrix
     return(m1)
}

## Output

##> x = rbind(c(5, -1/2), c(-1/2, 5))
##> m = makeCacheMatrix(x)
##> cacheSolve(m)
##           [,1]       [,2]
##[1,] 0.20202020 0.02020202
##[2,] 0.02020202 0.20202020
##> source("Cachematrix.R")
##> m = makeCacheMatrix(x)
##> m$getMatrix()
##     [,1] [,2]
##[1,]  5.0 -0.5
##[2,] -0.5  5.0
##> cacheSolve(m)
##           [,1]       [,2]
##[1,] 0.20202020 0.02020202
##[2,] 0.02020202 0.20202020
##> m$getMatrix()
##     [,1] [,2]
##[1,]  5.0 -0.5
##[2,] -0.5  5.0
##> cacheSolve(m)
##Getting Matrix from the Cache...
##           [,1]       [,2]
##[1,] 0.20202020 0.02020202
##[2,] 0.02020202 0.20202020
##> 
