## functions in this file calculate the inverse of a matrix
## 1. call makeCacheMatrix (matrix) and assign the return value to someVariable
## 2. call solveCache(someVariable)


## MakeCacheMatrix initializes a "wrapper object" (actually it is a string)
##which stores the matrix a serves as a cache for inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  ##variables in the environment of particular instance of makeCacheMatrix function 
  inverseMatrix <- NULL
  
  ##nested functions for getting and setting the underlying matrix
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  
  ##nested functions for getting/setting inverse of the matrix from/to cache 
  ##cache is in reality the environment of makeCacheMatrix
  setInverseMatrix <- function(y) inverseMatrix <<- y
  getInverseMatrix <- function() inverseMatrix
  
  ##return value: quasi object in form of a list of functions
  list(set = setMatrix, get = getMatrix,
       setInv = setInverseMatrix, getInv = getInverseMatrix) 
}



## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##tries to load the inverse of a matrix from "cache"
  matrix_inversed <- x$getInv()
  if(!is.null(matrix_inversed)) {
    message("getting cached data")
    return(matrix_inversed)
  }
  
  ##it did not find the inverse matrix in "cache"
  ##compute the inverse and put in into "cache" e.g. environment of makeCacheMatrix function
  matrix_plain <- x$get()
  matrix_inversed <- solve(matrix_plain)
  x$setInv(matrix_inversed)
  
  ##return value
  matrix_inversed
}
