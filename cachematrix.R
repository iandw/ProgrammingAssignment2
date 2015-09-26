#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#Input: an invertible matrix x

#Output: a list consisting of 4 parts:

#1. createMatrix
#2. getMatrix
#3. calculateInverse
#4. getInverse

#This list is then used as input into the second function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  X_inverse <- NULL
  createMatrix <- function(y) {
    x <<- y
    X_inverse <<- NULL
  }

  getMatrix <- function() x
 
  calculateInverse <- function(inverse) X_inverse <<- inverse
 
  getInverse <- function() X_inverse
 
  list(createMatrix=createMatrix,
       getMatrix=getMatrix,
       calculateInverse=calculateInverse,
       getInverse=getInverse
       )
 
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then cachesolve retrieves the inverse from the cache.
#Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
  #Get the inverse from the list 
  X_inverse <- x$getInverse()
 
  #If the inverse has already been calculated (i.e. it's no longer NULL)
  if(is.null(X_inverse)!=TRUE) {
    print("getting inverse from the cache")
    return(X_inverse)
  }
 
  else { #If the inverse has not already been calculated, calculate the inverse
  matrix_to_inverse <- x$getMatrix()
  X_inverse = solve(matrix_to_inverse, ...)
  }
 
  #Set the value of the inverse in the cache
  x$calculateInverse(X_inverse)
  X_inverse
 
}
