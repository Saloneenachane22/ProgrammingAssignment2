## Function creates a matrix that caches the inverse of the matrix

## The function creates a special matrix which is a list containing a function to 
## - set the matrix value
## - return the matrix
## - sets value of the inverse of the matrix
## - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setmat = setmat, getmat = getmat,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special matrix created above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cache data")
    return(inv)
  } ## Retrieves the cached inverse if matrix is unchanged 
  matI <- x$getmat()
  inv <- solve(matI,...)
  x$setInverse(inv)
  inv  ## Returns the new inverse of the matrix
}
