#Here are the two functions that can be used to cache the inverse of a matrix

#The makeCacheMatrix function creates a special “matrix” object that can cache its inverse. It returns a list of four functions:
  
#set: This function is used to set the value of the matrix.
#get: This function is used to get the value of the matrix.
#setInverse: This function is used to set the value of the inverse of the matrix.
#getInverse: This function is used to get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#The cacheSolve function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cacheSolve should retrieve the inverse from the cache. 
#It returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
 