##
## Author: Jim Lombardi
## Original: 22-MAR-2015
##
## The following pair of functions can be used to calculate and cache the inverse of a supplied matrix.
## Although for small matrices, this doesn't seem particularly useful, for much larger matrices,
## the calculation can be expensive (time consuming).  If the inversion is to be used repeatedly,
## it's nice to have it available without re-calculating.
##
## Example usage
##   1. Set up the original matrix - in this case, a 15x15 matrix of random integers:
##        A <- matrix(sample.int(225),15,15)
##
##   2. Define the caching functions on A:
##        Ac <- makeCacheMatrix(A)
##
##   3. Calculate and cache the inverse of A, using the functions defined in Step 2:
##        cacheSolve(Ac)
##
##   If you run Step 3 more then once, you will see that the first run calculates the inverse ancaches it.  Subsequent runs in
##   same session retrieve the inverse from the cache without re-calculating it.


## The first function, makeCacheMatrix, defines a set of functions on the matrix object supplied in its arguement.  
## These functions enable the caller to:
##   - Cache the original matrix (set)
##   - Retrieve the original matrix from the cache (get)
##   - Calculate and cache the inverse of the original matrix (setInverse)
##   - Retrieve the cached version of the inverse matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function, cacheSolve, returns the inverse of its argument.  If has not been calcualted in the current session,
## that calculation is done (using the R "solve" function), and the result is cached.  If the calculation has already been done,
## the inverse is retrieved from cache without re-calculation.  In the latter case, a message is printed indication the cache retrieval.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if ( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
