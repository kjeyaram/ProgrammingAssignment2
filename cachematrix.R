## makeCacheMatrix and cacheSolve matrix are used to create a matrix object, cache
## the inverse value of a square invertible matrix and access the inverse value 
## without having to execute the inverse operation again
##
## In order to understand how the functions work
## a. Create 2 square matrices, say tm1 and tm2
## b. Create and cache special matrix objects for matrix inverse mcm1 and mcm2
##    mcm1 = makeCacheMatrix(tm1)
##    mcm2 = makeCacheMatrix(tm2)
## c. Compute the inverse for the 2 matrices using their respective special matrix
##    objects
##    cacheSolve(mcm1)
##    cacheSolve(mcm2)
## d. At the first execution, the inverse is calculated and the user defined 
##    message "getting cached data" is NOT displayed. However, when cachSolve(mcm1)
##    or cacheSolve(mcm2) is executed subsequent times, the user defined message 
##    "getting cached data" is displayed. This indicates that the "if" evaluated 
##    was true and that the inverse was got from the cache
## e. (d) holds true as long as mcm1 and mcm2 do not change
##
##
## makeCacheMatrix : This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL  
  set<- function(y) {
    x<<- y
    matinv<<- NULL
  }
  get<- function () x
  setinv<- function(solve) matinv<<- solve
  getinv<- function() matinv  
  list (set = set, get = get, setinv = setinv, getinv = getinv)  
}

## cacheSolve : This function computes the inverse of the special "matrix object" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  if (!is.null(matinv)) {
    message ("getting cached data")
    return (matinv)
  }
  inverse <- x$get()
  matinv <- solve(inverse,...)
  x$setinv(matinv)
  matinv
  ## Returns a matrix that is the inverse of 'x'
}