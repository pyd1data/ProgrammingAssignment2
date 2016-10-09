## These two functions allow to speed up the computation of a matrix M-inverse if
## it was already calculated once before. First we compute an intermediate matrix, a special matrix
## named CachedMatrix. CachedMatrix will store the M value in cache:
## CachedMatrix <- makeCacheMatrix(M)
## Then we call the inversion of M using this intermediate special matrix:
## cacheSolve(CachedMatrix)
## At first call, the output is the inverse of M, which is computed and stored in cache, in CachedMatrix.
## Following calls detect if the inverse is already computed. If so, they produce the message "getting cached data" and
## gather the inverse out of the cache.

## Given a matrix M, makeCacheMatrix is a function that computes a special matrix named CachedMatrix.
## CachedMatrix$get() shows M

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a special matrix makeCacheMatrix(M),
## cacheSolve evaluates the inverse of M.
## CachedMatrix$getinverse() shows it.
## Following calls detect if the inverse is already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
