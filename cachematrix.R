##This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(size) {
  M <- matrix(nrow=size, ncol=size)
  for (i in 1:size)
    for (j in 1:size)
      M[i,j] <- solve(i,j)
  return(M)
}
  
  function(size) {
  m <- NULL
  set <- function(y) {
    size <<- y
    m <<- NULL
  }
  get <- function() size
  setmatrix <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
}

##This function computes the inverse the the matrix returned by makeCacheMatrix.  If the 
##inverse has already been calculated, and not changed, cacheSolve will retrieve the inverse
##from the cache.
cacheSolve <- function(size, ...) {
  m <- size$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- size$get()
  m <- solve(data, ...)
  size$setinverse(m)
  m
}

