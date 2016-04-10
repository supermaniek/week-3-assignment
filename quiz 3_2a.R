makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix  x 
  
  ##  m is the cache
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to var x in the parent environment
    m <<- NULL ## re-initialisation of m
  }
  get <- function() x # return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
  ## to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}