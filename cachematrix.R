## create an inversable matrix
makeCacheMatrix <- function(m = matrix()) {
  
  invs <- NULL
  
  set <- function(matrix) {
    m <<- matrix
    invs <<- NULL
  }
  
  get <- function() m
  
  setInverse <- function(inverse) invs <<- inverse
  getInverse <- function() invs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Inverse the matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  
  ## Compute the Inverse of the Matrix
  m <- x$getInverse()
  
  # If the inverse is already calculated, return it 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix 
  data <- x$get()
  ## Calculate the inverse 
  m <- solve(data, ...)
  ## Cache the inverse 
  x$setInverse(m)
  ## Return it 
  m
}
