## Function that cache the inverse of a matrix

## Create a special "matrix", which is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above function
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m    
}
