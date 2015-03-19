## These functions invert a matrix and cache it to avoid re-inverting the same matrix if 
## later required

## creates a vector of function that sets the value of the matrix, gets the value of the matrix, 
## sets the value of the inverse matrix, gets the value of the inverse matrix

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


## calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated

cacheSolve <- function(x, ...) {
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
