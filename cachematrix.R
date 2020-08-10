## This program contain a pair of functions that cache the inverse of a matrix.

##This first function creates a special "matrix" which is a list containing a 
##function to:
##1. Set the value of the matrix
##2. Get the value of the matrix
##3. Set the value of the inverse
##4. Get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(z) m <<- z
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function returns the inverse of the special "matrix" created with the 
##above function.

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  return(m)
}
