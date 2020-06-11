## Given the example in the assignment's instructions, I basically replaced the 
## mean function in the example to the solve function and changed variable and
## function names accordingly

## MakeCacheMatrix creates the special "matrix" as a list of functions that set 
## the matrix value, gets its value, sets the value of its inverse matrix and 
## gets the the value of its inverse matrix.

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


## cacheSolve calculates the inverse matrix of the special "matrix" created by 
## the MakeCacheMatrix function if it hasn't been calculated yet. If it has 
## already been calculated, it skips the calculation and retrieves it from the
## cache. If it hasn't been calculated yet, the function calculates the inverse
## matrix using the solve function and sets it as the inverse value in the cache
## using the setinverse function.

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
