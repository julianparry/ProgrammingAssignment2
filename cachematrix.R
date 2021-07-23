## When the output of the 'makeCache...' function is passed into the
## 'cacheSolve' function, it checks the cache and either returns a stored result
## or solves the matrix.

## This function:
## initialises the input (x) and inverse (inv) objects,
## defines four functions (set, get, setinverse, getinverse) that clear the
## previous values of the global objects and calculate them for the new input,
## and then assigns each function's output into a list for cacheSolve to use.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the output of the 'makeCache...' function.
## It calls the 'getinverse' function and checks if the output is NULL.
## It if is not NULL, it retrieves the result from cache. If it is NULL,
## it solves the matrix. Finally, it prints the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_data = x$get()
  inv <- solve(matrix_data, ...)
  x$setinverse(inv)
  inv
}
