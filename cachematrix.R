## These functions take advantage of lexical scoping rules to create a special matrix object
## that can cache the result of calculating the inverse of the matrix

## Creates a special matrix object containing a list of functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special matrix. If the inverse has already been calculated it 
## returns the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(x$get())
  x$setinverse(i)
  i
}
