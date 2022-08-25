## this is an assigment in the week 3 of the coursera R programming course
## function makeCacheMatrix creates a matrix, sets and gets the value of the matrix
## and sets and gets the value of the inverse of that matrix if computable

makeCacheMatrix <- function(x= matrix()) {
  m <- NULL ## initialise to NULL, as new matrix is created
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the cacheSolve function either calculates the inverse of the matrix if not previously cached,
## or if the check for previous calculated value (is not null) returns an existing value
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { ## check for previous cached matrix 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)  ## Return a matrix that is the inverse of 'x'
  m
}
