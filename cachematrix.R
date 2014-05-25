## makeCacheMatrix acts as an object for accessing getter/setter methods
## for the inverse of a matrix cached for easy access

## makeCacheMatrix: Makes the inverse matrix and stores it in easily accessible way

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() {
    inverseMatrix
  }
  setInverse <- function(inverseMatrixInput) {
    inverseMatrixInput <- as.matrix(inverseMatrixInput)
    inverseMatrix <<- inverseMatrixInput
  }
  getInverse <- function() {
    inverseMatrix
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Stores the solution to the inverse matrix in easily accessed memory

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  m <- x$get()
  invMatrix <- solve(m)
  x$setInverse(invMatrix)
  invMatrix
}
