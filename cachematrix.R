## makeMatrix takes as input a matrix and returns a list which contains the functions to:
## set()= set the values of the matrix
## get()= get the values of the matrix
## setInverse()= set the values of the inverted matrix
## getInverse()= get the values of the inverted matrix
## Returns a list object with the mentioned functions


makeMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

##cacheSolve takes as input, the output of the makeMatrix function
##If the inverse has already been calculated it gets the inverted matrix
## from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix and 
## sets the inverted matrix in the cache via the setMatrix function

cacheSolve <- function(x, ...) {      
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMatrix(m)
  m
}
