## This function creates a special "matrix" object that can cache its inverse.   

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## function that sets the matrix the user entered.
  setMatrix <- function(matrix)    m <<- solve(matrix) 
  ##function that returns the user inputed matrix
  getMatrix <- function() x
  list( getMatrix = getMatrix, setMatrix = setMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if (!is.null(m)){
      message("getting cached data")
      return(m)
    }
    newMatrix <- x$get()
    m <- mean(newMatrix , ...)
    x$setMatrix(m)
    m
}
