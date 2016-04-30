## makeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL             
  setMatrix <- function(y) {
    x <<- y                             
    m <<- NULL                      
  }
  getMatrix <- function() x
  setInversematrix <- function(matirx) m <<- matrix    
  getInversematrix <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInversematrix = setInversematrix,
       getInversematrix = getInversematrix)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInversematrix(m)
  m
}

