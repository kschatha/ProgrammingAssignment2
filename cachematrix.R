## These 2 functions are used create a matrix and compute its inverse.
## Before computing the inverse, its checked if the matrix's inverse is stored
## the cache, and if yes then it is retrieved from cache else computed and stored
## in cache.

## makeCacheMatrix function is used to create a matrix and store the inverse
## of the given matrix in the cache.

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
      
  setMatrix <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
                                      
  getMatrix <- function()
  {
    x
  }
                                                              
  setInverse <- function(i)
  {
    inverse <<- i
  }
                                                                                      
  getInverse <- function()
  {
    inverse
  }
                                                                                                              
  list(setMatrix = setMatrix, getMatrix = getMatrix,
    setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is used to compute the given matrix's inverse. If the matrix's
## inverse is cached then its returned else it is computed here and saved.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
      
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
