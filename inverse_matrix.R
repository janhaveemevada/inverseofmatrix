makeCacheMatrix <- function(x = matrix()) #Take matrix on which you want inverse
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL # check if inverse than get matrix or return NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #Assign inverse matrix value
  getInverse <- function() inv # get the inverse matrix value
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}
cacheSolve <- function(x, ...) 
{
  ##inverse matrix that is the of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}