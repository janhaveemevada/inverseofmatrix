makeCacheMatrix <- function(x = matrix()) #Take matrix on which you want inverse
{
  inv <- NULL # initialize the cache 
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL # when the matrix set after will clear the cache
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #set the cache 
  getInverse <- function() inv # get the  matrix value
  # list with four functions
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}
## function:computes the inverse of the "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) 
{
  ##check inverse matrix that is the of 'x' calculted
  inv <- x$getInverse()
  if (!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  # set the inverse matrix data to cache 
  x$setInverse(inv)
  inv
}
