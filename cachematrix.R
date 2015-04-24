## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(theMatrix = matrix()) {
  inverse <- NULL #Initial the result
  set <- function(x) {
    theMatrix <<- x;
    inverse <<- NULL;
  }
  get <- function() return(theMatrix);
  setinverse <- function(inv) inverse <<- inv;  #calculates the inverse
  getinverse <- function() return(inverse);  #returns the inverse
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## 2. cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(theMatrix, ...) {
  inverse <- theMatrix$getinverse()
  if(!is.null(inverse)) {
    message("Getting data from the cache...")
    return(inverse)
  }
  dataset <- theMatrix$get()
  inverse <- solve(dataset, ...)
  theMatrix$setinverse(inverse)
  return(inverse)
}
