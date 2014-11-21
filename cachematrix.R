## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix which is
## just a list of functions that allows one to 
## set the value of the matrix, get the value of the matrix
## set the inverse of the matrix and get the inverse of
## the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list (set = set, get=get,
        setInv = setInv, getInv = getInv)
}


## Used with the function above to cache the inverse
## if it is not calculated and retrieve it if it is

cacheSolve <- function(x, ...) 
{
        inv <- x$getInv()
        if(!is.null(inv))
        {
          message("getting cached data")
          return(inv)
        }
        xMat <- x$get()
        inv <- solve(xMat)
        x$setInv(inv)
        inv
}
