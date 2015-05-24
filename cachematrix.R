## @author Nagendra Kumar
## @Coursera Assignment work

## PROBLEM STATEMENT
## CACHE THE CONTENT OF INVERSE OF MATRIX

## makeCacheMatrix  creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
     x <<- y
     inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_m <<- inverse
  getinverse <- function() inverse_m
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The below function checks whether inverse exists
## If inverse exists it returns the result otherwise
## it calculates the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  data <- x$get()
  inverse_m <- solve(data, ...)
  x$setinverse(inverse_m)
  inverse_m
}
