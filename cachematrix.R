# Matrix inversion is usually a costly computation and their may be some benefit to
# caching the inverse of a matrix rather than compute it repeatedly.

## Write a short comment describing this function
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  get <- function() x

  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i

  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# Example:
# x = matrix(c(0,1,1,0), 2)
# c = makeCacheMatrix(x)
# c$get()

# This function returns the inverse of a matrix. It checks if
# the inverse has been cached before.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached vector...")
    return(i)
  }

  d <- x$get()
  i <- solve(d)
  x$setinverse(i)
  i
}
# Example:
# x = matrix(c(0,1,1,0), 2)
# c = makeCacheMatrix(x)
# c$get()
# cacheSolve(c)
