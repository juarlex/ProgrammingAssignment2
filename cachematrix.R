# Matrix inversion is usually a costly computation and their may be some benefit to
# caching the inverse of a matrix rather than compute it repeatedly.

# makeCacheMatrix: This function creates a matrix for the input of the
# cacheSolve() function.
makeCacheMatrix <- function(original.matrix = matrix()) {
  
  if (!is.matrix(original.matrix)) {
    stop("Please give me a matrix")
  }
  
  inverted.matrix <- NULL
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() original.matrix
  
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}
# Example:
# x = matrix(c(0,1,1,0), 2)
# c = makeCacheMatrix(x)
# c$get()
# 
#       [,1] [,2]
# [1,]    0    1
# [2,]    1    0

# cacheSolve: This function returns the inverse of a matrix. It checks if
# the inverse has been cached before.
cacheSolve <- function(cacheable.matrix, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()
  
  if(!is.null(inverted.matrix)) {
    message("Getting cached inverted matrix...")
    return(inverted.matrix)
  }

  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix
}
# Example:
# x = matrix(c(0,1,1,0), 2)
# c = makeCacheMatrix(x)
# c$get()
# cacheSolve(c)
#       [,1] [,2]
# [1,]    0    1
# [2,]    1    0
#
# cacheSolve(c)
# Getting cached inverted matrix...
#        [,1] [,2]
# [1,]    0    1
# [2,]    1    0
