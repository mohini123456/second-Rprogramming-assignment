##Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
#Note: For this assignment, assume that the provided matrix  is always invertible.
#1.creates a special "matrix", which is really a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(invrs) I <<- invrs
  getinverse <- function() I
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#2.This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
# A <- matrix(c(1,5,6,4),2,2)
# B <- makeCacheMatrix(A)
# C <- cacheSolve(B)
#C <- round(cacheSolve(B),3)
# C
