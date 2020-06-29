## Put comments here that give an overall description of what your
## functions do
##"makeMatrix" and "cacheinverse" that cache the inverse of a matrix
##"makematrix" creates a special "matrix" that caches its inverse for the input
makeMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function




## Write a short comment describing this function
##solves the inverse of the special "matrix" created above
cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
