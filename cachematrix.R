## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

 ##set the value of the matrix
 ##get the value of the matrix
 ##set the value of the matrix inverse
 ##get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(mat_inverse) inverse <<- mat_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of special "matrix"

cacheSolve <- function(x, ...) {
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  matrix <- x$get()
  m_inverse <- solve(matrix, ...)
  x$setinverse(m_inverse)
  ## Return a matrix that is the inverse of 'x'
  m_inverse
}
