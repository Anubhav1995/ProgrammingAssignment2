## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as input and it creates a list containing set, get, setInverse and 
## getInverse function.This object is then passed to cacheSolve during its call.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function receives the matrix object(list of functions created by above function)
## and checks if inverse exists or not .If it exists then it returns cached data otherwise 
## it sets the value of inverse in the inv variable and then passses it to the setInverse func
## so that value of inv is updated there for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
