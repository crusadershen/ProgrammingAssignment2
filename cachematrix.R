## These two functions solve the inverse of a matrix and cache the results for future access

## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## The inverse of matrix x is default as "Null"
  set <- function(y) {
    x <<- y ## Assign value to create matrix x
    inv <<- NULL ## Default value of "inv"
  }
  get <- function() x ## Retrieve x value
  setInverse <- function(inverse) inv <<- inverse ## Assign the inverse calculated to "inv" and cache it
  getInverse <- function() inv ## Retrieve the inverse of matrix x
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ## Retrieve the inverse value "inv"
  if(!is.null(inv)) { ## Test whether the inverse has been cached
    message("getting cached data") ## Retrieving cached inverse value
    return(inv) ## Return the inverse value and terminate function
  }
  data <- x$get() ## In case of first-time calculation of the inverse, retrieve x value first
  inv <- solve(data, ...) ## Calculate the inverse of x
  x$setInverse(inv) ## Cache the inverse value for future access
  inv ## Return the inverse value and terminate function
}
