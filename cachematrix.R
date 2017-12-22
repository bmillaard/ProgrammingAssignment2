## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## Below are a couple of functions which we have used to create a function that can cache the inverse of a matrix 
## and a special object that can cache it's inverse for the input 

## We will write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <-function(y) {
            x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

 
## makeCacheMatrix: This function computes a special matrix object that can cache it's inverse. This is created by the function above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## cacheSolve is a function which computes the inverse of the special matrix which is created with the above function. 
## If the inverse already has been calculated then the caheSolve is supposed to retrieve the inverse from the cache
        
