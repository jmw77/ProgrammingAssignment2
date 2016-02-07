## These functions are used to compute and cache the inverse of a matrix.
## If the inverse has been cached, cacheSolve.R will retrieve this matrix.
## If no inverse has been cached, cacheSolve.R will compute the inverse and cache the matrix.

## This function creates a special matrix. 
## Really this is a list containing a function.
## 1. It sets the values of the matrix.
## 2. It gets the values of the matrix.  
## 3. It sets the inverse of the matrix.
## 4. It gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix created
## with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
