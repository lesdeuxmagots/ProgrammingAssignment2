## The overall goal of this script is to create a matrix that can cache its inverse

## The following function sets the value of a matrix, gets the value of a matrix, 
## sets the value of the inverse, and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL
      
      set <- function(y) {
            x <<- y
            y <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(solve) i <<- solve
      
      getinverse <- function() i
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse) 
}


## The following function calculates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the solve function.

cacheSolve <- function(x, ...) {
      
      i <- x$getinverse()
      
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      data <- x$get()
      
      i <- solve(data, ...)
      
      x$setinverse(i)
      
      i #returns inverse
}
