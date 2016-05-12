## Matrix inversion is a costly computation, therefore it can be 
## benefical to cache the inverse of matrix. This can be achieved 
## using the functions below. below 

# Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# Computes the inverse of the matrix returned by make Cache Matrix
# If the inverse has already been calculated cacheSolve retrieves the
# inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
