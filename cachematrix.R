## These two functions below will create a square invertible matrix
## and make the inverse of the matrix available in the cache environment


## This function creates a list of functions used by cacheSolve to initially set, or to retrieve 
## the inverted matrix in the cache environment.

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv, 
           getinv = getinv)
      
}


## The cacheSolve function calculates the inverse of the matrix returned by the function makeCacheMatrix. 
##If the inverse has previously been calculated and hasn't changed, cacheSolve will just use the cached result and indicate such.

cacheSolve <- function(x, ...) { 
      s<- x$getinv()
      if(!is.null(s)) {
            message("cached data will be used")
            return(s)
      }
      data <- x$get()
      t<- solve(data, ...)
      x$setinv(t)
      t
      
}