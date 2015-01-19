
## This function is pretty similar to what was provided.
## Main thing is changing the input type, to a matrix.
## And renaming function names as appropriate.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL		# reset m cache
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse	# double arrow here, to set m in the parent environment
      getinverse <- function() m
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # return the methods as a list
}

## This method returns the inverse of a square matrix.
## It checks if a cache value is in memory first,
## if not, it proceeds to compute the value.
cacheSolve <- function(x, ...) {
  
      m <- x$getinverse()
      if(!is.null(m)) {		# If obtained before, retrieve from memory
          message("getting cached data.")
          return(m)
      }
      data <- x$get()		# Retrieve original matrix
      m <- solve(data, ...) # Return a matrix that is the inverse of 'x'
      x$setinverse(m)		# Set in object for further use
      m
}


## Tester methods
newM <- matrix(runif(50*50), ncol=50) 
i <- makeCacheMatrix(newM)
cacheSolve(i)
