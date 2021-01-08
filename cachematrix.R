##  These functions saves the inverse of a matrix in a separate environment than
## the current environment and uses the cached inverse for subsequent calculation
## saving time and memory.

## The following function takes a matrix and save it and its inverse in a cache
## or a separate environment.

makeCacheMatrix <- function(x = matrix()) {
                  m <- NULL
                  set <- function(y) {
                    x <<- y
                    m <<- NULL
                  }
                  get <- function() x
                  setinverse <- function(solve) m <<- solve
                  getinverse <- function() m
                  list(set = set, get = get, setinverse = setinverse,
                       getinverse = getinverse)
}


## The following function tries to get the inverse from the cache  which is 
## saved by the previous function and returns it. If it finds no cached value of
## inverse, then it calculates it and returns the inverse. Thus, it saves time for
## computation of the inverse.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
         } ## Return a matrix that is the inverse of 'x'
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
       
}
