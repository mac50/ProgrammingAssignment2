## This assignment consists of a closure to store and retrieve a matrix and its inverse, 
##and a companiion function that actually computes the inverse if it has not yet been done.
##MSF 4/1/2016

## makecacheMatrix is a closure that contains four functions for matrix storage and retrival
##note that as a closure it will store both the matrix and its inverse since the executing environment
## is persistent as long as a particular instantiation exists. The four functions are set() to set the value
## of the matrix.  get fetches the matrix, Setinvere stores the value of the inverse and get inverse retrieves
##it. Note that the matrix inverse is not computed by makeCachematrix, it simply sets the inverse value to null
##when the matrix is intially stored. It depends on cachesolve() to find the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}

## This function is used in conjunction with makeCacheMatrix
## It retrieves the value of the cache inverse from makeCacheMatrix
## if the value is null it computes the inverse ans stores it via
## makeCacheMatrix setinverse function, otherwise it just displays
## inverse value along with a message that it was retrieved from
#the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinvers(inv)
      inv
}
