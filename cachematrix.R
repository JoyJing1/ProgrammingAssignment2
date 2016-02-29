## Goal: Write a pair of functions that cache the inverse of a matrix
#        so that if the inverse has already been calculated, we can
#        call on the cached inverse rather than re-calculating.

# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing functions to:

#1. Set the value of the matrix (set)
#2. Get the value of the matrix (get)
#3. Set the value of the inverse (setinverse)
#4. Get the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      get <- function() {x}
      setinverse <- function(inverse){i <<- solve(x)}
      getinverse <- function(){i}
      list(set=set, get=get,
            setinverse = setinverse,
            getinverse = getinverse)
}



## The second function calculates the inverse of the "special matrix"
# from the first function. It checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips 
# the computation. If not, it calculates the inverse and sets the inverse 
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      # Check cache "matrix" to see if we've already calculated inverse
      # If so, pull cached inverse
      i <- x$getinverse()
      if(!is.null(i)){
            message("Getting cached data")
            return(i)
      }
      
      #If not, calculate inverse and set the inverse in the cache
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

