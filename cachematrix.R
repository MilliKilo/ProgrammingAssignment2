## There are two functions defined here:
## The first one (makeCachematrix) creates a special object (matrix) that stores a matrix
## and the second one (cacheSolve) caches the inverse of the matrix.
## This code assumes the matrices provided will be invertible.

## The function makeCachematrix creates a special matrix which is a list containing
## functions to:
## 1) Set the value of a matix
## 2) get the value of a matix
## 3) solve the matix
## 4) solve (get the inverse of) the matix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     x <- x
     set <- function(y) {
          x <<- y    ## pointer to the cached matrix.  x (cached matrix) is assigned a global value, 
                     ## unchanged between makeCacheMatrix and cacheSolve.
          m <<- NULL
     }
     
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() { 
          message("getting inverse") 
          return(m)}
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## create the list 
     ## of functions representing the special matrix
}

## The second function, cacheSolve, calculates the mean of the special matrix.  It 
## first checks to see if the matrix inverse was already calculated or not.
## If it was already calculated, the function returns the value from the cache.  Else
## it determines the inverse and caches the newly created inverse of the matrix.

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x
                                   
     m <- x$getinverse()
     if (!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinverse(m)               ## x is the inverted and the Cached matrix.
     m
}
