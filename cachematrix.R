## Matrix inversion is usually a costly computation and there
## may be instances (such as in loop calculations) that it could be
## beneficial caching its inverse, rather than computing it repeatedly.
## The two below functions can be used to accomplish this task.


## The "makeCacheMatrix" function creates a special "matrix" object
## that can cache its inverse. The "matrix" object is passed to the
## function using the parameter x.
##
## The function contains a list of four functions that help with
## the following:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
    ## track the inverse matrix value here
    inv <- NULL
    
    ## set function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get function
    get <- function() x
    
    # setinverse function
    setinverse <- function(inverse) inv <<- inverse
    
    # getinverse function
    getinverse <- function() inv
    
    # function list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix function above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve function
## should retrieve its inverse from the cache without the need to calculate
## it again.

cacheSolve <- function(x, ...) {
    
    ## get the inverse of the passed matrix
    inv <- x$getinverse()
    
    ## check if the inverse is null or not
    if(is.null(inv)) {
        ## calculate and cache inverse of 'x' matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
  
    ## Return the inverse of the 'x' matrix
    inv
}
