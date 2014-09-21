## TWo functions which can be used together to set a matrix and return its
## inverse.  Subsequent calls to return the inverse will return a cached
## value, saving the need to calculate it again


## Function to return a list of functions to set a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialise cached inverse matrix to null
    i <- NULL
    
    
    ## define function to set new matrix and reset cached inverse matrix to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    
    ## define function to return current matrix
    get <- function() x
    
    
    ## define function to set new cached inverse matrix
    setinverse <- function(inverse) i <<- inverse
    
    
    ## return currently cached inverse matrix
    getinverse <- function() i
    
    
    ## return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



## Function to return inverse matrix for list of type makeCacheMatrix
##
## The first time the function is called for a given matrix, the 
## result is cached.  Subsequent calls for the same matrix will 
## return the cached result

cacheSolve <- function(x, ...) {
    
    ## lookup current cached inverse matrix
    i <- x$getinverse()
    
    
    ## if not null, return current cached matrix and exit function
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    
    ## lookup current matrix
    data <- x$get()
    
    
    ## calculate inverse matrix and update cached value
    i <- solve(data, ...)
    x$setinverse(i)
    
    
    ## Return a matrix that is the inverse of 'x'
    i
    
}
