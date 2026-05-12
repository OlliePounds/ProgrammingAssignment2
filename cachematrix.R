## These functions create a special matrix object that can cache its inverse
## to avoid recomputing it multiple times.

## This function creates a special "matrix" object that can store a matrix
## and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         #cached inverse
    
    set <- function(y) {
        x <<- y         # assign new matrix
        inv <<- NULL    # reset cached inverse
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes (or retrieves) the inverse of the cached matrix.
## If the inverse is already cached, it returns it directly.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)     # compute inverse
        x$setinverse(inv)           # cache the inverse
        
        inv
}
