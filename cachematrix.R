## Put comments here that give an overall description of what your
## functions do

## <summary>
## Function to cache a matrix and its inverse.
## Inverse is set to null every time matrix changes.
## </summary>
## <Returns>
## List of functions to manipulate matrix and inverse cache.
## </Returns>

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


## <summary>
## Function to solve and cache a matrix inverse.
## </summary>
## <Returns>Matrix inverse.</Returns>
## <param name="x">makeCacheMatrix "object"</param>

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
