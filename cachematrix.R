## This function creates a special matrix and contains sub-functions to set or get elements and inverse of a special matrix 
## These sub-functions function are stored in a list which can be called on command line with a special matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## This function will compute the inverse of special matrix created via makeCacheMatrix function. This function will check if inverse has already been cached, if yes then cached value is retuned. 
## And if not then inverse is computed and returned.

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
