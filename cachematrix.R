## These functions cache the potentially time-consuming computation of the inverse of a matrix.

## This function (makeCacheMatrix) creates a special "matrix", a list with a function to:
##1) define the matrix
##2) get the matrix
##3) define the inverse of the matrix using solve()
##4) get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function (cacheSolve) checks to see if the matrix inverse has already been calculated. 
## If so, it gets the inverse from the cache. 
## If not, it calculates the inverse of special "matrix" created by the makeCacheMatrix function,
## and caches it using the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
