## Computation of inverse is an intensive operation
## The following functions make use of a cache to determine
## if the inverse has already been calculated.
## If so, it skips computation and returns the inverse from cache

## The first Function makeCacheMatrix takes a matrix argument
## sets the matrix and returns 
## a list containing functions to
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matix()) {
        if(!class(x) == "matrix") stop("arg passed is not a matrix")
        invmat <- NULL
        set <- function(y = matrix()) {
                if(!class(y) == "matrix") stop("arg passed is not a matrix")
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmat <<- inv
        getinv <- function() invmat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second Function cacheSolve calculates the inverse of the matrix 
## created with makeCacheMatrix. However, it checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise it computes the inverse fo the matrix
## and sets the value of the inverse via the setinv function


cacheSolve <- function(x, ...) {
        invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data)
        x$setinv(invmat)
        invmat
}