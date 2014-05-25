
## These functions enable solving the inverse of a matix with caching,
## so that after one time the invese is computed it will be cached
## and the next times will be retrieved.

## makeCacheMatrix accepts a matrix 
## and returns a list containg getters and setters for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve accepts a list created by makeCacheMatrix,
## and returns its inverse. 
## The first time it is called for a certain makeCacheMatrix output list,
## it solves the inverse and caches it; 
## the next times it retrieves the cahced value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

