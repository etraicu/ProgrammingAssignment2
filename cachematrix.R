
## Write a short comment describing this function
## This function creates and returns a special vector consisting of 4 methods : 
## "set, get, setSolve and getSolve"

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, 
			 get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Write a short comment describing this function
## This function gets the inverse of a matrix from cache if it's already been calculated and cached 
## else calculates the inverse. It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
