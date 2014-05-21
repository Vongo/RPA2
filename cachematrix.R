## Put comments here that give an overall description of what your
## functions do
#


# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	s = NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) s <<- solve
	getSolve <- function() s
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(is.null(s))
	    x$setSolve(s <- solve(data <- x$get(), ...))
    else
    	message("Getting data from cache...")
    s
}

