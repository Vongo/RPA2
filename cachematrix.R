# This cutie tool allows to avoid repeating the inversion of a given square nonsingular matrix
# by caching the result of the operation
# See cacheMatrixTest for an example of how to use it.

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

# Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    # The following is the same as the sample but in a more compact way ;)
    if(is.null(s))
	    x$setSolve(s <- solve(data <- x$get(), ...))
    else
    	print("Getting data from cache...")
    s
}

# Inverts /someMatrix/ twice, and checks whether the result is the same.
# You will have to check on the display to know whether it used the cached value.
# /print/s details (by default).
cacheMatrixTest <- function(someMatrix = rbind(c(1, -1/4), c(-1/4, 1)), print = T) {
	if(print){
		print("This is what your matrix looks like :")
		print(someMatrix)
	}
	someCacheMatrix <- makeCacheMatrix(someMatrix)
	if(print){
		print("This is what the cacheMatrix looks like :")
		print(summary(someCacheMatrix))
		print("Inverting the matrix once...")
	}
	r1 <- cacheSolve(someCacheMatrix)
	if(print){
		print(someCacheMatrix$get())
		print("Done.")
		print("Inverting the matrix once again...")
	}
	r2 <- cacheSolve(someCacheMatrix)
	if(print) print(identical(r1, r2)?"Test passed":"Test failed")
}

# Chains the previous test for /count/ iterations
# with a randomly generated square matrix of /dim/*/dim/
randomTestGenerator <- function(count = 5, dim = 1000) {
	set.seed(42)
	time <- matrix(NA ,ncol = 3, nrow = count)
	colnames(time) <- c("User","System","Elapsed")
	for(i in 1:count){
		t0 <- proc.time()
		cacheMatrixTest(someMatrix = matrix(runif(dim^2),dim), print = F)
		mt1 <- proc.time()
		time[i,]<-(mt1-t0)[1:3]
	}
	print(time)
	print("Total elapsed time :")
	print(paste(sum(time[,"Elapsed"]), "seconds"), sep = " ")
}

# Credits go to kohske (http://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r)
# for funny this implementation of ternary operator \o/
`?` <- function(x, y) {
	xs <- as.list(substitute(x))
	if (xs[[1]] == as.name("<-")) x <- eval(xs[[3]])
	r <- eval(sapply(strsplit(deparse(substitute(y)), ":"), function(e) parse(text = e))[[2 - as.logical(x)]])
	if (xs[[1]] == as.name("<-")) {
		xs[[3]] <- r
		eval.parent(as.call(xs))
	} else r
}       