## These functions are used to create a vector object that stores
## a matrix and its inverse. By caching these values, computation
## time is saved on the inverse calculation.

## This function takes a matrix and returns a vector of functions
## to cache the matrix. When cacheSolve is called on the resulting
## vector, the matrix inverse is also cached.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes the vector returned by makeCacheMatrix,
## calculates the inverse of the stored matrix, and caches it
## in the same vector.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
    	message("Getting cached inverse")
    	return(m)
    }    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
