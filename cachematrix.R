## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverted <- function(inverted) i <<- inverted
	getInverted <- function() i		
	list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getInverted()
	if (!is.null(i)) {
		message("cached inverted")
		return(i)
	}
	data <- x$get()
	solved <- solve(data, ...)
	x$setInverted(solved)
	solved
}