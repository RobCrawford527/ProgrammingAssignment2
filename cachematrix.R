## These two functions together take a square matrix as input and cache the
## matrix itself and its inverse.

## The first function takes a square matrix and creates a list of four functions.
## These allow the matrix and its inverse to be set and retrieved.
## When the matrix is set the inverse is reset to NULL.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function takes an object created by the first function as its argument.
## It checks whether the inverse has been calculated by using getinverse()
## from the first function. If this is NULL it calculates the inverse
## and stores this in the object using setinverse() from the first function.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("already calculated, getting cached data")
		return(inv)
	} else {
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv
	}
}
