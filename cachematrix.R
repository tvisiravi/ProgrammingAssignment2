## Caching the inverse of a Matrix

## Creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(	set = set, 
		get = get, 	
		setInverse = setInverse, 
		getInverse = getInverse)
}

## Computs the inverse of the special "matrix" created by function above

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (lis.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
