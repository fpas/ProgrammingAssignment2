## Coursera R Programming - Johns Hopkins
## Programming assignment 2: Lexical Scoping

## Cache the inverse of a matrix
## To be applied on square invertible matrices only

## makeCacheMatrix creates a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) inv <<- solve
	getsolve <- function() inv
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, it is retreved from the cache

cacheSolve <- function(x, ...) {
	inv <- x$getsolve()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setsolve(inv)
	inv
}
