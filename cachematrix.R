## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL								## This sets the matrix
	set <-function(y) {
		x <<- y
		inv <<- NULL
	}  														
	get <-function() x							## This gets the matrix
	setinv <- function(inverse) inv <<- inverse				## This sets the inverse of the matrix in parent environment
	getinv <- function() inv						## This gets the inverse of the matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)            ## This is needed to set an environment and refer to the functions provided by the reader
	
}


## Write a short comment describing this function
## This is a function that calculates the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
	 ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()							## Get the inverse if already calculated
	if(!is.null(inv)){
		message("getting cached data")
		return (inv)
	}
	
	data <- x$get()
	inv <-solve (data,...)
	x$setinv(inv)
	inv									##This sets the inverse from the cached matrix
       
}
