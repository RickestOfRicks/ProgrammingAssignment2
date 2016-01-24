## This function was created for the Coursera course:
## R Programming's second assignment

## This function creates a special "Matrix" object that can have it's 
## inverse cached.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(temp){
		x <<- temp
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(tempInverse) inverse <<- tempInverse
	getInverse <- function() inverse

	##Sets the return for the list() function
	list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}


## This function calculates the inverse of the given matrix. If this inverse
## has already been calculated, the function returns the cached inverse if 
## and only if the parent matrix has remained unchanged.

cacheSolve <- function(x, ...) {

	inverse <- x$getInverse()

	##Checks if inverse has been cached1
	if(!is.null(inverse)){
		message("Getting cached inverse")
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(x)
	x#setInverse(inverse)

      ## Return a matrix that is the inverse of 'x'
	return(inverse)
}
