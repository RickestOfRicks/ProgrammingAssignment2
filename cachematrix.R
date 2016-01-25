## This function was created for the Coursera course:
## R Programming's second assignment

## This function creates a special "Matrix" object that can have it's 
## inverse cached.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
      set <- function(temp) {
            x <<- temp
		inverse <<- NULL
		print("SetInverse")
      }
	setInverse <- function(tempInverse){
		inverse <<- tempInverse
	}
	get <- function() x
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the given matrix. If this inverse
## has already been calculated, the function returns the cached inverse if 
## and only if the parent matrix has remained unchanged.

cacheSolve <- function(x, ...) {

	inverse <- x$getInverse()
	##Checks if inverse has been cached
	if(!is.null(inverse)){
		message("Getting cached inverse.")
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(matrix)
	x$setInverse(inverse)

      ## Return a matrix that is the inverse of 'x'
	inverse
}
