## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Author: NGUYEN THINH PHAT
# Date: 07-26-2015

makeCacheMatrix <- function(x = matrix()) {

	# This variable hold the cached value of a Matrix or NULL if nothing is cached
	cache <- NULL

	# Store a matrix
	setMatrix <- function(pMatrix) {
		x <<- pMatrix
		# flush the cache because the matrix is changed
		cache <<- NULL
	}

	# Return stored matrix
	getMatrix <- function() {
		x
	}

	# Cache the inverse of a matrix
	cacheInverseMatrix <- function(inverseMat) {
		cache <<- inverseMat
	}

	# Get the cached value
	getInverseMatrix <- function {
		cache
	}

	# Return a list of functions
	list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverseMatrix = cacheInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # Get the cached value
        inverseMat <- x$getInverseMatrix()

        # Check whether the cached value exists or not
        if(!is.null(inverseMat)) {
        	message("getting an inverse matrix from cache")
        	return(inverseMat)
        }

        # Otherwise get the new matrix, calculate the inverse and cache it
        m <- x$getMatrix()
        inverseMat <- solve(m, ...)
        x$cacheInverseMatrix(inverseMat)

        # Return the inverse matrix
        inverse
}
