##  These functions introduce a special matrix object 
##  which caches the matrix and its inverse.

## makeCacheMatrix creates a list of functions to set the value of the matrix,
## get the value of the matrix, set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL  ## inverse will store the cached inverse matrix
	    set <- function(y) {
				x <<- y ## set the value
			inverse <<- NULL ## clear the cache
			}
	
	    get <- function() x ## function to get the value of the matrix
   setinverse <- function(inv) inverse <<- inverse ## Set the inverse
   getinverse <- function() inverse ## function to get the inverse


	list(set = set, get = get, ## return the matrix with the above functions
	setinverse = setinverse,
	getinverse = getinverse)
	

}


## cacheSolve: Compute the inverse of the matrix. However, 
## if the inverse is already calculated, it returns the cashed inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inverse <- x$getinverse()

	if(!is.null(inverse)) { ## if the inverse is not yet calculatedm calculate it
		message("getting cached data")
		return(inverse)
	}
	## the cache was empty, so we need to calculate it
	data <- x$get() ## get value of matrix
	inverse <- solve(data, ...) ## calculate inverse
	x$setinverse(inverse) ## cache the inverse
	inverse ## return the inverse
}
