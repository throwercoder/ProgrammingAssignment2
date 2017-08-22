## These two functions allow a matrix to be stored and retrieved.
## A second function calculates and stores the inverse of the matrix. 
## This allows the inverse of the matrix to be rapidly accessed, which
## may save significant amounts of time when the objects involved are large.

## This first function makeCacheMatrix creates a list of functions used to 
## store and access the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This second function is used to calculate and store the inverse of the
## "matrix" object created with the previous function. If the function detects
## that the inverse has previously been calculated, the inverse will be 
## retreived from the cache rather than re-calculated, saving time.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i     
}
