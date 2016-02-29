## File contain two methods for finding the inverse of a given matrix and cache 
## the result so that there is no need to find the inverse again for the same matrix

##
# Function to generate a list of function to handle the environment level variables for caching
##
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	## Set the matrix in local scope
	mat <- x

	##
	# Update the original matrix and reset the cached inverse
	##
	set <- function(new_matrix = matrix()) {
		# Update the original matrix
		mat <<- new_matrix
		# Reset the cache
		inv <<- null
	}

	##
	# Get the original matrix
	##
	get <- function() {
		mat
	}

	##
	# Set the inverse matrix in local scope
	setInverse <- function(mInv)  {
		inv <<- mInv
	}

	##
	# Get the inverse from the local scope
	##
	getInverse <- function() {
		inv
	}

	# Return the list list defined functions
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##
# Function to generate the matrix inverse and store it in the cache, so that 
# on a second request to get the inverse (for the same matrix) it is served from cache
##
cacheSolve <- function(x, ...) {
	# Try to get the inverse
	inverse <- x$getInverse()

	# Return the inverse if already cached
	if(!is.null(inverse)) {
		message("getting inverse from the cache")
		# Return the inverse
		return(inverse)
	}

	## Create the inverse and cache it if 'inverse' is null

	# Get the original Matrix
	mat <- x$get()
	# Find the inverse of original matrix
	inverse <- solve(mat)
	# Store the inverse in cache
	x$setInverse(inverse)

	# return the inverse
	inverse
}
