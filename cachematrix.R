## Below are two functions that are used to cache the
## inverse of a matrix. 

## makeCacheMatrix: creates a special "matrix" object
## that can cache its inverse.

## There are 2 variables in makeCacheMatrix: x and m.
## x is used to process matrix, 
## m is used to store the result after processing.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	## Return x, which is data that need processing.
	get <- function() x

	## Assign the value of solve to m,
	## the value is the result after cache.
	setsolve <- function(solve) m <<- solve

	## Return m, which is the processing result.
	getsolve <- function() m

	## Function makeCacheMatrix returns a list,
	## which contains 4 elements, every one of them
	## is a defined function above.
	list(set = set, get = get, 
		setsolve = setsolve, 
		getsolve = getsolve)
}


## cacheSolve: computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has
## already been calculated(and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## The variable x here must be a makeCacheMatrix.
cacheSolve <- function(x, ...) {
	## x returns a makeCacheMatrix,
	## x$getsolve calls the "getsolve" function 
	## in makeCacheMatrix, and checks if the inverse
	## has already been calculated.
	m <- x$getsolve()
	if(!is.null(m)) {
		## m is not NULL, means that the inverse has already
		## been calculated, then the cache result will be 
		## returned directly.
		message("getting cached data")
		return(m)
	}

	## If the function runs to this step, then the inverse
	## hasn't been calculated yet.
	## Call "get" function in x to get data that need processing. 
	data <- x$get()
	## calculate "solve".
	m <- solve(data, ...)
	## Call "setsolve" function inside x, 
	## store the result back into x;
	## nexttime when "cacheSolve" is called, x already
	## has the inverse result, "cacheSolve" will return
	## the result directly from "if" structure.
	x$setsolve(m)
	# Return the inverse result.
	m
}

## The 2 functions are used this way:
## testmatrix <- matrix(1:4, nrow=2)
## testmatrix
## 	   [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cachedMatrix <- makeCacheMatrix(testmatrix)
## cacheSolve(cachedMatrix) ## 1st time
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(cachedMatrix) ## 2nd time
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## "cacheSolve" function has been called twice.
## 1st time it calculates and returns the inverse matrix
## of testmatrix;
## 2nd time it outputs a message "getting cached data",
## and prints the result that has already been calculated
## and stored in the 1st time of calling.


