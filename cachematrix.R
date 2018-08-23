# Assignment: Caching the Inverse of a Matrixless
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly

# Hemant Agarkar - 23-Aug-18

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}

# Tests conducted
# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# my_matrix$getinverse()
# Result NULL - confirms no data at begining
# 
# cacheSolve(my_matrix)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# my_matrix$getinverse()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# cacheSolve(my_matrix)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

