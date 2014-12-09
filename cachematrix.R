## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix - creates a list of helper functions
## cacheSolve - retrieves the matrix inverse from cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	getinverse <- function() inv
	setinverse <- function(inverse) inv <<- inverse
	list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data)
	x$setinverse(inv)
	inv
}
