## The two functions create a cached special "matrix" object and calculate 
## the inverse of a matrix, respectively.

## makeCacheMatrix creates a special matrix object that can cache its inverse;
## this object has four functions - get (returns the value of a matrix), set 
## (changes the value of a matrix), setinverse (set the value of the matrix 
## inverse) and getinverse (returns the value of a matrix inverse)

## To create an empty matrix object use x <- makeCacheMatrix()
## To set its value use x$set(<matrix>), e.g. x$set(matrix(c(1,3,2,2,1,1,3,1,2),nrow=3,ncol=3))
## To get its value use x$get()
## To get matrix inverse use x$getinverse()
## To set matrix inverse use x$setinverse(<matrix>)

## makeCacheMatrix returns a list with four methods/functions for manipulating
## the object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve functions calculates the value of a matrix inverse (if not 
## calculated or matrix has not changed) and caches it within the special 
## matrix object given with argument x

## cacheSolve function uses solve for calculating the inverse

## To call this function use cacheSolve(<matrix_object>)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

