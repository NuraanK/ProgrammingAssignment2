## makeCacheMatrix is the function that GETS the value 
## of a matrix (that is input by the user), then SETS the value of the INVERSE of the matrix 
## and GETS that value of the inverse matrix.   
## cacheSolve takes as its input the output from makeCacheMatrix. It checks if this matrix from makeCacheMatrix has a value
## already or not (i.e. is it empty or not?). If makeCacheMatrix matrix is empty, it gets the ORIGINAL input matrix data  	
## and sets the invertible  matrix by using the solve function.	
## If the makeCacheMatrix matrix DOES have a value in it, it returns the message  "getting cached data" and returns the cached object.

## Note that <<- is used to assign a value to an object in an environment that is DIFFERENT from the CURRENT environment

## The makeCacheMatrix function takes a matrix as an input
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             ## sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x                     ## get the value of the matrix
        setInverse <- function(inverse) inv <<- inverse ## set the value of the inverse of the matrix
        getInverse <- function() inv            ## get the value of the inverse of the matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function uses the output from makeCacheMatrix as its input

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")          ##If the makeCacheMatrix matrix IS NOT NULL, then shows this message and returns the value of the inverted matrix
                return(inv)
        }
        mat <- x$get()                                  ##If the makeCacheMatrix matrix IS NULL, get the original matrix data
        inv <- solve(mat, ...)                          ##Use solve function to get value of the inverse of the matrix
        x$setInverse(inv)                               ##Sets the value of the inverse of the matrix
        inv                                             ##returns the inverse of the matrix
}
