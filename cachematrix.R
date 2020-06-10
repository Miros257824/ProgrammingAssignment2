## Two functions called "makeCacheMatrix" and "cacheSolve" are used to create an object
## (matrix) that store in cache the inverse for the input (invertible square matrix).

makeCacheMatrix <- function(x = matrix()) {

    ##Initialize the inverse
    i<- NULL

    ## set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## get the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setInv <- function(inverse) 
    i <<- inverse
    
    ## get the inverse of the matrix
    getInv <- function() i

    ## Return a list of the methods
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    }

    ## The function cacheSolves compute the inverse of the matrix returned by 
    ## the function "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
    message("getting cached result")
    return(m)
    }
   
    ## get the matrix from our object
    data <- x$get()
    
    ## inverse using matrix multiplication
    m <- solve(data, ...)
    x$setInv(m)

    ## Return the inverse matrix
     m
    }
