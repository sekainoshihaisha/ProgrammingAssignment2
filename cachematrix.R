## Functions to make a cached matrix and then returning the inverse of it

## Inverting the matrix

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInverseMatrix <- function(MatrixToInvert) m <<- MatrixToInvert
    getInverseMatrix <- function() m
    
    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Getting the inverse of a matrix

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    
    if(!is.null(m))
    {
        message("Getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}
