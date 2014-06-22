## This program is modified from given example which is 
## "Caching the Mean of a Vector"
## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. sets the matrix
## 2. gets the matrix
## 3. sets the matrix for solve
## 4. gets the inverse matrix

## This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        ##  This function creates a special "matrix" object 
        ##  that can cache its inverse
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("obtaining cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
