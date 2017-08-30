## 2 functions that cache the inverse of a matrix


## Create a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
        
        ## set the inverse property
        v <- NULL
        
        ## set the matrix
        set <- function( matrix ) {
                m <<- matrix
                v <<- NULL
        }
        
        ## get the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## set the inverse of the matrix
        setIV <- function(inverse) {
                v <<- inverse
        }
        
        ## get the inverse of the matrix
        getIV <- function() {
                ## Return the inverse property
                v
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setIV = setIV,
             getIV = getIV)
}


## Calculates inverse of the matrix returned by "makeCacheMatrix"
## If inverse has already been calculated and no changes since,
## then the "cachesolve" retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getIV()
        
        ## Just return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setIV(m)
        
        ## Return the matrix
        m
}
