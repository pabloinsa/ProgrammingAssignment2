## Put comments here that give an overall description of what your
## functions do

## Make Cache Matrix assignment 1

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cache Solve assignment 2

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()

    if( nrow(data)==ncol(data)){                    #Square-dimension matrix
        
        if(det(data)< -1e-15 || det(data)>1e-15){      #det different from 0
            i <- solve(data)
            x$setinverse(i)
            return(i) 
        }else{
            message("The matrix determinant is 0 so there can't be an inverse.")
        }
    }else{
        message("The matrix should be square-dimension")
    }

    
        ## Return a matrix that is the inverse of 'x'
}
