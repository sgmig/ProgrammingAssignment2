## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes a matrix and returns a list, which is capable
# of storing the value of it's inverse, so that it doesn't have to compute it 
# every time. 
# When we run the function it sets the inverse as NULL, but it generates the
# necessary functions to modify this value. 

makeCacheMatrix <- function(x = matrix()) {
    
    invMat <- NULL
    
    set <- function(y) {
            x <<- y
            invMat <<- NULL
        }
        get <- function() x
        setInv <- function(a) invMat <<- a
        getInv <- function() invMat
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
    }

## Write a short comment describing this function
# This function takes the output of makeCacheMatrix and returns the inverse matrix.
# It first checks whether the value of the inverse is stored (cached) in the list. If it is, 
# it returns it. If it is not, it computes it and runs setInv to store this value in the 
# orginal object. In this way, the inverse is computed only once. 

cacheSolve <- function(x, ...) {
    
    invMat <- x$getInv()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInv(invMat)
    invMat
    }
        ## Return a matrix that is the inverse of 'x'
