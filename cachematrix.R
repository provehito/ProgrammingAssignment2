##JJHRA

##Function for inverse of a Matrix
# x is a invertible matrix 
##First function captures the data in cache


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
        
}


##Cache function to solve the inverse of cachematrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data for matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m      
}
