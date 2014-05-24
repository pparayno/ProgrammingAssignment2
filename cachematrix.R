## This function creates a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                ## This subfunction, using the operator <<-, alters the 
                ## variables x and m in the parent environment, 
                ## the makeCacheMatrix() function.
        }
        get <- function() x  
        ## This gets the matrix that is stored in x. 
        setmatrix <- function(solve) m <<- solve
        ## This saves the inverse of the matrix and stores it in m.
        getmatrix <- function() m
        ## This returns the cache.
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
## This second function computes the inverse of the special matrix that is 
## returned by the function makeCacheMatrix. If the inverse has already been 
## calculated and the matrix has not changed, then the cacheSolve retrieves 
## the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                ## This checks if the returned cache has any contents. If it
                ## does, print the message "getting cached data" and then return
                ## the cached matrix.
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        ## In the first time, when the cache is empty, this function gets 
        ## the matrix and place it in a local variable called matrix.
        m <- solve(matrix, ...)
        ## This calculates the inverse of the matrix and stores the
        ## inverse in the local variable m. The dot dot dot (...) allows
        ## including other arguments in the calculation of the inverse
        ## of the matrix.
        x$setmatrix(m)
        m
        ## This stores the inverse of the matrix and returns it to whichever
        ## function that calls it in the first place.
}