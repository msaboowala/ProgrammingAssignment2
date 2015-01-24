
################ Overview ##############
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


################ makeCacheMatrix ##############
## The first function, makeCacheMatrix does the following (Note, variable m is the inverse matrix variable)
## set the value of the vector
## get the value of the vector
## set the value of the inverse vector
## get the value of the inverse vector

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
	
    list(set=set, get=get, 
	setinverse=setinverse, 
	getinverse=getinverse)
}

################ cacheSolve ##############
## The second function, cacheSolve does the following:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above (assuming makeCasheMatrix function is already defined)
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Finally, it returns the inverse matrix (Note, variable m is the inverse matrix variable)

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}



################ test cases ##############

test1data <- matrix(runif(9,1,100),3,3)  ##creates 3X3 matrix of uniformily distributed variables
test2data <- matrix(rnorm(100, mean=0, sd=1), 10,10)  ##creates 10X10 matrix of Normally distributed data
test3data<- matrix(rt(25, df=10),5,5) ## creates a 5X5 matrix of the t distributed data

test1 <- makeCacheMatrix(test1data) ## test case 1 to make cached matrix
cacheSolve(test1)  ## test case 1 to solve inserve from cache
solve(test1data)  ## Used to check 

test2 <- makeCacheMatrix(test2data) ## test case 2 to make cached matrix
cacheSolve(test2)  ## test case 2 to solve inserve from cache
solve(test2data)  ## Used to check 

test3 <- makeCacheMatrix(test3data) ## test case 3 to make cached matrix
cacheSolve(test3)  ## test case 2 to solve inserve from cache
solve(test3data)  ## Used to check 


