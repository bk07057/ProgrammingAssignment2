## This function will create a cached version of the matrix to compute its inverse
makeCacheMatrix <- function(x = matrix()) {
   ## initialize an empty bucket to store cached value of matrix inverse
   m <- NULL
   ## the setter function 
   set <- function(y) {
     x <<- y
     m <<- NULL	 
   }
   ## the getter function
   get <- function() x
   ## use the solve() function in R to compute inverse of a square matrix 
   setinv <- function(solve) m <<- solve
   ## return cached value (if inverse of a matrix hasn't been computed then the value of m is NULL
   getinv <- function() m
   ## list of available functions 
   list(set = set, get = get, 
        setinv = setinv,
		getinv = getinv)
}

## This function uses cached version of matrix created by makeCacheMatrix 
## and computes its inverse. The standard R function solve() is used to compute matrix inverse
## If the inverse of matrix is already computed once and also if the matrix has not
## been modified then cached version of result is returned otherwise result from matrix inverse computation is returned. 
## 
cacheSolve <- function(x, ...) {
        ## try to get inverse of matrix from the cache first
		m <- x$getinv()
		## if the cache value is not null then simply return this value 
		if (!is.null(m)) {
		    message ("getting cached data")
			return (m)
		}
		## else get the matrix to inverse in data  
		data <- x$get()
		## compute inverse of the 'data' and store result in m  
		m <- solve(data,...)
		## now that we have computed inverse once, save this result (i.e. m) in cache 
		x$setinv(m)
		## return the result 
		m
}