## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will 
## (i) create a matrix (using set or via input parameters)
## (ii) show the matrix (get)
## also it will cache the inverse calculated by cacheSolve

## example:

# > mymatrix <- matrix(1:4, 2, 2)
# > specialmatrix <- makeCacheMatrix(mymatrix)
# > specialmatrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(specialmatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(specialmatrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



makeCacheMatrix <- function(x=matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) m<<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will 
## (i) invert the matrix using solve (if it hasn't been inverted yet)
## (ii) will read the cached data if the inverse has been already calculated
## 
## example:

# > mymatrix <- matrix(1:4, 2, 2)
# > specialmatrix <- makeCacheMatrix(mymatrix)
# > specialmatrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(specialmatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(specialmatrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5




cacheSolve <- function(x,...){
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}