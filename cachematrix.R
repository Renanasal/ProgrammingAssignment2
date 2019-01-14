
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x<<- y
      inv <<- NULL
    }
    get<- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if (!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data,...)
  x$setinverse(inv)
  inv
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

m2 <-makeCacheMatrix(m1)
cacheSolve(m2)
m1
