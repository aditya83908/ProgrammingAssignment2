## Put comments here that give an overall description of what your
## functions do

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
      x<<-y
      inv<<- NULL
  }
  get <- function() x
  getinv<-function() inv
  setinv<-function(inverse) inv<<-inverse
  list(set=set, get=get, getinv=getinv, setinv=setinv)

}


## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting cache data")
    return(inv)
    mat.dat<- x$get()
    inv<- solve(mat.data, ...)
    x$setinv(inv)
    return(inv)
  }
}
