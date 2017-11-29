## special Cache Matrix And Inverse Matrix functions

## Function to create special matrix 
## with get, set, and getinverse function

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}
  

## Cachesolve function checks for inverse
## If not found returns null then prints the inverse of special matrix

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
      inver <- x$getinverse()
      if(!is.null(inver)) {
            message("getting cached data")
            return(inver)
        }
      mat.data = x$get()
      inver = solve(mat.data)
      x$setinverse(inver)
      inver
}
