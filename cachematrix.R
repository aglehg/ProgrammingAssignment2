## Cacheable Matrix Inverse  
## pull the cpu heavy matrix inverse from cache 
## instead of calculating it each time 

## !! Cacheable matrix 
## Returns an object that can cache the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse 
  )
}

## 
## !! Check cache for inverse 
## if the inverse was already calculated 
## return the cached inverse 
## If inverse is NOT cached, calculate it, store it, then return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting chached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}