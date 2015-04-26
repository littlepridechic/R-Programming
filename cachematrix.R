# makeCacheMatrix creates a list of functions for the storage of the inverse of a matrix in cache. 
# X and inv are defined to be allowed to accesse outside of the function.

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# cacheSolve return the inverse of the matrix
# first checks if the inverse has already been calculated
# If not, it computes the inverse, sets the value in the cache via setinverse function.
# If yes, it gets the result and will skip the calculation again
# This function will returns the inverse of the matrix.

cacheSolve <- function(x, ...) 
{
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

# TestRun:
## > x <- matrix(rnorm(25), nrow = 5) 
## > m <- makeCacheMatrix(x)
## > m$get()
##             [,1]        [,2]       [,3]        [,4]        [,5]
## [1,] -1.22449196  0.44659497  1.3376695 -1.16391840  0.73293197
## [2,]  1.57925225  1.39488021 -1.8180090  1.99177449  0.03530816
## [3,] -1.01899998  0.84913761  0.3702461  0.01911171 -0.27767701
## [4,] -0.06042997 -1.12817965  0.1969665 -0.18395841  1.28825901
## [5,] -0.40996045  0.01458704  0.3870460 -0.99776661 -1.25167426

## > cacheSolve(m)
##             [,1]       [,2]        [,3]       [,4]       [,5]
## [1,]  0.40194095 -0.4341666 -1.61134476 -1.6043336 -1.0706444
## [2,]  0.60754738  0.4592617 -0.10348774 -0.1529146  0.2342855
## [3,]  0.07923564 -1.8168928 -1.45329328 -3.3493429 -3.1296895
## [4,] -0.67967925 -1.1425056  0.02253378 -1.6928416 -2.1775425
## [5,]  0.44173755  0.4964736  0.05920327  0.8374333  0.3225158

## > cacheSolve(m)
## getting cached data
##             [,1]       [,2]        [,3]       [,4]       [,5]
## [1,]  0.40194095 -0.4341666 -1.61134476 -1.6043336 -1.0706444
## [2,]  0.60754738  0.4592617 -0.10348774 -0.1529146  0.2342855
## [3,]  0.07923564 -1.8168928 -1.45329328 -3.3493429 -3.1296895
## [4,] -0.67967925 -1.1425056  0.02253378 -1.6928416 -2.1775425
## [5,]  0.44173755  0.4964736  0.05920327  0.8374333  0.3225158