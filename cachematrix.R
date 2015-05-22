##
## makeCacheMatrix
## cacheSolve
##
## These functions calculate the inverse of a matrix and cache the result
## for use in subsequent calls to avoid recalculating the matrix inverse
##
## Usage examples:
##    # m1, m2 are square, invertable matrices
##
##    e <- makeCacheMatrix()
##    e$set(m1)
## or:
##    e <- makeCacheMatrix(m1)
##
##    cacheSolve(e)
##    cacheSolve(e)
##    e$set(m2)
##    cacheSolve(e)
##    e$getinv()
##

## makeCacheMatrix
##  - function initialises the cache environment
##  - also defines the closure functions for managing the cache
##
makeCacheMatrix <- function(cmat = matrix()) {
  ## Set the cache environment which will be used by the closure functions
  ##
  ## cache variables: cmat, cminv
  ## cache closure functions: set, get, setinv, getinv
  ##
  ## Changes to variables within this cache environment by calls to the
  ## closure functions will persist across calls to the closure functions

  ## cmat - cached matrix
  ## Initialised via the function call
  ## Either set to the function argument or initialised to an empty matrix

  ## cminv - cached matrix inverse
  ## Initialise to NULL
  cminv <- NULL

  ## set - function resets the cached matrix and resets the cached inverse
  ## Assignments are made within the cache environment
  ##
  set <- function(m) {
    ## Set the cached matrix within the cache environment
    cmat <<- m
    ## Reset the cached matrix inverse to NULL within the cache environment
    cminv <<- NULL
  }

  ## get - function returns the cached matrix from the cache environment
  ##
  get <- function() cmat

  ## setinv - function sets the cached inverse within the cache environment
  ##
  setinv <- function(marg) cminv <<- marg

  ## getinv - function returns the cached inverse from the cache environment
  ##
  getinv <- function() cminv

  ## makeCacheMatrix return value - list containing the closure functions
  ## This enables the functions to be called using the $ construct
  ## The closure functions will also enable access to the cache environment
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
 
}

## cacheSolve
##  - function retrieves a cached matrix inverse if it exists in the cache
##  - or first calculates the matrix inverse and caches it for future use
##
cacheSolve <- function(c, ...) {
  ## Return a matrix that is the inverse of the cached matrix
  ##
  ## The cache environment is accessed using the list of closure functions
  ## supplied by the function argument c
  ##
  ## If the matrix inverse has already been calculated for the cached matrix
  ## then the cache environment will contain the inverse and this is returned
  ##
  ## If the matrix inverse has not already been calculated for the cached
  ## matrix then the cache environment returns NULL for the matrix inverse;
  ## so the matrix inverse is calculated and cached before being returned

  ## c - list of cache environment closure functions from makeCacheMatrix

  ## minv - local copy of the matrix inverse
  ## Retrieve the cached matrix inverse from the cache environment
  minv <- c$getinv()

  ## If minv is not NULL then the matrix inverse has previously been
  ## calculated for the cached matrix and cached within the cache environment
  ## Print a suitable message and return this value
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }

  ## Else the matrix inverse has not previously been calculated...

  ## data - local copy of the cached matrix
  ## Retrive the cached matrix from the cache environment
  data <- c$get()

  ## Calculate the matrix inverse
  ## Calling 'solve' with a square matrix for the first argument and using
  ## defaults for other arguments will return the inverse of the matrix
  ## Assume for now that the matrix is square and invertable
  minv <- solve(data, ...)

  ## Set the cached matrix inverse within the cache environment to enable
  ## the matrix inverse to be reused later instead of being recalculated
  c$setinv(minv)

  ## Return the matrix inverse
  minv
  
}
