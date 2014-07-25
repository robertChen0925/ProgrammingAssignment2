## makeCacheMatrix class
## This class has inverse matrix attributes, setting and getting method.
## The class is used to handle inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    ## ========== Attributes ==========
    inverseMatrix <- NULL
    
    ## ============ Methods ============
    ## Sets matrix 
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## Gets matrix
    ## @return matrix
    get <- function() {x}
    
    ## Sets inverse matrix
    setInverseMatrix <- function(iMatrix) {
        inverseMatrix <<- iMatrix  
    } 
    
    ## Gets inverse matrix
    ## @return inverse matrix
    getInverseMatrix <- function() {inverseMatrix} 
    
    ## Makes makeCacheMatrix object can use method below
    ## e.g. 
    ##      x <- matrix(rnorm(25, 1), ncol = 5)
    ##      object <- makeCacheMatrix(x) ## create makeCacheMatrix object
    ##      object$get() ## calls method of makeCacheMatrix object
    list(set = set, 
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Caches inverse matrix if it not exist.
## Otherwise, return it from cache. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverseMatrix()
    
    ## Return inverse matrix if inverse marix in cache
    if(!is.null(inverseMatrix)) {
        message("Getting cached inverse matrix.")
        return(inverseMatrix)
    }
    
    ## If inverse matrix not exist
    ## creates inverse matrix then cache it
    matrix <- x$get()
    inverseMatrix <- solve(matrix)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
