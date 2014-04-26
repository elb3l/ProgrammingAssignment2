## here we have two functions makeCacheMatrix & cacheSolve

## the makeCacheMatrix function: take a matrix as an argument 
## returns a list of functions some will set and get the matrix and it's Inverse Matrix

## the makeCacheMatrix function: compute the inverse matrix of the special makeCacheMatrix output
## if the cached inverse Matrix already exist then it will get the inverse matrix from the cache.

## makeCacheMatrix takes a Matrix as an argument and return a list object containing functions that perform set & get over (matrix, inverse).

makeCacheMatrix <- function(A = matrix()) 
{
		## create an empty object that will hold the inverse matrix
        invmatx <- NULL
		
		## a set function to reset the A argument as well as invmatx
        setmatx <- function(B = matrix())
        {
				## set the A matrix with <<- operator outside setmatx environment 
                A <<- B
				
                ## set the the invmatx to NULL recomupte the Inverse for the new matrix with <<- operator outside setmatx environment 
				invmatx <<- NULL
        }
        ## getmatx will be used to provide the cacheSolve with the original matrix to compute the inverse matrix
        getmatx <- function() A
		
		## setinvmatx will be used to store the inverse matrix comupted by cacheSolve 
		## along side the original matrix in the special Matrix create by makeCacheMatrix
        setinvmatx <- function(solve) invmatx <<- solve
        
		## getinvmatx will be used by cacheSolve funtion to get and the inverse matrix If the inverse has already been calculated
        getinvmatx <- function() invmatx
        
		## makeCacheMatrix will return a list object containing functions that perform set & get over (matrix, inverse).
        list(setmatx = setmatx, getmatx = getmatx, setinvmatx = setinvmatx, getinvmatx = getinvmatx)
}


## cacheSolve take an agument returned by makeCacheMatrix function 
## and calculate the inverse of the matrix if the inverse matrix already exist it return the cached one

cacheSolve <- function(A, ...) 
{
        ## get & assign the inverse matrix from the object create by makeCacheMatrix using getinvmatx() inside makeCacheMatrix object into invmatx
        invmatx <- A$getinvmatx() ##class(invmatx) ---- matrix
        
		## check if the value returned by getinvmatx() and assign to invmatx is empty or not
		## if invmatx was not empty then it will return it from the cache
        if(!is.null(invmatx)) {
                message("getting cached inverse matrix") ## just to prove that we are using the cache value
                return(invmatx)
        }
		
		## get & assign the original makeCacheMatrix matrix using  getmatx()
        C <- A$getmatx() ##class(C) ---- matrix
		
		## using the builtin  solve function to compute the inverse matrix and assign it to invmatx
        invmatx <- solve(C) ##?solve	##class(invmatx) ---- matrix
        
		## set the makeCacheMatrix invmatx object using setinvmatx() i.e store the inverse matrix in the cache
		A$setinvmatx(invmatx)
        ## return the calcuted inverse matrix
		invmatx
        
}
