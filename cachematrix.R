
#################################################################################
## Regarding example, the first part of my function establish and obtained a matrix,
## the second part establish and obtained the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {

          m <-NULL
      set_M <-function(y){
               x<<-y
               m<<-NULL
       }
        get_M <-function()x

        set_Inv_M <-function(solve)m<<-solve
        get_Inv_M <-function()m

        list(set_M = set_M,
             get_M = get_M,
             set_Inv_M = set_Inv_M,
             get_Inv_M = get_Inv_M)
}
#################################################################################

## This second function is similar that second example, it first checks to
## see if the inverse has already been calculated, then, If so, it gets the
## inverse of the matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {

        m <- x$get_Inv_M()
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }

   data <- x$get_M()
        m <- solve(data, ...)
        x$set_Inv_M(m)
        m
}
#################################################################################
