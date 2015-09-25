#Rprog-032 Assignment 2
#Caching the Inverted Matrix of a square nonsingular (i.e. invertible) matrix
#
#############
#This second programming assignment required us to write an R function that
#would be able to cache potentially time-consuming computations. Specifically,
#taking the Inverted matrix of a invertible matrix, which could be a fast
#operation with a 100x100 matrix, but surely not with a 10000x10000. This may
#take too long to compute, especially if it has to be computed repeatedly (e.g.
#in a loop).
#
#If the contents of a matrix are not changing, it may make sense to cache the
#value of the inverted so that when we need it again, it can be looked up in the
#cache rather than recomputed.
#
#In this Programming Assignment we were suposed to take advantage of the scoping
#rules of the R language and how they can be manipulated to preserve state
#inside of an R object.
#
#######################
#
#As in the example given for this assignment the <<- operator is used here.
#It can be used to assign a value to an object in an environment that is 
#different from the current environment.
#
#Below are the two functions that are used to create a special object that
#(1) stores a matrix and (2) cache's its inverted. The second function only
#works with invertible matrices.
#
################################
#
#The first function, makeCacheMatrix creates a special "matrix", which is really
#a list containing a function to
#
#set the value of the matrix
#get the value of the matrix
#set the value of the inverted matrix
#get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#The following function calculates the inverted of the special "matrix"
#created with the above function.
#
#However, it first checks to see if the inverted matrix has already been
#calculated. If so, it gets the inverted matrix from the cache and skips the
#computation.
#
#Otherwise, it calculates the inverted matrix of the data and sets the value of
#the inverted matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
