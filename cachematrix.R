## cachematrix.R
##
## Description: This R file contains two functions: 
## 		makeCacheMatrixfunction - defines function to create the matrix
##		cacheSolve - defines function to create or retrieve the inverse
##
## Date: submitted 27 July 2014 EST
## Programming Assignment 2
##
## stub by: Prof R Peng, JHU
## 			Associate Professor
##			Department of Biostatistics
##			Johns Hopkins Bloomberg School of Public Health
##
## Source: 	https://github.com/rdpeng/ProgrammingAssignment2
##
## forked version by student: JHUDSSJC
## Coursera Course: R Programming (05) start: 4 July 2014
##
## Repository: JHUDSSJC Git Account
## Fork: 	https://github.com/JHUDSSJC/ProgrammingAssignment2
##

makeCacheMatrix <- function(x = matrix()) {

## This function creates a special "matrix", which is really a list containing 
## a function that:
##		sets the value of the matrix
##		gets the value of the matrix
##		sets the value of the inverse
##		gets the value of the inverse

        matinv <- NULL			# initialise matrix inverse to NULL

        set <- function(y) {	# define set method to set value of matrix 
                x <<- y			# cache matrix 'x' to Global environment
                matinv <<- NULL	# cache matrix inverse to Global environment
        }

        get <- function() x		# define get method to return matrix 'x'

        setinv <- function(solve) matinv <<- solve
        getinv <- function() matinv

        list(set = set, get = get,	# define list of method definitions for
             setinv = setinv,		#	function
             getinv = getinv)

## Returns a list of methods for makeCacheMatrix

}


cacheSolve <- function(x, ...) {

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

        matinv <- x$getinv()	# retrieve cached inverse
        						# store in local matinv variable 

        if(!is.null(matinv)) {	# if the cached matrix inverse is not NULL
                message("getting cached matrix")	# print message to user
                return(matinv)						# return the cached value
        }
        
        ## else:

        data <- x$get()				# get the cached matrix, assign to 'data'
        matinv <- solve(data, ...)	# calculate the matrix inverse of x
        x$setinv(matinv)			# store the inverse in the cache var
        matinv						# return the value of matinv

## Returns a matrix that is the inverse of 'x'

}
