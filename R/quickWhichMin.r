
#' Computes quickly the maxima of each column of a matrix
colMaxs = function(mat) {
  C_colMaxs(mat)
}

#' Computes quickly the minima of each column of a matrix
colMins = function(mat) {
  return(C_colMins(mat))
}

#' Computes quickly the minima of each row of a matrix
rowMaxs = function(mat) {
  C_rowMaxs(mat)
}

#' Computes quickly the minima of each row of a matrix
rowMins = function(mat) {
  return(C_rowMins(mat))
}  

#' Computes quickly the index of the largest element of each row of a matrix
which.rowMaxs = function(mat) {
  C_which_rowMaxs(mat)
}

#' Computes quickly the index of the largest element of each column of a matrix
which.colMaxs = function(mat) {
  C_which_colMaxs(mat)
}

#' Computes quickly the index of the smallest element of each row of a matrix
which.rowMins = function(mat) {
  return(C_which_rowMins(mat))
}  


#' Computes quickly the index of the smallest element of each column of a matrix
which.colMins = function(mat) {
  return(C_which_colMins(mat))
}  


examples.colMaxs = function() {  
  mat = cbind(c(1,2,3),c(100,-12,30))
  mat
  
  colMaxs(mat)
  colMins(mat)
  rowMaxs(mat)
  rowMins(mat)

  which.colMaxs(mat)
  which.colMins(mat)
  which.rowMaxs(mat)
  which.rowMins(mat)
  
  
  mat = matrix(runif(3*4),3,4)
  mat
  which.colMaxs(mat)
  
  
  library(rbenchmark)
  
  colMaxs.apply = function(mat) {
    apply(mat,2,max)
  }

    
  Z <- matrix(rnorm(500*500), 500, 500)
  benchmark(colMeans(Z), colMaxs(Z), rowMaxs(Z),colMaxs(t(Z)), colMaxs.apply(Z), C_colMaxs(Z), which.colMaxs(Z), colMins(Z),replications=100, order="relative")

}

