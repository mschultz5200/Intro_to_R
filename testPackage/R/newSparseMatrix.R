newSparseMatrix <-
function(vec) {
  setClass("SparseMatrix",
           contains = "matrix")
  sparse <- diag(vec, nrow = length(vec), ncol = length(vec))
  return(new("SparseMatrix", sparse))
}
