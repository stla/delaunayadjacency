
#' @import clipr
#' @import magrittr

matrix2list <- function(M){
  outfile <- tempfile(fileext=".txt")
  write_clip(M, sep=", ")
  clipboard <- c(sprintf("%s],", head(read_clip(),-1)),
                 sprintf("%s]", tail(read_clip(),1)))
  out <- paste0(c("[", paste0(" [", clipboard))) %>%
    cat(sep="\n", file=outfile) %>%
    cat("]", sep="", file=outfile, append=TRUE)
  return(outfile)
}

#' Generates adjacency matrix
#'
#' @param vertices matrix of vertices (n x d)
#' @param outfile output file
#'
#' @return
#' @export
#'
#' @examples
#' v <- rbind(
#'  c( -5, -5,  16 )
#', c( -5,  8,   3 )
#', c(  4, -1,   3 )
#', c(  4, -5,   7 )
#', c(  4, -1, -10 )
#', c(  4, -5, -10 )
#', c( -5,  8, -10 )
#', c( -5, -5, -10 ))
#'adjMatrix(v)
adjMatrix <- function(vertices, outfile=tempfile(fileext=.txt)){
  infile <- matrix2list(vertices)
  command <- sprintf("%s %s %s",
                     system.file("bin", "windows", "adjacencymatrix.exe", package="delaunayadjacency"),
                     infile, outfile)
  system(command)
  outfile <- strsplit(readr::read_file(outfile), "\\]\r\n\\[")[[1]]
  outfile <- gsub("\\[", "", gsub("\\]", "", outfile))
  outfile <- gsub(",", " ", outfile)
  outfile <- sapply(outfile, function(x) strsplit(x, " ")[[1]], USE.NAMES = FALSE )
  return(outfile)
}
