#' Some functions that are useful for coding


#' Looks through all loaded functions and searches for
#' global variables that are used within the functions
#' this is a common source for errors
check.global.vars = function() {
  require(codetools)
  print("Usage of global variables in loaded functions:")
  funs = ls.funs(env=globalenv())
  for (fn in funs) {
    cmd = paste("findGlobals(",fn,",merge=FALSE)$variables",sep="")
    glob = try(eval(parse(text=cmd)))
    if (length(glob)>0) {
      print("")
      print(paste(paste(fn,": "),paste(glob,collapse=", ")))
    }
  }
}
#check.global.vars()

#' List all functions
ls.funs <-function(env=sys.frame(-1)) {
  unlist(lapply(ls(env=env),function(x)if (is.function(get(x)))x))
}
#' List all variables
ls.vars <- function(env=sys.frame(-1)) {
  unlist(lapply(ls(env=env),function(x)if(!is.function(get(x)))x))
}

