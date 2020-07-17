
normtrunc <- function(x,m,sd,lower,upper, tol=1000000, progb = FALSE){
  v <- rnorm(x,m,sd)
  c <- 1
  if(progb) { pb <- txtProgressBar(min = 0, max = tol, style = 3, char="+") }
  while ((min(v) < lower | max(v) > upper) & c < tol ) {
    v <- rnorm(x,m,sd)
    c <- c+1
    if(progb) { setTxtProgressBar(pb, c) }
  }
  rl <- list(vector=v, errors=NA, c)
  if (c>=tol) {rl$vector <- NA; rl$errors="range too small"}
  rl
}



breakText <- function(x, breakP=80){
  x <- gsub("\\n","___lb___",x)
  x <- gsub("\\t","___t___",x)
  parts <- strsplit(gsub("  "," ",x), split=" ")[[1]]
  ret <- ""
  memory <- ""
  for (i in parts){
    if (nchar(memory) + nchar(i) < breakP) { ret <- paste0(ret,ifelse(ret=="",""," "),i); memory <- paste0(memory," ",i) }
    else {ret <- paste0(ret, "\n",i); memory <- i}
  }
  gsub("  "," ",gsub("___t___","\t",gsub("___lb___","\n",ret)))
}

