print("------------------ Johanns R functions loaded. -------------------------")

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

breakTextSPSSProduction<- function(x, breakP=80, chunks="\\.\\n"){
  lb <- "_x_x_x_x_"
  parags <- gsub("^ +| +$","",strsplit(x,split=chunks)[[1]])
  retv <- c()
  for (p in parags){
    print(p)
    xx<-p
    xx<-gsub("^\\n+","",xx)
    xx<-gsub("\\n ",lb,xx)
    xx<-gsub("\\n",lb,xx)
    print(xx)
    parts <- strsplit(gsub(" +"," ",xx), split=" ")[[1]]
    ret <- ""
    memory <- ""
    for (ii in parts){
      #i <- gsub("^ +| +$","",ii)
      i <- ii
      if (nchar(memory) + nchar(i) + 1 < breakP-1) { ret <- paste0(ret,ifelse(ret=="" | substr(i,1,1)==" ",""," "),i); memory <- paste0(memory,ifelse(ret=="",""," "),i) }
      else {
        if (lengths(regmatches(memory, gregexpr("\\'|\"",memory)))%%2>0){ret <- paste0(ret, " +")}
        ret <- paste0(ret, "\n ", i)
        memory <- i
      }
      if (grepl(lb,i)) {memory <- tail(strsplit(i,split=lb)[[1]],1)}
    }
    retv <- c(retv, paste0("\n",gsub(lb,"\n ",ret)))
  } 
  returnt <- gsub("\\n\\n\\n","\n\n",paste0(retv, collapse="."))
  returnt
}

