rm(list=ls())
library(plyr)

args <- (commandArgs(TRUE)) ## load arguments from R CMD BATCH
print(args)
## Should only be one parameter argument
## data.file=path to file
if(length(args)>0)  { ## Then cycle through each element of the list and evaluate the expressions.
  print(paste0('loading in ', args, ' from R CMD BATCH'))
  for(i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}



convertToDat <- function(lines){
  lines <- lines[-1]
  data <- list()
  all_dat <- vector("list", 100)
  n<-1
  n2<-1
  for(char in lines){

    if(!grepl(char, pattern = "Simulation", fixed=T) &
         !char==lines[length(lines)]){
      data[[n]] <- strsplit(char, split=", ")[[1]]
      n<- n+1
    } else {
      if(length(data)!=0){
        all_dat[[n2]] <- do.call(rbind, data)
        class(all_dat[[n2]]) <- "numeric"
        colnames(all_dat[[n2]]) <- c("time", "S", 'E01', 'E02', "I01","I02","P1","P2",
                                     "S12","S21", "E12", "E21", "I12","I21", "R", "cumI1", "cumI2",
                                     "avgDegS1", "avgDegS2", "avgDegI1", "avgDegI2",
                                     "avgEffDegS2", "avgEffDegSqS2")
      }
      data <- list()
      n<-1
      n2<-n2+1
    }
  }
  return(all_dat)
}

getPar <- function(path){
  temp <- strsplit(path, split="/")[[1]]
  temp <- temp[length(temp)]
  temp <- strsplit(temp, split="_")[[1]]

  net <- temp[1]
  a <- as.numeric(temp[2])
  b2 <- as.numeric(temp[3])
  intro <- as.numeric(temp[4])
  p <- as.numeric(strsplit(temp[5], "\\.")[[1]][1])

  return(list(network = net, intro_time=intro, alpha=a, phi=p, beta2=b2))
}


epidemicRuns <- convertToDat(readLines(data.file))
params <- getPar(data.file)
save(list = c("epidemicRuns", "params"), file = gsub(pattern = "txt", replacement = "Rdata", x = data.file))

