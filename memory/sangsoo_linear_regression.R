## Memory cross-protection analysis simulation
## Nicholas Reich
## May 2014

# For BATCH
# Rcmd BATCH "test.R" "test.out"
# "C:\Program Files\R\R-3.1.0\bin\x64\R.exe" CMD BATCH "C:\Users\Skyler\Documents\RAworks2014_summer\Sangsoo_pompSandbox\memory\sangsoo_Beta_distribution.R" "C:\Users\Skyler\Documents\RAworks2014_summer\Sangsoo_pompSandbox\memory\sangsoo_Beta_distribution.out"

## preliminaries
setwd('~/Documents/code_versioned/pompSandbox/memory/')
source('fourStrainMemory.R')
path_File <- paste0('~/Documents/simdata_5_local/')
key <- read.csv(paste0(path_File,"key_5.csv"))
colnames(key)[1] <- "filename"

## parameters
k <- 300
lambda <- 26*4

file_idx <- 1:12000 #c(701:750, 4681:4730, 2911:2934, 2936:2961)
n_files <- length(file_idx)

##### Setting parameters of linear regression 
b <- c(0.001, 0.0001) # intercept: A & B
c <- c(0.01, 0.001) # last point
a <- (c - b)/1040 # slope calculation : 1041 -> nrow of tsir_data

# generation of 1041 reporting rates for each regression
aLinear <- a[1]*0:1040 + b[1]
bLinear <- a[2]*0:1040 + b[2]

# generation of five scenarios
order_nS <- list()
order_nS[[1]] <- cbind(aLinear, aLinear,aLinear,aLinear)
order_nS[[2]] <- cbind(aLinear, aLinear,bLinear,bLinear)
order_nS[[3]] <- cbind(aLinear, bLinear,bLinear,bLinear)
order_nS[[4]] <- cbind(aLinear, bLinear,bLinear,bLinear)
order_nS[[5]] <- cbind(bLinear, bLinear,bLinear,bLinear)

order_mat <- rbind(c("aLinear","aLinear","aLinear","aLinear"),
                   c("aLinear","aLinear","aLinear","bLinear"),
                   c("aLinear","aLinear","bLinear","bLinear"),
                   c("aLinear","bLinear","bLinear","bLinear"),
                   c("bLinear","bLinear","bLinear","bLinear"))

nS <- length(order_nS) # five scenarios

## storage: changed from 11 to 15 to include reporting rates
results <- matrix(NA, ncol=16, nrow=n_files)
colnames(results) <- c(colnames(key),"sc","rp1","rp2","rp3","rp4", "k", 
                       "lambda", "loglik", "z", "beta_M", "lambda_ci_low", "lambda_ci_high")
results[, 1] <- file_idx
results[, 2:4] <- as.matrix(key[file_idx,2:4])

######################################################################

require(foreach)
require(doMC)

nCores <- 20
registerDoMC(nCores)

allResults <- list()

tic <- Sys.time()

for (sc in 1:nS){
  
  results[, "sc"] <- sc
  results[,c("rp1","rp2","rp3","rp4")] <- order_mat[sc,] 
    
  parResults <- foreach(i = 1:n_files, .combine=rbind) %dopar% {
    
    fn <- paste0(path_File, key[file_idx[i],"filename"])
    raw_data <- read.csv(fn, row.names=1)
    tsir_data <- raw_data[,2:5]
    
    tsir_data_2 <- tsir_data/as.numeric(results[i,'rho']) # backe to the true  
    
    rRate <- order_nS[[sc]]
    
    # applying reporting rates
    for (j in 1:ncol(tsir_data_2)){ # each dengue type time series
             
            tsir_data_2[,j] <- rbinom(nrow(tsir_data_2), 
                                      prob=rRate[,j], 
                                      size=tsir_data_2[,j]) 
    }
    
    # Input= tsir_data_2 instead of tsir_data
    runCrossProtectMemoryAnalysis(data=tsir_data_2, k=k, 
                                  subset=500:1041,
                                  max_lambda=lambda, 
                                  plot=FALSE, verbose=FALSE)  
    
  }
  
  results[,10:16] <- parResults
  allResults[[sc]] <- results
}

toc <- Sys.time()
toc-tic


final <- rbind(allResults[[1]], allResults[[2]], allResults[[3]], allResults[[4]], allResults[[5]])
write.csv(final, file="regres_simResults.csv", quote=FALSE, row.names=FALSE)
