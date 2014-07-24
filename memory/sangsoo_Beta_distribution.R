## Memory cross-protection analysis simulation
## Nicholas Reich
## May 2014

# For BATCH
# Rcmd BATCH "test.R" "test.out"
# "C:\Program Files\R\R-3.1.0\bin\x64\R.exe" CMD BATCH "C:\Users\Skyler\Documents\RAworks2014_summer\Sangsoo_pompSandbox\memory\sangsoo_Beta_distribution.R" "C:\Users\Skyler\Documents\RAworks2014_summer\Sangsoo_pompSandbox\memory\sangsoo_Beta_distribution.out"

## preliminaries
path <- paste0(getwd(),"/memory/",sep="") 
source(file.path(paste0(path,"fourStrainMemory.R")))
path_File <- paste0(path, "/simdata_5_local/")
key <- read.csv(paste0(path,"key_5.csv"))
colnames(key)[1] <- "filename"

## parameters
k <- 300
lambda <- 26*4

file_idx <- 1:12000 #c(701:750, 4681:4730, 2911:2934, 2936:2961)
n_files <- length(file_idx)

# parameter set for beta distribution
a <- c(0.1, 1, 3) # alpha
b <- c(9.9, 9, 27) # beta

nS <- length(a) # three scenarios

## storage: changed from 11 to 15 to include reporting rates
results <- matrix(NA, ncol=17, nrow=n_files)
colnames(results) <- c(colnames(key),"a", "b", "rp1","rp2","rp3","rp4", "k", 
                       "lambda", "loglik", "z", "beta_M", "lambda_ci_low", "lambda_ci_high")
results[, 1] <- file_idx
results[, 2:4] <- as.matrix(key[file_idx,2:4])

######################################################################

require(foreach)
require(doParallel)

nCores <- 2
cl <- makeCluster(nCores)
registerDoParallel(cl)

allResults <- list()

tic <- Sys.time()

for (sc in 1:nS){
  
  results[, 5] <- a[sc]
  results[, 6] <- b[sc]
  results[, 7:10] <- pmax(0.0001, rbeta(4*nrow(results), a[sc], b[sc]))
  
  parResults <- foreach(i = 1:n_files, .combine=rbind) %dopar% {
    
    fn <- paste0(path_File, key[file_idx[i],"filename"])
    raw_data <- read.csv(fn, row.names=1)
    tsir_data <- raw_data[,2:5]
    
    tsir_data_2 <- tsir_data/results[i,'rho'] # backe to the true  
    
    for (j in 1:ncol(tsir_data_2)){
      
      cname <- paste0("rp", j)
      tsir_data_2[,j] <- rbinom(nrow(tsir_data_2), prob=results[i,cname], size=tsir_data_2[,j]) # applying reporting rates
    }
    
    # Input= tsir_data_2 instead of tsir_data
    runCrossProtectMemoryAnalysis(data=tsir_data_2, k=k, 
                                  subset=500:1041,
                                  max_lambda=lambda, 
                                  plot=FALSE, verbose=FALSE)  
    
  }

  results[,11:17] <- parResults
  allResults[[sc]] <- results
}

toc <- Sys.time()
toc-tic

stopCluster(cl)

final <- rbind(allResults[[1]], allResults[[2]], allResults[[3]])
write.csv(final, file="Beta_simResults.csv", quote=FALSE, row.names=FALSE)

