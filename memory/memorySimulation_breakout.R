## Memory cross-protection analysis simulation
## Nicholas Reich
## May 2014

## preliminaries
setwd('~/Documents/code_versioned/pompSandbox/memory/')
source('~/Documents/code_versioned/pompSandbox/memory/fourStrainMemory.R')
path <- "~/Documents/work/dengueData/simdata_5_local/"
key <- read.csv("~/Documents/work/research/dengueCrossProtection/data/simulatedData_fromMI/key_5.csv")
colnames(key)[1] <- "filename"

## parameters
k <- 300
lambda <- 26*4
file_idx <- 1:12000 #c(701:750, 4681:4730, 2911:2934, 2936:2961)
n_files <- length(file_idx)

## storage
results <- matrix(NA, ncol=12, nrow=n_files)
colnames(results) <- c(colnames(key), "k", "lambda", "loglik", "z", "beta_M", "beta_Mself", "lambda_ci_low", "lambda_ci_high")
results[,1] <- file_idx
results[,2:4] <- as.matrix(key[file_idx,2:4])

## create loop for each filename
require(doMC)
registerDoMC(18)
tic <- Sys.time()
parResults <- foreach(i = 1:n_files, .combine=rbind) %dopar% {
        fn <- paste0(path, key[file_idx[i],"filename"])
        raw_data <- read.csv(fn, row.names=1)
        tsir_data <- raw_data[,2:5]
        runCrossProtectMemoryAnalysis(data=tsir_data, k=k, 
                                      subset=500:1041,
                                      max_lambda=lambda, 
                                      plot=FALSE, verbose=FALSE)
}
toc <- Sys.time()
toc-tic
results[,5:12] <- parResults

write.csv(results, file="simResults.csv", quote=FALSE, row.names=FALSE)

#results <- read.csv("simResults_20140611.csv")

results <- as.data.frame(results)
results$trueLambda <- 26/results$delta
results$memoryIndicator <- results$lambda>13# abs(results$beta_M)>.02
results$ciCover <- results$trueLambda>=results$lambda_ci_low & results$trueLambda<=results$lambda_ci_high
results$ciWidth <- results$lambda_ci_high-results$lambda_ci_low
results$bias <- results$lambda - results$trueLambda
results$cpPresent <- results$lambda_ci_low>0
with(results, table(trueLambda, ciCover))/2000
with(results, table(trueLambda, cpPresent))/2000

#results
qplot(factor(trueLambda), lambda, data=results, geom="violin")
qplot(factor(trueLambda), lambda, data=results, geom="boxplot", facets=.~rho)
qplot(factor(trueLambda), lambda_ci_high, data=results, geom="boxplot", facets=.~rho)
qplot(factor(trueLambda), lambda_ci_low, data=results, geom="boxplot", facets=.~rho)
qplot(factor(trueLambda), ciWidth, data=results, geom="boxplot", facets=.~rho)
qplot(factor(trueLambda), ciWidth, data=results, geom="violin", facets=.~rho)

qplot(factor(trueLambda), bias/trueLambda, data=results, geom="boxplot", facets=.~rho) + ylim(-3,3)

qplot(factor(trueLambda), z, data=results, geom="violin")
qplot(factor(trueLambda), z, data=results, geom="boxplot")
qplot(factor(trueLambda), sign(z)*lambda, data=results, geom="boxplot")
qplot(factor(trueLambda), beta_M, data=results, geom="boxplot")
qplot(factor(trueLambda), beta_M, data=results, geom="violin")
qplot(factor(memoryIndicator), data=results, geom="bar", fill=factor(trueLambda))

## run Dengue analysis
load("/Users/nick/Dropbox/work/research/dengueCrossProtection/data/bkk.dengue.cases.new.rda")
dengue_data <- bkk.dengue.cases[,3:6]
dengue_data[which(dengue_data<0, arr.ind=TRUE)] <- 0 ## remove negative values
tmp <- runCrossProtectMemoryAnalysis(data=dengue_data, k=300, 
                                     max_lambda=26*6, 
                                     plot=TRUE, verbose=FALSE)

## run flu A, flu B, RSV and paraflu
rsv_data <- read.csv("/Users/nick/Dropbox/work/research/maskStudy/manuscripts/ALERTv2/dataCleaned/chco.csv")
rsv_data_subset <- rsv_data[,c("RSV", "Total.Flu.A", "Flu.B", "Paraflu")]
tmp <- runCrossProtectMemoryAnalysis(data=rsv_data_subset, k=120, 
                                     max_lambda=100, 
                                     plot=TRUE, verbose=FALSE)

rsv_data_subset <- rsv_data[,c("RSV", "Total.Flu.A", "Flu.B", "Rhinovirus")]
tmp <- runCrossProtectMemoryAnalysis(data=rsv_data_subset, k=120, 
                                     max_lambda=100, 
                                     plot=TRUE, verbose=FALSE)

rsv_data_subset <- rsv_data[,c("RSV", "Total.Flu.A", "Flu.B", "Adenovirus")]
tmp <- runCrossProtectMemoryAnalysis(data=rsv_data_subset, k=120, 
                                     max_lambda=100, 
                                     plot=TRUE, verbose=FALSE)

rsv_data_subset <- rsv_data[,c("RSV", "Total.Flu.A", "Flu.B", "Enterovirus")]
tmp <- runCrossProtectMemoryAnalysis(data=rsv_data_subset, k=120, 
                                     max_lambda=100, 
                                     plot=TRUE, verbose=FALSE)
