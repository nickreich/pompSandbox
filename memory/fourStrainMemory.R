#################################
## Create 4-strain memory term ##
#################################

createMemory <- function(dat, k, lambda=NULL) {
        ## assume that dat is data from a four-strain model, columns are case counts
        ## k is a duration
        ## lambda is an exponential parameter
        ## first, we will create a matrix to help with the memory term summations.
        ## this matrix will have 0s and 1s and will be multiplied by the data vectors
        
        ## vector of 1s of length k
        if(is.null(lambda)) {
                vec1 <- rep(1, k)
        } else {
                ## rough cumulative probabilities of still being susceptible
                vec1 <- 1-pexp((k-1):0, rate=1/lambda)
                ## remove most recent kstar counts from consideration to avoid autocorrelation
                kstar <- 10
                vec1[(k-kstar+1):k] <- 0
        }
        ## vector of 0s of length ncol(dat)-k+1
        vec0 <- rep(0, nrow(dat)-k+1)
        memMatrixRows <- nrow(dat)-k

        ## the matrix for self strain
        firstReps <- rep(c(rep(1, k), vec0), times=memMatrixRows-1)
        last1s <- c(rep(1, k),0)
        memMatrixSelf <- matrix(c(firstReps, last1s), byrow=TRUE, nrow=memMatrixRows)
        
        ## the matrix for other strains
        firstReps <- rep(c(vec1, vec0), times=memMatrixRows-1)
        last1s <- c(vec1,0)
        memMatrix <- matrix(c(firstReps, last1s), byrow=TRUE, nrow=memMatrixRows)
        M1 <- memMatrixSelf%*%dat[,1] +  memMatrix%*%dat[,2] + memMatrix%*%dat[,3] + memMatrix%*%dat[,4]  
        M1 <- ( M1-mean(M1) ) / sd(M1)
        M2 <- memMatrix%*%dat[,1] + memMatrixSelf%*%dat[,2] +memMatrix%*%dat[,3] + memMatrix%*%dat[,4]  
        M2 <- ( M2-mean(M2) ) / sd(M2)
        M3 <- memMatrix%*%dat[,1] + memMatrix%*%dat[,2] + memMatrixSelf%*%dat[,3] +memMatrix%*%dat[,4]  
        M3 <- ( M3-mean(M3) ) / sd(M3)
        M4 <- memMatrix%*%dat[,1] + memMatrix%*%dat[,2] + memMatrix%*%dat[,3] + memMatrixSelf%*%dat[,4]
        M4 <- ( M4-mean(M4) ) / sd(M4)
        
        dataIdx <- (k+1):nrow(dat)
        dat1 <- data.frame(y=dat[dataIdx,1], 
                           yAR = dat[dataIdx-1, 1],
                           M=M1, 
                           time=1:length(dataIdx),
                           strain=1)
        dat2 <- data.frame(y=dat[dataIdx,2], 
                           yAR = dat[dataIdx-1, 2],
                           M=M2, 
                           time=1:length(dataIdx),
                           strain=2)
        dat3 <- data.frame(y=dat[dataIdx,3], 
                           yAR = dat[dataIdx-1, 3],
                           M=M3, 
                           time=1:length(dataIdx),
                           strain=3)
        dat4 <- data.frame(y=dat[dataIdx,4], 
                           yAR = dat[dataIdx-1, 4],
                           M=M4, 
                           time=1:length(dataIdx),
                           strain=4)
        data <- rbind(dat1, dat2, dat3, dat4)
        return(data)
}


runCrossProtectMemoryAnalysis <- function(data, subset=NULL, k, max_lambda, plot=FALSE, verbose=TRUE) {
        if(!is.null(subset)){
                data <- data[subset,]
        }
        nLam <- max_lambda+1
        logLiks <- matrix(NA, ncol=5, nrow=nLam)
        colnames(logLiks) <- c("k", "lambda", "loglik", "z", "beta_M")
        for(i in 1:nLam){
                logLiks[i,"k"] <- k
                lambda <- logLiks[i,"lambda"] <- i-1
                dat <- createMemory(data, k=k, lambda=lambda)
                dat$t <- 1:nrow(dat)
                m1 <- glm(y ~ factor(strain) + log(yAR+1) + M, data=dat, family="poisson")
                logLiks[i,"z"] <- summary(m1)$coef["M","z value"] #logLik(m1)
                logLiks[i,"loglik"] <- logLik(m1)
                logLiks[i,"beta_M"] <- summary(m1)$coef["M","Estimate"]
                if(verbose)
                        message(paste("finished iteration", i, Sys.time()))
        }
        
        if(plot) {
                require(gridExtra)
                p1 <- qplot(lambda, loglik, data=data.frame(logLiks), geom="line")
                #p2 <- qplot(lambda, z, data=data.frame(logLiks), geom="line")       
                #p3 <- qplot(lambda, beta_M, data=data.frame(logLiks), geom="line")       
                grid.arrange(p1, ncol=1)
        }
        
        ## find "best" lambda
        idx_best_ll <- which(logLiks[,"loglik"]==max(logLiks[,"loglik"]))
        if(length(idx_best_ll)!=1) {
                warning("multiple log-likelihood maxima. first max selected.")
                idx_best_ll <- min(idx_best_ll)
        }
        
        
        ## compare likelihoods obtain chi-square based confidence interval
        chisq_threshold <- qchisq(.95, df=1)/2
        idx_ci_ll <- which(logLiks[,"loglik"]>=logLiks[idx_best_ll,"loglik"]-chisq_threshold)
        ci_low <- min(logLiks[idx_ci_ll,"lambda"])
        ci_high <- max(logLiks[idx_ci_ll,"lambda"])
        
        return(c(logLiks[idx_best_ll,], lambda_ci_low=ci_low, lambda_ci_high=ci_high))
}