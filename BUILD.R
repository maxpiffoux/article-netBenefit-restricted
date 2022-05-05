### BUILD.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: apr  7 2022 (11:10) 
## Version: 
## Last-Updated: apr 20 2022 (11:47) 
##           By: Brice Ozenne
##     Update #: 19
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)
library(ggplot2)

## * function used to collect the results from different files
loadRes <- function(path, tempo.file = FALSE, type = NULL,
                    export.name = FALSE, trace = TRUE){
    all.files <- list.files(path)
    file.tempo <- grep("(tempo)",all.files,value = TRUE)
    file.final <- setdiff(all.files, file.tempo)

    if(tempo.file){
        file.read <- file.tempo
    }else{
        file.read <- file.final
    }
    if(!is.null(type)){
        file.read <- grep(pattern=type,x=file.read,value=TRUE)
    }
    
    n.file <- length(file.read)

    myApply <- switch(as.character(as.logical(trace)),
                      "TRUE" = pbapply::pblapply,
                      "FALSE" = lapply)

    if(export.name){ ## export name of the files
        return(file.path(path,file.read))
    }else{ ## export content of the files
        ls.out <- do.call(myApply, args = list(X = 1:n.file, FUN = function(iFile){
            iRead <- readRDS(file = file.path(path,file.read[iFile]))
            iOut <- cbind(data.table::as.data.table(iRead),
                          file = file.read[iFile])
            return(iOut)
        }))
        out <- do.call(rbind, ls.out)
        return(out)
    }
}

## * Scenario 1
## loadRes("Results/scenario1-ChemoVSChemo", export.name = TRUE)
## file.exists(loadRes("Results/scenario1-ChemoVSChemo", export.name = TRUE))
## readRDS(loadRes("Results/scenario1-ChemoVSChemo", export.name = TRUE)[1])
dt.sc1 <- loadRes("Results/scenario1-ChemoVSChemo")


setnames(dt.sc1, new = "censure", old = "Taux censure reel")
setnames(dt.sc1, new = "threshold", old = "Threshold")
setnames(dt.sc1, new = "rtime", old = "Restriction_time")

dtS.sc1 <- dt.sc1[, .(rep = .N, censure = mean(censure),
                      power.logrank = mean(pvalLOGRANK<=0.05),
                      power.wlogrank = mean(pvalWeightedLOGRANK<=0.05),
                      power.rmstDiff = mean(pvalRMSTdif<=0.05),
                      power.rmstRatio = mean(pvalRMSTratio<=0.05),
                      power.nbGehan = mean(pval.RNBGehan<=0.05),
                      estimate.nbGehan = mean(RNBenefit.Gehan),
                      power.rnbPeron = mean(pvalRNB<=0.05),
                      estimate.rnbPeron = mean(RNB)
                      ), by = c("scenario","threshold","rtime") ]

dtEstimate.sc1 <- melt(dtS.sc1, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("estimate."),
                 value.name = c("estimate"), variable.name = "estimator")
dtEstimate.sc1[,estimator := gsub("estimate.","",estimator)]

dtPower.sc1 <- melt(dtS.sc1, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("power."),
                 value.name = c("power"), variable.name = "estimator")
dtPower.sc1[,estimator := gsub("power.","",estimator)]


ggPower1 <- ggplot(dtEstimate.sc1[scenario==0], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggPower1 <- ggPower1 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower1 <- ggPower1 + xlab("restriction time")
ggPower1

ggPower1 <- ggplot(dtPower.sc1[scenario==0], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower1 <- ggPower1 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower1 <- ggPower1 + xlab("restriction time")
ggPower1


## * Scenario 2
dt.sc2 <- loadRes("Results/scenario2-ChemoVSImmuno")
setnames(dt.sc2, new = "censure", old = "Taux censure reel")
setnames(dt.sc2, new = "threshold", old = "Threshold")
setnames(dt.sc2, new = "rtime", old = "Restriction_time")

dtS.sc2 <- dt.sc2[, .(rep = .N, censure = mean(censure),
                      power.logrank = mean(pvalLOGRANK<=0.05),
                      power.wlogrank = mean(pvalWeightedLOGRANK<=0.05),
                      power.rmstDiff = mean(pvalRMSTdif<=0.05),
                      power.rmstRatio = mean(pvalRMSTratio<=0.05),
                      power.nbGehan = mean(pval.RNBGehan<=0.05),
                      estimate.nbGehan = mean(RNBenefit.Gehan),
                      power.rnbPeron = mean(pvalRNB<=0.05),
                      estimate.rnbPeron = mean(RNB)
                      ), by = c("scenario","threshold","rtime") ]

dtEstimate.sc2 <- melt(dtS.sc2, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("estimate."),
                 value.name = c("estimate"), variable.name = "estimator")
dtEstimate.sc2[,estimator := gsub("estimate.","",estimator)]

dtPower.sc2 <- melt(dtS.sc2, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("power."),
                 value.name = c("power"), variable.name = "estimator")
dtPower.sc2[,estimator := gsub("power.","",estimator)]


ggPower2 <- ggplot(dtEstimate.sc2[scenario==0], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggPower2 <- ggPower2 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower2 <- ggPower2 + xlab("restriction time")
ggPower2

ggPower2 <- ggplot(dtPower.sc2[scenario==0], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower2 <- ggPower2 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower2 <- ggPower2 + xlab("restriction time")
ggPower2

## * Scenario 3
dt.sc3 <- loadRes("Results/scenario3-ImmunoVSImmuno")
dt.sc3$scenario <- as.numeric(dt.sc3$Threshold %in% c(0,6,12) == FALSE)
setnames(dt.sc3, new = "censure", old = "Taux censure reel")
setnames(dt.sc3, new = "threshold", old = "Threshold")
setnames(dt.sc3, new = "rtime", old = "Restriction_time")

dtS.sc3 <- dt.sc3[, .(rep = .N, censure = mean(censure),
                      power.logrank = mean(pvalLOGRANK<=0.05),
                      power.wlogrank = mean(pvalWeightedLOGRANK<=0.05),
                      power.rmstDiff = mean(pvalRMSTdif<=0.05),
                      power.rmstRatio = mean(pvalRMSTratio<=0.05),
                      power.nbGehan = mean(pval.RNBGehan<=0.05),
                      estimate.nbGehan = mean(RNBenefit.Gehan),
                      power.rnbPeron = mean(pvalRNB<=0.05),
                      estimate.rnbPeron = mean(RNB)
                      ), by = c("scenario","threshold","rtime") ]

dtEstimate.sc3 <- melt(dtS.sc3, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("estimate."),
                 value.name = c("estimate"), variable.name = "estimator")
dtEstimate.sc3[,estimator := gsub("estimate.","",estimator)]

dtPower.sc3 <- melt(dtS.sc3, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("power."),
                 value.name = c("power"), variable.name = "estimator")
dtPower.sc3[,estimator := gsub("power.","",estimator)]


ggPower3 <- ggplot(dtEstimate.sc3[scenario==0], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggPower3 <- ggPower3 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower3 <- ggPower3 + xlab("restriction time")
ggPower3

ggPower3 <- ggplot(dtPower.sc3[scenario==0], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower3 <- ggPower3 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower3 <- ggPower3 + xlab("restriction time")
ggPower3

##----------------------------------------------------------------------
### BUILD.R ends here
