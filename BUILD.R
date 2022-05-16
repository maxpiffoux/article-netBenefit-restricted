### BUILD.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: apr  7 2022 (11:10) 
## Version: 
## Last-Updated: maj 16 2022 (11:56) 
##           By: Brice Ozenne
##     Update #: 39
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)

if(system("whoami",intern=TRUE)=="unicph\\hpl802"){  

}else{
    setwd("C:/Users/max/Desktop/simulation_peron/simulation-article-restricted")
}

## * function used to collect the results from different files
loadRes <- function(path, tempo.file = FALSE, type = NULL,
                    export.attribute = NULL, trace = TRUE){
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

    ls.out <- do.call(myApply, args = list(X = 1:n.file, FUN = function(iFile){
        iRead <- readRDS(file = file.path(path,file.read[iFile]))
        iOut <- cbind(data.table::as.data.table(iRead),
                      file = file.read[iFile])
        return(iOut)
    }))
    out <- do.call(rbind, ls.out)
    return(out)
}

## * Scenario 1
dt.sc1 <- loadRes("Results/scenario1-ChemoVSChemo")
setnames(dt.sc1, new = "censure", old = "Taux censure reel")
setnames(dt.sc1, new = "threshold", old = "Threshold")
setnames(dt.sc1, new = "rtime", old = "Restriction_time")

dtS.sc1 <- dt.sc1[, .(rep = .N, censure = mean(censure),
                      power.logrank = mean(pvalLOGRANK<=0.05),
                      power.wlogrank = mean(pvalWeightedLOGRANK<=0.05, na.rm = TRUE),
                      power.rmstDiff = mean(pvalRMSTdif<=0.05),
                      power.rmstRatio = mean(pvalRMSTratio<=0.05),
                      power.nbGehan = mean(pval.RNBGehan<=0.05),
                      estimate.nbGehan = mean(RNBenefit.Gehan),
                      power.rnbPeron = mean(pvalRNB<=0.05),
                      estimate.rnbPeron = mean(RNB)
                      ), by = c("scenario","threshold","rtime") ]


## * Scenario 2
dt.sc2 <- loadRes("Results/scenario2-ChemoVSImmuno")
setnames(dt.sc2, new = "censure", old = "Taux censure reel")
setnames(dt.sc2, new = "threshold", old = "Threshold")
setnames(dt.sc2, new = "rtime", old = "Restriction_time")

dtS.sc2 <- dt.sc2[, .(rep = .N, censure = mean(censure),
                      power.logrank = mean(pvalLOGRANK<=0.05),
                      power.wlogrank = mean(pvalWeightedLOGRANK<=0.05, na.rm = TRUE),
                      power.rmstDiff = mean(pvalRMSTdif<=0.05),
                      power.rmstRatio = mean(pvalRMSTratio<=0.05),
                      power.nbGehan = mean(pval.RNBGehan<=0.05),
                      estimate.nbGehan = mean(RNBenefit.Gehan),
                      power.rnbPeron = mean(pvalRNB<=0.05),
                      estimate.rnbPeron = mean(RNB)
                      ), by = c("scenario","threshold","rtime") ]


## * Scenario 3
dt.sc3 <- loadRes("Results/scenario3-ImmunoVSImmuno")
setnames(dt.sc3, new = "censure", old = "Taux censure reel")
setnames(dt.sc3, new = "threshold", old = "Threshold")
setnames(dt.sc3, new = "rtime", old = "Restriction_time")

dtS.sc3 <- dt.sc3[, .(rep = .N, censure = mean(censure),
                      power.logrank = mean(pvalLOGRANK<=0.05),
                      power.wlogrank = mean(pvalWeightedLOGRANK<=0.05, na.rm = TRUE),
                      power.rmstDiff = mean(pvalRMSTdif<=0.05),
                      power.rmstRatio = mean(pvalRMSTratio<=0.05),
                      power.nbGehan = mean(pval.RNBGehan<=0.05),
                      estimate.nbGehan = mean(RNBenefit.Gehan),
                      power.rnbPeron = mean(pvalRNB<=0.05),
                      estimate.rnbPeron = mean(RNB),
                      sd.rnbPeron = sd(RNB)),
                  by = c("scenario","threshold","rtime") ]


## * export
saveRDS(dt.sc1, file = "Results/sim-ChemoVSChemo.rds")
saveRDS(dt.sc2, file = "Results/sim-ChemoVSImmuno.rds")
saveRDS(dt.sc3, file = "Results/sim-ImmunoVSImmuno.rds")

saveRDS(dtS.sc1, file = "Results/simSummary-ChemoVSChemo.rds")
saveRDS(dtS.sc2, file = "Results/simSummary-ChemoVSImmuno.rds")
saveRDS(dtS.sc3, file = "Results/simSummary-ImmunoVSImmuno.rds")


##----------------------------------------------------------------------
### BUILD.R ends here
