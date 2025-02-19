## * Header
## ** interactive
## path <- "h:/SundKonsolidering_BioStatHome/Cluster/GPC/article-restricted/"
## setwd(path)
## source("BATCH_scenario3-ImmunoVSImmuno.R")
## ** slurm
## cd ucph/hdir/SundKonsolidering_BioStatHome/Cluster/GPC/article-restricted/
## sbatch -a 1-1 -J 'scenario3-ImmunoVSImmuno' --output=/dev/null --error=/dev/null R CMD BATCH --vanilla BATCH_scenario3.R /dev/null 
## ** BATCH
## cd ucph/hdir/SundKonsolidering_BioStatHome/Cluster/GPC/article-restricted/
## R CMD BATCH --vanilla '--args iter_sim=1 n.iter_sim=10' BATCH_scenario3-ImmunoVSImmuno.R output/scenario3-ImmunoVSImmuno/R-ImmunoVSImmuno-1.Rout &
## ** BATCH loop
## cd ucph/hdir/SundKonsolidering_BioStatHome/Cluster/GPC/article-restricted/
## for ITER in `seq 1 10`;
## do
## eval 'R CMD BATCH --vanilla "--args iter_sim='$ITER' n.iter_sim=10" BATCH_scenario3-ImmunoVSImmuno.R output/scenario3-ImmunoVSImmuno/R-ImmunoVSImmuno-'$ITER'.Rout &'
## done

## [22] 3353323
## [23] 3353324
## [24] 3353325
## [25] 3353326
## [26] 3353327
## [27] 3353328
## [28] 3353329
## [29] 3353330
## [30] 3353331
## [31] 3353332

rm(list = ls())
gc()

## * Seed
args <- commandArgs(TRUE) ## BATCH MODE
if(length(args)>0){
    for (arg in args){
        eval(parse(text=arg))
    }
}else{ ## SLUMR
    iter_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
    n.iter_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
} ## interactive
if(is.na(iter_sim)){iter_sim <- 1}
if(is.na(n.iter_sim)){n.iter_sim <- 10}
cat("iteration ",iter_sim," over ",n.iter_sim,"\n",sep="")

set.seed(1)
seqSeed <- sample(1:max(1e5,n.iter_sim),size=n.iter_sim,replace=FALSE)
iSeed <- seqSeed[iter_sim]
set.seed(iSeed)

cat("seed: ",iSeed,"\n")

## * Prepare export
path <- "."
path.res <- file.path(path,"Results","scenario3-ImmunoVSImmuno")
if(dir.exists(path.res)==FALSE){
    if(dir.exists(file.path(path,"Results"))==FALSE){
    dir.create(file.path(path,"Results"))
    }
    dir.create(path.res)
}
path.output <- file.path(path,"output","scenario3-ImmunoVSImmuno")
if(dir.exists(path.output)==FALSE){
    if(dir.exists(file.path(path,"output"))==FALSE){
    dir.create(file.path(path,"output"))
    }
    dir.create(path.output)
}

## * Libraries
require(survival)
require(BuyseTest) ## install.packages("BuyseTest")
require(survRM2) ## install.packages("survRM2")
require(FHtest) ## install.packages("FHtest")

## * Settings
n.sim <- 500

Tps.inclusion <- 12 
Restriction.time_list <- c(12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60) ## every 3 months
Threshold_list <- c(0,6,12) ## 0,6,12
TpsFin <- 60

grid <- expand.grid(restrictionTime = Restriction.time_list,
                    threshold = Threshold_list,
                    scenario = 0)

grid <- rbind(grid,
              cbind(restrictionTime = Restriction.time_list,
                    threshold = 0.1*Restriction.time_list,
                    scenario = 1))

#pour faire varier les ratio threshold/restriction time
grid <- rbind(grid,
              cbind(restrictionTime = Restriction.time_list,
                    threshold = 0.05*Restriction.time_list,
                    scenario = 2))

grid <- rbind(grid,
              cbind(restrictionTime = Restriction.time_list,
                    threshold = 0.2*Restriction.time_list,
                    scenario = 3))

#pour obtenir la valeur exacte du net benefit sans censure
grid <- rbind(grid,
              cbind(restrictionTime = 1000,
                    threshold = Threshold_list,
                    scenario = 4))

## * Loop
res <- NULL
for(iSim in 1:n.sim){ ## iSim <- 1
    cat(iSim,": ")
    for (iGrid in 1:NROW(grid)){ 
        cat("*")

        iThreshold <- grid$threshold[iGrid]
        iTime <- grid$restrictionTime[iGrid]
        iScenario <- grid$scenario[iGrid]

        ## ** Generate data
        HR1C <- 0.1
        HR1T <- 0.1
      
        TpsFin <- iTime
        HazC <- 0.1
        HazT <- 0.07
      
        HazT2T <- HazT*(0.75+0.25*HR1T)
        HazT3T <- HazT*(0.5+0.5*HR1T)
        HazT4T <- HazT*(0.25+0.75*HR1T)
        HazT5T <- HazT*(HR1T)
      
        HazT2C <- HazC*(0.75+0.25*HR1C)
        HazT3C <- HazC*(0.5+0.5*HR1C)
        HazT4C <- HazC*(0.25+0.75*HR1C)
        HazT5C <- HazC*(HR1C)
      
        t1 <- 3
        t2 <- 12
        t3 <- 17.75
        t4 <- 24
        
        t1C <- 12
        t2C <- 24
        t3C <- 36
        t4C <- 48
        
        n.Treatment <- 200
        n.Control <- 200
        n <- n.Treatment+n.Control
        group <- c(rep(1, n.Treatment),rep(0, n.Control))
      
        ## *** in control group
        TimeEvent.Ctr1 <- rexp(n.Control,HazC)
        TimeEvent.Ctr2 <- rexp(n.Control,HazT2C)
        TimeEvent.Ctr3 <- rexp(n.Control,HazT3C)
        TimeEvent.Ctr4 <- rexp(n.Control,HazT4C)
        TimeEvent.Ctr5 <- rexp(n.Control,HazT5C)
        
        TimeEvent.Ctr <- ifelse(TimeEvent.Ctr1<t1C,TimeEvent.Ctr1,
                              ifelse(t1C+TimeEvent.Ctr2<t2C,t1C+TimeEvent.Ctr2,
                                     ifelse(t2C+TimeEvent.Ctr3<t3C,t2C+TimeEvent.Ctr3,
                                            ifelse(t3C+TimeEvent.Ctr4<t4C,t3C+TimeEvent.Ctr4,
                                                   t4C+TimeEvent.Ctr5))))
        
        ## *** in treatment group
        TimeEvent.Tr1 <- rexp(n.Control,HazT)
        TimeEvent.Tr2 <- rexp(n.Control,HazT2T)
        TimeEvent.Tr3 <- rexp(n.Control,HazT3T)
        TimeEvent.Tr4 <- rexp(n.Control,HazT4T)
        TimeEvent.Tr5 <- rexp(n.Control,HazT5T)
        TimeEvent.Tr <- ifelse(TimeEvent.Tr1<t1,TimeEvent.Tr1,
                             ifelse(t1+TimeEvent.Tr2<t2,t1+TimeEvent.Tr2,
                                    ifelse(t2+TimeEvent.Tr3<t3,t2+TimeEvent.Tr3,
                                           ifelse(t3+TimeEvent.Tr4<t4,t3+TimeEvent.Tr4,
                                                  t4+TimeEvent.Tr5))))
        
        TimeEvent <- c(TimeEvent.Tr,TimeEvent.Ctr)
        Time.Cens <- runif(n,48,TpsFin)
        Time <- pmin(Time.Cens,TimeEvent)
        Event <- Time==TimeEvent
        Event <- as.numeric(Event)
        tab <- data.frame(group,Time,Event)
        
        ## ** Analysis using LR
        LR <- (survdiff(Surv(time=Time, event=Event) ~ group, data=tab, rho=0))
        pval.LR <- 1 - pchisq(LR$chisq, 1) 
        Taux.cens.reel <- 1-mean(Event)
  
        ## ** analysis using RNBGehan
        NBGehan <- BuyseTest(data=tab,group ~ TTE(Time, status=Event, iThreshold),
                              method.inference = "u-statistic", scoring.rule = "Gehan", trace = 0)
        NBGehan.confint <- confint(NBGehan)

        ## ** Analysis using RMST
        RMST <- rmst2(time=Time, status=Event, arm=group, tau = NULL, covariates = NULL, alpha = 0.05)
        pval.RMSTdif <- RMST[["unadjusted.result"]][1,4]
        pval.RMSTratio <- RMST[["unadjusted.result"]][2,4]
  
        ## ** Analysis using WLR # modifi? dans la version 2
        WLR <- try(FHtestrcc(Surv(time=Time, event=Event) ~ group, data = tab, rho = 0,lambda = 1))
        if(inherits(WLR,"try-error")){
            pval.WLR <- NA
        }else{
            pval.WLR <- WLR$pvalue
        }

        ## ** Analysis using RNBPeron
        RNBPeron <- BuyseTest(data=tab,group ~ TTE(Time, status=Event, iThreshold, restriction=iTime),
                                  method.inference = "u-statistic", scoring.rule = "Peron", trace = 0)
        RNBPeron.confint <- confint(RNBPeron)
  
        ## ** créer la table de résultats
        res <- rbind(res,c(iteration = iSim,
                           Restriction_time = iTime,
                           scenario = iScenario,
                           Threshold = iThreshold,
                           "Taux censure reel" = Taux.cens.reel,
                           estimate.NBGehan = NBGehan.confint[,"estimate"],
                           se.NBGehan = NBGehan.confint[,"se"],
                           lower.NBGehan = NBGehan.confint[,"lower.ci"],
                           upper.NBGehan = NBGehan.confint[,"upper.ci"],
                           pval.NBGehan = NBGehan.confint[,"p.value"],
                           estimate.RNBPeron = RNBPeron.confint[,"estimate"],
                           se.RNBPeron = RNBPeron.confint[,"se"],
                           lower.RNBPeron = RNBPeron.confint[,"lower.ci"],
                           upper.RNBPeron = RNBPeron.confint[,"upper.ci"],
                           pval.RNBPeron = RNBPeron.confint[,"p.value"],
                           pval.LOGRANK = pval.LR,
                           pval.WeightedLOGRANK = pval.WLR,
                           pval.RMSTdif = pval.RMSTdif,
                           pval.RMSTratio = pval.RMSTratio))

    }
    saveRDS(res, file = file.path(path.res,paste0("simul_ImmunoVSImmuno_",iter_sim,"(tempo).rds")))
    cat("\n")
}

## * Export
saveRDS(res, file = file.path(path.res,paste0("simul_ImmunoVSImmuno_",iter_sim,".rds")))

## * R version
print(sessionInfo())


	
