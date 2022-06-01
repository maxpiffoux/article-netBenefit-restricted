### FIGURE.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: maj  9 2022 (09:53) 
## Version: 
## Last-Updated: jun  1 2022 (19:46) 
##           By: Brice Ozenne
##     Update #: 14
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

if(system("whoami",intern=TRUE)=="unicph\\hpl802"){  
    setwd("x:/GPC/article-restricted/")
}else{
    setwd("C:/Users/max/Desktop/simulation_peron/simulation-article-restricted")
}

## * Load results
dtS.sc1 <- readRDS(file = "Results/simSummary-ChemoVSChemo.rds")
dtS.sc2 <- readRDS(file = "Results/simSummary-ChemoVSImmuno.rds")
dtS.sc3 <- readRDS(file = "Results/simSummary-ImmunoVSImmuno.rds")
dtS.sc4 <- readRDS(file = "Results/simSummary-type1.rds")

## * Generate plots
## ** scenario 1
dtEstimate.sc1 <- melt(dtS.sc1, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("estimate."),
                 value.name = c("estimate"), variable.name = "estimator")
dtEstimate.sc1[,estimator := gsub("estimate.","",estimator)]

dtPower.sc1 <- melt(dtS.sc1, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("power."),
                 value.name = c("power"), variable.name = "estimator")
dtPower.sc1[,estimator := gsub("power.","",estimator)]

## petit changement de nom du graphe pour ne pas melanger
ggBenefit1 <- ggplot( dtEstimate.sc1[scenario == 0], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggBenefit1 <- ggBenefit1 + geom_point() + geom_line() + facet_grid(~threshold, labeller = label_both)
ggBenefit1 <- ggBenefit1 + xlab("Follow-up time (months)") + ylab("Benefit")
ggBenefit1

ggBenefit1.bis <- ggplot( dtEstimate.sc1[scenario %in% 1:3], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggBenefit1.bis <- ggBenefit1.bis + geom_point() + geom_line() + facet_wrap(~scenario, labeller = label_both)
ggBenefit1.bis <- ggBenefit1.bis + xlab("Follow-up time (months)") + ylab("Benefit")
ggBenefit1.bis

ggPower1 <- ggplot(dtPower.sc1[scenario==0], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower1 <- ggPower1 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower1 <- ggPower1 + xlab("Follow-up time (months)")
ggPower1

ggPower1.bis <- ggplot(dtPower.sc1[scenario %in% 1:3], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower1.bis <- ggPower1.bis + geom_point() + geom_line() + facet_wrap(~scenario, labeller = label_both)
ggPower1.bis <- ggPower1.bis + xlab("Follow-up time (months)")
ggPower1.bis

## ** scenario 2
dtEstimate.sc2 <- melt(dtS.sc2, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("estimate."),
                 value.name = c("estimate"), variable.name = "estimator")
dtEstimate.sc2[,estimator := gsub("estimate.","",estimator)]

dtPower.sc2 <- melt(dtS.sc2, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("power."),
                 value.name = c("power"), variable.name = "estimator")
dtPower.sc2[,estimator := gsub("power.","",estimator)]

## petit changement de nom du graphe pour ne pas melanger
ggBenefit2 <- ggplot(dtEstimate.sc2[scenario==0], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggBenefit2 <- ggBenefit2 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggBenefit2 <- ggBenefit2 + xlab("Follow-up time (months)") + ylab("Benefit")
ggBenefit2

ggBenefit2.bis <- ggplot(dtEstimate.sc2[scenario %in% 1:3], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggBenefit2.bis <- ggBenefit2.bis + geom_point() + geom_line() + facet_wrap(~scenario, labeller = label_both)
ggBenefit2.bis <- ggBenefit2.bis + xlab("Follow-up time (months)") + ylab("Benefit")
ggBenefit2.bis

ggPower2 <- ggplot(dtPower.sc2[scenario==0], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower2 <- ggPower2 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower2 <- ggPower2 + xlab("Follow-up time (months)")
ggPower2

ggPower2.bis <- ggplot(dtPower.sc2[scenario %in% 1:3], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower2.bis <- ggPower2.bis + geom_point() + geom_line() + facet_wrap(~scenario, labeller = label_both)
ggPower2.bis <- ggPower2.bis + xlab("Follow-up time (months)")
ggPower2.bis

## ** scenario 3
dtEstimate.sc3 <- melt(dtS.sc3, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("estimate."),
                 value.name = c("estimate"), variable.name = "estimator")
dtEstimate.sc3[,estimator := gsub("estimate.","",estimator)]

dtPower.sc3 <- melt(dtS.sc3, id.vars = c("rep","censure","scenario","threshold","rtime"),
                 measure = patterns("power."),
                 value.name = c("power"), variable.name = "estimator")
dtPower.sc3[,estimator := gsub("power.","",estimator)]

## petit changement de nom du graphe pour ne pas melanger
ggBenefit3 <- ggplot(dtEstimate.sc3[scenario==0], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggBenefit3 <- ggBenefit3 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggBenefit3 <- ggBenefit3 + xlab("Follow-up time (months)") + ylab("Benefit")
ggBenefit3

ggBenefit3.bis <- ggplot(dtEstimate.sc3[scenario %in% 1:3], aes(x = rtime, y = estimate, group = estimator, color = estimator))
ggBenefit3.bis <- ggBenefit3.bis + geom_point() + geom_line() + facet_wrap(~scenario, labeller = label_both)
ggBenefit3.bis <- ggBenefit3.bis + xlab("Follow-up time (months)") + ylab("Benefit")
ggBenefit3.bis

ggPower3 <- ggplot(dtPower.sc3[scenario==0], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower3 <- ggPower3 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggPower3 <- ggPower3 + xlab("Follow-up time (months)")
ggPower3

ggPower3.bis <- ggplot(dtPower.sc3[scenario %in% 1:3], aes(x = rtime, y = power, group = estimator, color = estimator))
ggPower3.bis <- ggPower3.bis + geom_point() + geom_line() + facet_wrap(~scenario, labeller = label_both)
ggPower3.bis <- ggPower3.bis + xlab("Follow-up time (months)")
ggPower3.bis

## ** scenario 4
range(dtS.sc4$power.logrank)
range(dtS.sc4$power.wlogrank)
range(dtS.sc4$power.rmstDiff)
range(dtS.sc4$power.nbGehan)
range(dtS.sc4$power.rnbPeron)

ggType1 <- ggplot(dtS.sc4[scenario==0], aes(x = rtime, y = power.rnbPeron))
ggType1 <- ggType1 + geom_point() + geom_line() + facet_wrap(~threshold, labeller = label_both)
ggType1 <- ggType1 + xlab("Follow-up time (months)") + ylab("Type 1 error")
ggType1

## * export
ggsave(ggBenefit1, filename = "Results/ggBenefit-ChemoVSChemo-scenario0.pdf")
ggsave(ggBenefit1.bis, filename = "Results/ggBenefit-ChemoVSChemo-scenario123.pdf")

ggsave(ggPower1, filename = "Results/ggPower-ChemoVSChemo-scenario0.pdf")
ggsave(ggPower1.bis, filename = "Results/ggPower-ChemoVSChemo-scenario123.pdf")

ggsave(ggBenefit2, filename = "Results/ggBenefit-ChemoVSImmuno-scenario0.pdf")
ggsave(ggBenefit2.bis, filename = "Results/ggBenefit-ChemoVSImmuno-scenario123.pdf")

ggsave(ggPower2, filename = "Results/ggPower-ChemoVSImmuno-scenario0.pdf")
ggsave(ggPower2.bis, filename = "Results/ggPower-ChemoVSImmuno-scenario123.pdf")

ggsave(ggBenefit3, filename = "Results/ggBenefit-ImmunoVSImmuno-scenario0.pdf")
ggsave(ggBenefit3.bis, filename = "Results/ggBenefit-ImmunoVSImmuno-scenario123.pdf")

ggsave(ggPower3, filename = "Results/ggPower-ImmunoVSImmuno-scenario0.pdf")
ggsave(ggPower3.bis, filename = "Results/ggPower-ImmunoVSImmuno-scenario123.pdf")


##----------------------------------------------------------------------
### FIGURE.R ends here
