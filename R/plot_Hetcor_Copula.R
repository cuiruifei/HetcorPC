###################################################################################
# Goal: Test the performance of Hetcor PC and Copula PC in estimating the skeleton.
#
# Metrics: TPR, FPR
#
# Setting: p = {15, 45, 90}, n = {100, 500, 1000, 2000}, E[N] = {2, 5}
#
###################################################################################

#### 0. Load Packages ####

##
library(ggplot2)
library(gridExtra)
library(dplyr)
library(psych)
library(xtable)

## help function
data2stat <- function(data){
  
  describe(data) %>%
    mutate(ci = qt(0.975,n-1) * se) %>%
    mutate(Methods = c(rep('HetSS',4), rep('HetLESS',4), rep('Copula',4))) %>%
    mutate(Sample.size = as.factor(rep(c(100, 500,1000,2000),3))) %>%
    select(Methods, Sample.size, mean, ci)
}


#### 1. Read Data ####

## for two neighbors
result.p15.N2 <- read.table('results/results_p15_N2_het2_cop')
result.p45.N2 <- read.table('results/results_p45_N2_het2_cop')
result.p90.N2 <- read.table('results/results_p90_N2_het2_cop')
## for five neighbors
result.p15.N5 <- read.table('results/results_p15_N5_het2_cop')
result.p45.N5 <- read.table('results/results_p45_N5_het2_cop')
result.p90.N5 <- read.table('results/results_p90_N5_het2_cop')


#### 2. Plot ####

pd <- position_dodge(0.3) # move them to the left and right
par(mfrow = c(2,2))
par(mar=c(3.5,4,2,1.5))

#### Figure 1: TPR and FPR for N2

## plot 1: TPR, p15
# statistical values
stat = data2stat(result.p15.N2[,1:12])
# plot
TPR.p15 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + coord_cartesian(ylim = c(0, 1)) +
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('TPR') + ggtitle('TPR (p=15)') + 
  theme_bw() +
  theme(legend.justification=c(0.99,0.01),legend.position=c(0.99,0.01),legend.title = element_blank(), legend.key.size = unit(0.35, "cm")) +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 2: TPR, p45
# statistical values
stat = data2stat(result.p45.N2[,1:12])
# plot
TPR.p45 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + coord_cartesian(ylim = c(0, 1)) +
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('TPR') + ggtitle('TPR (p=45)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 3: TPR, p90
# statistical values
stat = data2stat(result.p90.N2[,1:12])
# plot
TPR.p90 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + coord_cartesian(ylim = c(0, 1)) +
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('TPR') + ggtitle('TPR (p=90)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 4: FPR, p15
# statistical values
stat = data2stat(result.p15.N2[,13:24])
# plot
FPR.p15 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + 
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('FPR') + ggtitle('FPR (p=15)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 5: FPR, p45
# statistical values
stat = data2stat(result.p45.N2[,13:24])
# plot
FPR.p45 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + 
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('FPR') + ggtitle('FPR (p=45)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 6: FPR, p90
# statistical values
stat = data2stat(result.p90.N2[,13:24])
# plot
FPR.p90 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + 
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('FPR') + ggtitle('FPR (p=90)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))


### Show and Save the Figure
grid.arrange(TPR.p15, TPR.p45, TPR.p90, FPR.p15, FPR.p45, FPR.p90, nrow = 2, ncol = 3)

# pdf(file = 'results/TFPR_N2.pdf', width = 6.7, height = 3.65)
# ## arrange
# grid.arrange(TPR.p15, TPR.p45, TPR.p90, FPR.p15, FPR.p45, FPR.p90, nrow = 2, ncol = 3)
# 
# dev.off()

#### Figure 2: TPR and FPR for N5

## plot 1: TPR, p15
# statistical values
stat = data2stat(result.p15.N5[,1:12])
# plot
TPR.p15 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + coord_cartesian(ylim = c(0, 0.8)) +
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('TPR') + ggtitle('TPR (p=15)') + 
  theme_bw() +
  theme(legend.justification=c(0.99,0.01),legend.position=c(0.99,0.01),legend.title = element_blank(), legend.key.size = unit(0.35, "cm")) +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 2: TPR, p45
# statistical values
stat = data2stat(result.p45.N5[,1:12])
# plot
TPR.p45 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + coord_cartesian(ylim = c(0, 0.8)) +
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('TPR') + ggtitle('TPR (p=45)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 3: TPR, p90
# statistical values
stat = data2stat(result.p90.N5[,1:12])
# plot
TPR.p90 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + coord_cartesian(ylim = c(0, 0.8)) +
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('TPR') + ggtitle('TPR (p=90)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 4: FPR, p15
# statistical values
stat = data2stat(result.p15.N5[,13:24])
# plot
FPR.p15 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + 
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('FPR') + ggtitle('FPR (p=15)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 5: FPR, p45
# statistical values
stat = data2stat(result.p45.N5[,13:24])
# plot
FPR.p45 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + 
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('FPR') + ggtitle('FPR (p=45)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))

## plot 6: FPR, p90
# statistical values
stat = data2stat(result.p90.N5[,13:24])
# plot
FPR.p90 = ggplot(stat, aes(x = Sample.size, y = mean, group = Methods, color = Methods)) + 
  geom_line(position=pd) + 
  geom_point(aes(shape=Methods), position=pd) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=pd) + xlab('sample size (n)') + ylab('FPR') + ggtitle('FPR (p=90)') + 
  theme_bw() +
  theme(legend.position='none') +
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5,  size = 8))


### Show and Save the Figure
grid.arrange(TPR.p15, TPR.p45, TPR.p90, FPR.p15, FPR.p45, FPR.p90, nrow = 2, ncol = 3)

# pdf(file = 'results/TFPR_N5.pdf', width = 6.7, height = 3.65)
# ## arrange
# grid.arrange(TPR.p15, TPR.p45, TPR.p90, FPR.p15, FPR.p45, FPR.p90, nrow = 2, ncol = 3)
# 
# dev.off()
