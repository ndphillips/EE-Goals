rm(list = ls())
gc()

# ------------
# DATA ANALYSIS
#
# Study (working title): Is behavior in a bandit task with a minimum aspiration level state
# dependent?
# 
# Authors:
#   - Markus Steiner (markus.steiner@unibas.ch)
#   - Nathaniel Phillips (nathaniel.phillips@unibas.ch)
# 
# This is the main analysis script for the confirmatory analysis part
#  
# 
# Code Sections:
#   0: Load Libraries and data
#   A: Game Data
#     A1: Descriptive Analyses
#     A2: Inference Statistics
#   B: Survey Data
#     B1: Descriptive Analyses
#     B2: Inference Statistics
#   C: Game and Survey Data; Inference Statistics
# ------------

# --------------------------
# Section 0: Load Libraries
# --------------------------

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(BayesFactor)) install.packages("BayesFactor"); library(BayesFactor)
if (!require(lsr)) install.packages("lsr"); library(lsr)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/SimulationData/useData/S1_dataTrialLevel.rds")
df.game <- readRDS("data/SimulationData/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/SimulationData/useData/S1_dataParticipantLevel.rds")


# ----------------------
# Section A: Game Data
# ----------------------

### Section A1: Descriptive Analyses

summary(df.trial)

# number of participants per condition
table(df.participant$goal.condition, df.participant$variance.condition)


### Section A2: Inference Statistics

#
## Trial level
# 

## Check the probability of selecting high-variance option given that one is BELOW 100 points
# PREDICTION: The probability of selecting the high-variance option given that one is below 100 points (the goal)
#             is HIGHER in the goal than in the no goal condition.

m.rug <- glmer(high.var.chosen ~ variance.condition + goal.condition + (1|game) + (1|id),
           data = subset(df.trial, overGoal == 0 & game > 1), family = binomial)
summary(m.rug)

# get the odds ration of choosing the high variance option when the variance condition is High compared to Equal
exp(m.rug@beta[2])
# get the odds ration of choosing the high variance option when the variance condition is Low compared to Equal
exp(m.rug@beta[3])
# get the odds ration of choosing the high variance option when the goal condition is NoGoal compared to Goal
exp(m.rug@beta[4])

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
par(mar=c(5,6.7,3,1.5))
pirateplot(risky.ug ~ goal.condition + variance.condition, data = df.participant,
           ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Under Goal")
# -----------------------

## Check the probability of selecting high-variance option given that one is ABOVE 100 points
# PREDICTION: The probability of selecting the high-variance option given that one is above 100 points (the goal)
#             is LOWER in the goal than in the no goal condition.

m.rag <- glmer(high.var.chosen ~ variance.condition + goal.condition + (1|game) + (1|id),
           data = subset(df.trial, overGoal == 1 & game > 1), family = binomial)
summary(m.rag)

# get the odds ration of choosing the high variance option when the variance condition is High compared to Equal
exp(m.rag@beta[2])
# get the odds ration of choosing the high variance option when the variance condition is Low compared to Equal
exp(m.rag@beta[3])
# get the odds ration of choosing the high variance option when the goal condition is NoGoal compared to Goal
exp(m.rag@beta[4])

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
par(mar=c(5,6.7,3,1.5))
pirateplot(risky.ag ~ goal.condition + variance.condition, data = df.participant,
           ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Above Goal")
# -----------------------

## With a logistic mixed effects model, check whether, in the goal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF.
# PREDICTION: In the goal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF.
m.chv <- glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id),
               data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)
# get the odds ration of choosing the high variance option when it is rational to do so
exp(m.chv@beta[2])

# plot this result on participant level

# first aggregate to participant level with choose.highvar as dichotomous variable
df.n <- aggregate(high.var.chosen ~ choose.highvar.subj + id + goal.condition + variance.condition, FUN = mean,
                  data = subset(df.tria, game > 1))

# plot the proportion of high variance chosen separated for the variance, the goal conditions and for whether it
# was, according to rsf, rational to choose the high variance option
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 46)
} else {
  quartz(height = 22, width = 46)
}
# PREDICTION 1: In the goal conditions, over all variance conditions,the proportion of high variance chosen, on average,
#               is higher when it is rational to do so, compared to when it is not.
#
# PREDICTION 2: This difference does not occur in the no goal condition, i.e. over all variance conditions there is no
#               difference in the proportion of high variance option chosen when it is rational to do so vs when it is
#               not.
par(mar=c(5,7,3,3))
pirateplot(high.var.chosen ~ choose.highvar.subj + variance.condition + goal.condition, data = df.n,
           ylab = "prop high var chosen", xlab = "choose high var subj (rsf)", main = "")
# -----------------------

## Check behavior when the RSF and high EV model make different choice predictions.
# PREDICTION: When the RSF and high EV model make different choice predictions, the choice prediction from the RSF
#             strategy will be more accurate than the EV strategy for the goal condition. However, in the no-goal
#             condition, the EV strategy will make better predictions.

# hwo often do the models make different predictions
with(subset(df.trial, game > 1), mean(pred.EV != pred.RSF, na.rm = TRUE))

m.pa <- glmer(pred.RSF.acc ~ goal.condition + (1|game) + (1|id),
               data = subset(df.trial, game > 1 & pred.EV != pred.RSF), family = binomial)
summary(m.pa)

# get the odds ration of RSF prediction accuracy when the condition is goal compared to no goal
exp(m.pa@beta[2])

df.p <- df.trial %>%
  filter(game > 1 & pred.EV != pred.RSF) %>%
  group_by(id, variance.condition, goal.condition) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc, na.rm = TRUE)
  )

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
par(mar=c(5,6.7,3,1.5))
pirateplot(pred.RSF.acc.rate ~ goal.condition + variance.condition, data = df.p,
           ylab = "RSF corr pred rate", xlab = "Condition", main = "Only Trials where RSF and EV differ")
# -----------------------


# 
## Participant level
#

## Check the proportion of high variance options chosen overall.
# PREDICTION: The proportion of high variance options chosen overall is higher in the goal compared to the no goal
#             conditions.
w.hvo <- wilcox_test(high.var.chosen.rate ~ as.factor(goal.condition), data = df.participant); w.hvo

# effect size r
eff.r.hvo <- as.numeric(w.hvo@statistic@teststatistic / sqrt(nrow(df.participant))); eff.r.hvo

# plot the result
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
par(mar=c(5,6.7,3,1.5))
pirateplot(high.var.chosen.rate ~ goal.condition, data = df.participant,
           ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Rate Overall")
# -----------------------

## Check the probability of reaching 100 points.
# PREDICTION: The probability of reaching 100 points is higher in the goal vs. the no goal condition.
w.rg <- wilcox_test(goalReachedRate ~ as.factor(goal.condition), data = df.participant); w.rg

# effect size r
eff.r.rg <- as.numeric(w.rg@statistic@teststatistic / sqrt(nrow(df.participant))); eff.r.rg

# plot the result
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
par(mar=c(5,6.7,3,1.5))
pirateplot(goalReachedRate ~ goal.condition, data = df.participant,
           ylab = "prop high var chosen", xlab = "Conditions", main = "Goal Reached Rate")
# -----------------------

## Check the absolute number of points earned.
# PREDICTION: Number of points earned are, on average, highest in the the "Equal" environment.
r.pc <- lm(points.cum ~ variance.condition + goal.condition + variance.condition * goal.condition, data = df.participant)
summary(r.pc)
anova(r.pc)

# plot the result
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
par(mar=c(5,6.7,3,1.5))
pirateplot(points.cum ~ variance.condition + goal.condition, data = df.participant,
           ylab = "Total Number of Points", xlab = "Conditions", main = "Total Points Reached")

# -----------------------


# ----------------------
# Section B: Survey Data
# ----------------------

### Section B1: Descriptive Analyses

# mean and range of age
mean(df.participant$age)
range(df.participant$age)

# distribution of sex
table(df.participant$sex)
round(prop.table(table(df.participant$sex)), 2)

# which strategy did most participants use
round(prop.table(table(df.participant$which.strategy)), 2)

# how does this look separated for the distributions
# PREDICTION: There are no systematic differences in the strategy used, neither for goal nor variance condition.
round(prop.table(table(df.participant$variance.condition, df.participant$which.strategy)), 2)
round(prop.table(table(df.participant$goal.condition, df.participant$which.strategy)), 2)

# which option did participant of the different conditions think to be the high EV condition
# PREDICTION: Participants in the Equal condition have highest values for 3; participants in the
#             High variance condition have the highest value for 1; participants in the Low variance
#             condition have the highest value for 2.
round(prop.table(table(df.participant$variance.condition, df.participant$whichHighEV)), 2)

# PREDICTION: There is no systematic difference between goal and no goal conditions regarding which option
#             they believe to have the highest EV.
round(prop.table(table(df.participant$goal.condition, df.participant$whichHighEV)), 2)


### Section B2: Inference Statistics

# did participants in the goal condition find it harder to earn points
# PREDICTION: Yes participants in the goal condition, on average, find it harder to earn points
w.test <- wilcox_test(game.difficulty ~ as.factor(goal.condition), data = df.participant); w.test
eff.r <- a.numeric(w.test@statistic@teststatistic / sqrt(nrow(df.participant))); eff.r



# -----------------------------------------------------
# Section C: Game and Survey Data; Inference Statistics
# -----------------------------------------------------

# Here we have no specific prediction, but can use different variables for exploratory analyses.


