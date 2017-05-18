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

# check the distribution of the dependent variable that will be tested afterwards
hist(df.participant$high.var.chosen.rate)


### Section A2: Inference Statistics

#
# check whether participants chose the high variance option whe it was rational to do so (by rsf with OBJECTIVE
# distributions) 
#

# aggregate to participant level with choose.highvar as dichotomous variable
df.n <- aggregate(high.var.chosen ~ choose.highvar + id + goal.condition + variance.condition, FUN = mean,
                  data = df.trial)

# check for overall effect between high.var.chosen when it's rational vs. when it's not
# PREDICTION: Mean high variance chosen over all environments in the goal conditionsis greater when it is rational
#             (by rsf with objective distributions)
with(subset(df.n, goal.condition == "Goal"), t.test(high.var.chosen[choose.highvar==1],
                                                    high.var.chosen[choose.highvar==0], paired = T))

cohensD(high.var.chosen ~ choose.highvar, data = subset(df.n, goal.condition == "Goal"), method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen over all environments in the goal conditionsis greater when it is rational
#             (by rsf with objective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal"), wilcoxsign_test(high.var.chosen ~ choose.highvar)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal"))); eff.r

# for the Equal EVs condition
# PREDICTION: Mean high variance chosen in the Equal EV environment in the goal conditionsis greater when it is rational
#             than when it is not (by rsf with objective distributions)
with(subset(df.n, goal.condition == "Goal" & variance.condition == "Equal"), t.test(high.var.chosen[choose.highvar==1],
                                                    high.var.chosen[choose.highvar==0], paired = T))

cohensD(high.var.chosen ~ choose.highvar, data = subset(df.n, goal.condition == "Goal" & variance.condition == "Equal"),
        method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen in the Equal EV environment in the goal conditionsis greater when it is rational
#             than when it is not (by rsf with objective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal" & variance.condition == "Equal"),
               wilcoxsign_test(high.var.chosen ~ choose.highvar)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal" & variance.condition == "Equal")))
eff.r

# for the high EV is low variance condition
# PREDICTION: Mean high variance chosen in the high EV is Low variance environment in the goal conditionsis greater when it is rational
#             than when it is not (by rsf with objective distributions)
with(subset(df.n, goal.condition == "Goal" & variance.condition == "Low"),
     t.test(high.var.chosen[choose.highvar==1], high.var.chosen[choose.highvar==0], paired = T))

cohensD(high.var.chosen ~ choose.highvar, data = subset(df.n, goal.condition == "Goal" & variance.condition == "Low"),
        method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen in the high EV is Low variance environment in the goal conditionsis greater when it is rational
#             than when it is not (by rsf with objective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal" & variance.condition == "Low"),
               wilcoxsign_test(high.var.chosen ~ choose.highvar)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal" & variance.condition == "Low")))
eff.r

# and for the high EV is high variance condition
# PREDICTION: Mean high variance chosen in the high EV is High variance environment in the goal conditionsis greater when it is rational
#             than when it is not (by rsf with objective distributions)
with(subset(df.n, goal.condition == "Goal" & variance.condition == "High"),
     t.test(high.var.chosen[choose.highvar==1], high.var.chosen[choose.highvar==0], paired = T))

cohensD(high.var.chosen ~ choose.highvar, data = subset(df.n, goal.condition == "Goal" & variance.condition == "High"),
        method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen in the high EV is High variance environment in the goal conditionsis greater when it is rational
#             than when it is not (by rsf with objective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal" & variance.condition == "High"),
               wilcoxsign_test(high.var.chosen ~ choose.highvar)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal" & variance.condition == "High")))
eff.r

# plot the results
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
# PREDICTION 1: In the goal conditions, over all variance conditions,the proportion of high variance chosen, on average,
#               is higher when it is rational to do so, compared to when it is not.
#
# PREDICTION 2: This difference does not occur in the no goal condition, i.e. over all variance conditions there is no
#               difference in the proportion of high variance option chosen when it is rational to do so vs when it is
#               not.
pirateplot(high.var.chosen ~ choose.highvar + variance.condition + goal.condition, data = df.n,
           ylab = "prop high var chosen", xlab = "choose high var (rsf)", main = "")

#
# check whether participants chose the high variance option whe it was rational to do so (by rsf with SUBJECTIVE
# distributions) 
#

# aggregate to participant level with choose.highvar as dichotomous variable
df.n <- aggregate(high.var.chosen ~ choose.highvar.subj + id + goal.condition + variance.condition, FUN = mean,
                  data = df.trial)

# check for overall effect between high.var.chosen when it's rational vs. when it's not
# PREDICTION: Mean high variance chosen over all environments in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
with(subset(df.n, goal.condition == "Goal"), t.test(high.var.chosen[choose.highvar.subj==1],
                                                    high.var.chosen[choose.highvar.subj==0], paired = T))


cohensD(high.var.chosen ~ choose.highvar.subj, data = subset(df.n, goal.condition == "Goal"), method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen over all environments in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal"), wilcoxsign_test(high.var.chosen ~ choose.highvar.subj)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal"))); eff.r

# for the Equal EVs condition
# PREDICTION: Mean high variance chosen in the Equal environment in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
with(subset(df.n, goal.condition == "Goal" & variance.condition == "Equal"), t.test(high.var.chosen[choose.highvar.subj==1],
                                                                                    high.var.chosen[choose.highvar.subj==0],
                                                                                    paired = T))

cohensD(high.var.chosen ~ choose.highvar.subj, data = subset(df.n, goal.condition == "Goal" & variance.condition == "Equal"),
        method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen in the Equal EV environment in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal" & variance.condition == "Equal"),
               wilcoxsign_test(high.var.chosen ~ choose.highvar.subj)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal" & variance.condition == "Equal")))
eff.r

# for the high EV is low variance condition
# PREDICTION: Mean high variance chosen in the high EV is Low variance environment in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
with(subset(df.n, goal.condition == "Goal" & variance.condition == "Low"),
     t.test(high.var.chosen[choose.highvar.subj==1], high.var.chosen[choose.highvar.subj==0], paired = T))

cohensD(high.var.chosen ~ choose.highvar.subj, data = subset(df.n, goal.condition == "Goal" & variance.condition == "Low"),
        method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen in the high EV is Low variance environment in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal" & variance.condition == "Low"),
               wilcoxsign_test(high.var.chosen ~ choose.highvar.subj)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal" & variance.condition == "Low")))
eff.r

# and for the high EV is high variance condition

# PREDICTION: Mean high variance chosen in the high EV is High variance environment in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
with(subset(df.n, goal.condition == "Goal" & variance.condition == "High"),
     t.test(high.var.chosen[choose.highvar.subj==1], high.var.chosen[choose.highvar.subj==0], paired = T))

cohensD(high.var.chosen ~ choose.highvar.subj, data = subset(df.n, goal.condition == "Goal" & variance.condition == "High"),
        method = "paired")

# non parametric version:
# PREDICTION: Mean high variance chosen in the high EV is High variance environment in the goal conditionsis greater when it is rational
#             (by rsf with subjective distributions)
w.test <- with(subset(df.n, goal.condition == "Goal" & variance.condition == "High"),
               wilcoxsign_test(high.var.chosen ~ choose.highvar.subj)); w.test

eff.r <- w.test@statistic@teststatistic / sqrt(nrow(subset(df.n, goal.condition == "Goal" & variance.condition == "High")))
eff.r

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
pirateplot(high.var.chosen ~ choose.highvar.subj + variance.condition + goal.condition, data = df.n,
           ylab = "prop high var chosen", xlab = "choose high var subj (rsf)", main = "")

# plot the proportion of risky options chosen over and under the goal for the different conditions
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
# PREDICTION: The proportions of risky options chosen are LOWER for the goal conditions than for the 
#             no goal conditions.
pirateplot(risky.ag ~ goal.condition + variance.condition, data = df.participant,
           ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Above Goal")

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
# PREDICTION: The proportions of risky options chosen are HIGHER for the goal conditions than for the 
#             no goal conditions.
pirateplot(risky.ug ~ goal.condition + variance.condition, data = df.participant,
           ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Under Goal")

# plot the Number of goals reached
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 22, width = 33)
}
# PREDICTIONS: The number of goals reached is highest for the Equal, second for the high ev is High variance
#               and lowest for the high ev is Low variance environment.
pirateplot(nGoalsReached ~ variance.condition, data = df.participant,
           ylab = "n Goals Reached", xlab = "Conditions", main = "Number of Goals Reached")


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
eff.r <- w.test@statistic@teststatistic / sqrt(nrow(df.participant)); eff.r



# -----------------------------------------------------
# Section C: Game and Survey Data; Inference Statistics
# -----------------------------------------------------

# Here we have no specific prediction, but can use different variables for exploratory analyses.


