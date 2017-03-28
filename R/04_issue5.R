# -----------------------
# issue #5: How does behavior change from game 1 to game 5?
# -----------------------

# Do variables such as points earned, proportion of reaching the goal, switch rates
# (etc.) change consistently across games from game 1 to game 5? 

library(yarrr)

df.games <- readRDS("data/dataGameLevel.rds")

# create separate variables for Environment and Goal
df.games$Variance <- ifelse(df.games$condition %in% c(1,3), "low", "high")
df.games$Goal <- ifelse(df.games$condition %in% c(1,2), "no", "yes")


# do points earned differ over games?
pirateplot(outcome.sum ~ game + Goal + Variance, data = df.games, ylab = "total points earned")

# do mean switch rates differ over games?
pirateplot(mean.switched ~ game + Goal + Variance, data = df.games, ylab = "mean switch rates")

# do median response times differ over games?
pirateplot(resp.time.median ~ game + Goal + Variance, data = df.games, ylab = "median response times")

# do mean high EV chosen differ over games?
pirateplot(highEV.mean ~ game + Goal + Variance, data = df.games, ylab = "mean high EV chosen")

# do mean mean high variance chosen differ over games?
pirateplot(high.var.chosen.mean ~ game + Goal + Variance, data = df.games, ylab = "mean high variance chosen")

# differences in goal reached rates?
aggregate(goalReached ~ game + Variance, FUN = mean, data = df.games)



