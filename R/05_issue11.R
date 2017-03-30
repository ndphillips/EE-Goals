
library(yarrr)
l.cols <- piratepal(palette = 'basel', trans = .6)
l.cols.m <- piratepal(palette = 'basel', trans = .1)

df.trial <- readRDS("data/dataTrialLevelPGetthere.rds")

plot(df.trial$points.cum, df.trial$switched)
abline(lm(df.trial$switched ~ df.trial$points.cum))





