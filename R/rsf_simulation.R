# --------------------------
# How does a risk sensitive foranging ("rsf") strategy compare to a ev maximization ("ev")
#  strategy?
# --------------------------

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# Load libraries
library(dplyr)
library(yarrr)
library(snowfall)
library(snow)

# Read learning functions
source("r/learning_functions.R")


# sim.dm is a design matrix of simulations. All combinations of the 
#  parameters will be simulated

sim.dm <- expand.grid(goal = c(90),                               # Goal
                      n.trials = c(25),                           # Trials in game
                      environment = 1:3,                          # Option environment
                      strategy = c("ev", "rsf"),                  # General strategy
                      selection.strat = c("egreedy"),  # Selection strategy
                      sim = 1:1000)                                # Simulations

# Each statistical environment is defined as a dataframe of means and standard deviations

environments <- list(data.frame(mean = c(3, 3),
                                sd = c(2, 8)),
                     data.frame(mean = c(3, 2),
                                sd = c(2, 8)),
                     data.frame(mean = c(2, 3),
                                sd = c(2, 8)))

# sim.dm.fun() runs the simulation for a given parameter combination and returns
#   aggregate statistics

sim.dm.fun <- function(x) {
  
  goal.i <- sim.dm$goal[x]
  n.trials.i <- sim.dm$n.trials[x]
  
  environment.i <- sim.dm$environment[x]
  
  option.mean.i <- environments[[environment.i]]$mean
  option.sd.i <- environments[[environment.i]]$sd
  
  strategy.i <- sim.dm$strategy[x]
  selection.strat.i <- sim.dm$selection.strat[x]
  
  sim.i <- rl.sim.fun(n.trials = n.trials.i,     # Trials in game
                      option.mean = option.mean.i,   # Mean of each option
                      option.sd = option.sd.i,   # SD of each option
                      prior.exp.start = rep(0, length(option.mean.i)), 
                      prior.sd.start = 1,
                      goal = goal.i,
                      epsilon = .2,                        # p(explore | selection.strat = "egreedy")
                      theta = 1, 
                      alpha = .2,
                      plot = FALSE, 
                      strategy = strategy.i, 
                      ylim = c(0, 100),
                      selection.strat = selection.strat.i)
  
  # Extract key statistics
  option.risky <- which(option.sd.i == max(option.sd.i))
  
  final.points.i <- sim.i$outcome.cum[n.trials.i]
  reach.goal.i <- final.points.i > goal.i
  risky.i <- mean(sim.i$selection == option.risky)
  
  # choosing risky option when above goal (ag) or under goal (ug)
  risky.ug.i <- mean(sim.i$selection[sim.i$outcome.cum < goal.i] == option.risky)
  risky.ag.i <- mean(sim.i$selection[sim.i$outcome.cum > goal.i] == option.risky)
  
  output <- data.frame("final.points" = final.points.i, 
                       "reach.goal" = reach.goal.i,
                       "risky" = risky.i,
                       "risky.ug" = risky.ug.i,
                       "risky.ag" = risky.ag.i)
   
  return(output)
  
}

# Run all simulations from sim.dm, then coerce into a dataframe
snowfall::sfInit(parallel = TRUE, cpus = 4)
snowfall::sfExportAll()
sim.result.ls <- snowfall::sfClusterApplySR(1:nrow(sim.dm), fun = sim.dm.fun, perUpdate = 1)
snowfall::sfStop()   # stop cluster

# Combine simulation results with sim.dm
sim.result.df <- do.call(rbind, sim.result.ls)
sim.dm <- cbind(sim.dm, sim.result.df)

# Calculate aggreagte statistics across simulations
sim.dm.agg <- sim.dm %>% group_by(goal, n.trials, environment, strategy) %>%
  summarise(
    reachgoal.p = mean(reach.goal, na.rm = TRUE),
    risky.mean= mean(risky, na.rm = TRUE),
    risky.ag.mean = mean(risky.ag, na.rm = TRUE),
    risky.ug.mean = mean(risky.ug, na.rm = TRUE),
    points.mean = mean(final.points, na.rm = TRUE)
  )


# Plotting
yarrr::pirateplot(reach.goal ~ strategy + environment, data = sim.dm)
yarrr::pirateplot(risky ~ strategy + environment, data = sim.dm)
#windows(height = 22, width = 33)
yarrr::pirateplot(risky.ag ~ strategy + environment, data = sim.dm)
yarrr::pirateplot(risky.ug ~ strategy + environment, data = sim.dm)

yarrr::pirateplot(final.points ~ strategy + environment, data = sim.dm)
mean(sim.dm$final.points >= 90)

