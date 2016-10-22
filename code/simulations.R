# simulations

detach("package:peRiodic", unload = TRUE)
library(devtools)
install_github("crtahlin/peRiodic", ref = "add_tmax")
library(peRiodic)
library(ROCR)
library(digest)
library(doParallel)
options(cores=3)
registerDoParallel()

##### SET UP THE SIMULATION PARAMETERS AND RESULTS PLACEHOLDER #################
B_variants <- c(1)
n_variants <- c(100)
n.new_variants <- c(100)
nk_variants <- c(5, 6)
knots_variants <- list(NULL)
par1sin_variants <- c(1)
par2sin_variants <- c(0.25)
par3sin_variants <- c(0.25)
par4sin_variants <- 1:4 # all interesting variants for par4sin (how many cycles in [0, 1])
par5sin_variants <- c(0, 0.25, 0.5, 0.75) # variants for par5sin (curve offset in multiples of 2*Pi)
tmax_variants <- c(1, 2, 365)
add_trend_variants <- c(TRUE, FALSE)
par1trend_variants <- c(1)
par2trend_variants <- c(0.1)
par3trend_variants <- c(0)
par4trend_variants <- c(1/10)
par5trend_variants <- c(0)
max_prob_value_variants <- c(0.95)
min_prob_variants <- c(0.05)
quantiles.cs_variants <- list(c(NULL)) # list(c(NULL), c(0.05, 0.25, 0.5, 0.75, 0.95)) 
# construct all interesting combinations of par4sin nad par5sin
results_index <- expand.grid(
  B=B_variants,
  n=n_variants,
  n.new=n.new_variants,
  nk=nk_variants,
  knots=knots_variants,
  par1sin=par1sin_variants,
  par2sin=par2sin_variants,
  par3sin=par3sin_variants,
  par4sin=par4sin_variants,
  par5sin=par5sin_variants,
  tmax=tmax_variants,
  add_trend=add_trend_variants,
  par1trend=par1trend_variants,
  par2trend=par2trend_variants,
  par3trend=par3trend_variants,
  par4trend=par4trend_variants,
  par5trend=par5trend_variants,
  max_prob_value=max_prob_value_variants,
  min_prob_value=min_prob_variants,
  quantiles.cs=quantiles.cs_variants,
  set_seed = 1234)
# calculate their hash value, to keep as index
strings <- apply(X = results_index, FUN = paste, MARGIN = 1, collapse = ",")
hashes <- unlist(lapply(X = strings, FUN = digest))
results_index[, "hash"] <- unlist(hashes)
results_list <- list() # placeholder for results
# folder to save results into
getwd()
results_folder <- "./results/test"
##### SIMULATE ! ###############################################################
system.time({ # time running of simulations
  # results_list <- 
  files <- list.files(path = results_folder)  
  
    foreach (line = 1:dim(results_index)[1],
                  .final = function(x) setNames(x, results_index[, "hash"]),
                  .inorder = TRUE,
                  .errorhandling = "pass") %dopar%  {
    
    hash <- results_index[line, "hash"]
    print(hash) # print out hash, for debugging sake
  
    # if file with has exists, skip this simulation
    if (any(grepl(pattern = hash, x = files))) {next}
    
    result <- f.sim.per.splines(B = results_index[line, "B"],
                                n = results_index[line, "n"], 
                                n.new = results_index[line, "n.new"],
                                nk = results_index[line, "nk"], 
                                knots = unlist(results_index[line, "knots"]),
                                par1sin = results_index[line, "par1sin"], 
                                par2sin = results_index[line, "par2sin"], 
                                par3sin = results_index[line, "par3sin"], 
                                par4sin = results_index[line, "par4sin"],  
                                par5sin = results_index[line, "par5sin"],
                                tmax = results_index[line, "tmax"], 
                                add_trend = results_index[line, "add_trend"],
                                par1trend = results_index[line, "par1trend"],
                                par2trend = results_index[line, "par2trend"],
                                par3trend = results_index[line, "par3trend"],
                                par4trend = results_index[line, "par4trend"],
                                par5trend = results_index[line, "par5trend"],
                                max_prob_value = results_index[line, "max_prob_value"],
                                min_prob_value = results_index[line, "min_prob_value"],
                                quantiles.cs = unlist(results_index[line, "quantiles.cs"]),
                                set_seed = results_index[line, "set_seed"]
                                ) 
    # result <- dummy
    # browser()
    # return(result)
    save(result, file = paste0(results_folder, "/", hash, ".Rdata"))
    }
  })
str(results_list)
##### SAVE RESULTS #############################################################
# save(results_list, file = "~/Sync/Projects/Sklop2_simulations/results/simulation_variant5_192sims/results_list.Rdata")
save(results_index, file = paste0(results_folder, "/", "results_index.Rdata"))
#### FUNCTION TO LOOK INTO SAVED RESULTS #######################################
# TODO: add saving estimated models for each simluation? so they can be plotted with real curve?
# TODO: save results into PDF?; make a table of results
look_at_simulation <- function(hash, results_list) {
  results <- results_list[[hash]] # extract results from a list
  
  # calculate average from data saved in my.res
  averages <- apply(X= results$my.res, FUN = function(x) {mean(x)}, MARGIN = 2)
  
  # make the averages into a data frame with named rows/columns
  attach(as.list(averages))
  averages_dt <- data.frame(
    rcs = c(brier.rcs.train, brier.rcs, AUCTrainEst.rcs, AUCNewEst.rcs, cal.rcs.1, cal.rcs.2, lrt.p.value.rcs.train, score.p.value.rcs.train),
    rcs.per = c(brier.rcs.per.train, brier.rcs.per, AUCTrainEst.rcs.per, AUCNewEst.rcs.per, cal.rcs.per.1, cal.rcs.per.2, lrt.p.value.rcs.per.train, score.p.value.rcs.per.train),
    cs.per = c(brier.cs.per.train, brier.cs.per, AUCTrainEst.cs.per, AUCNewEst.cs.per, cal.cs.per.1, cal.cs.per.2, lrt.p.value.cs.per.train, score.p.value.cs.per.train),
    row.names = c("Brier - train", "Brier - test", "AUC - train", "AUC - test", "Calibration intercept", "Calibration slope", "LRT P value", "Score P value")
  ) 
  
  # plot sine curve used as the originial probability function
  curve(
    sine_function(x, 
                  par1sin = results$par1sin,
                  par2sin = results$par2sin,
                  par3sin = results$par3sin,
                  par4sin = results$par4sin,
                  par5sin = results$par5sin,
                  par1trend = results$par1trend,
                  par2trend = results$par2trend,
                  par3trend = results$par3trend,
                  par4trend = results$par4trend,
                  par5trend = results$par5trend,
                  add_trend = results$add_trend,
                  max_prob_value = results$max_prob_value,
                  min_prob_value = results$min_prob_value),
    from = 0, to = results$tmax, main = "Probability function used in simulation")
  
  # print out the averages
  print(averages_dt)
  # print out used parameters of the sine function (TODO: add the rest)
  # print(paste("par4sin = ",results$par4sin,"; par5sin =", results$par5sin))
}

#### THE ACTUAL LOOKING INTO THE RESULTS #######################################
load("results_list.Rdata")
look_at_simulation(hash = "7fe65c226c7cd0faf9422dc2a4933714", results_list)
look_at_simulation(par4sin = 1, par5sin = 0, results_list = results_list) # "normal" variant of sine curve
look_at_simulation(par4sin = 1, par5sin = 0.25, results_list = results_list) # shifted curve - a cosine instead of sine
look_at_simulation(par4sin = 2, par5sin = 0, results_list = results_list) # sine curve, but with two cycles in period

