########################
# Creates paramlist file with all paramater sweep commands
########################
rm(list=ls())
if(grepl('spencerfox', Sys.info()['login'])) setwd('~/projects/network/transVax/')
if(grepl('vagrant', Sys.info()['user'])) setwd('/vagrant/network/transVax/')
if(grepl('sjf826', Sys.info()['login'])) setwd('/home1/02958/sjf826/transVax/')
if(grepl('tacc', Sys.info()['nodename'])) setwd('/home1/02958/sjf826/transVax/')

options(scipen=999)

####################################
## Specify the networks ("er", "exp", or "unif" currently)
####################################
network <- "exp"

####################################
## Specify gammas and initially vaccinated
####################################
gamma_vax <- 1 / 5
gamma_disease <- 1 / 5
init_vax <- 5000
init_inf <- 1

####################################
## vaccination type
####################################
vax_type = "trans"

####################################
## Getting the betas for use -- todo: alter once different network types are specified
####################################
source("R/network_fxns.R")
mean_deg <- 10
num_nodes <- 10000
exp_dist <- get_exp_deg_dist(mean_deg = mean_deg, n = num_nodes)
beta_vax <- getBetaVec(r_not = seq(0.05, 1, by=0.05), deg_dist = exp_dist, gamma = gamma_vax)
beta_disease <- getBeta(r_not = 3.23, deg_dist = exp_dist, gamma = gamma_disease)

####################################
## Num reps
####################################
num_reps <- 5000

## Change to where data should be saved
dataLoc <- "$WORK/data/transVax/"

sink("launcher/sim_jobs_script.txt")
for(beta_v in beta_vax){
    start_cmd <- "./../../EpiFire/research/trans_vaccine/vax_sim "
    param_cmd <- paste(beta_v, beta_disease, gamma_vax, gamma_disease, init_vax, init_inf, network, vax_type, num_reps)
    end_cmd <- paste0(" > ", dataLoc, paste(beta_v, beta_disease, gamma_vax, gamma_disease, init_vax, init_inf, network, vax_type, sep="_"), ".txt")
    full_cmd <- paste0(start_cmd, param_cmd, end_cmd)
    #print(full_cmd)
    cat(full_cmd)               # add command
    cat('\n')              # add new line
}
sink()
