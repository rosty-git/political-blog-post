# Rostyslav Popadyn
# 5/16/2016
# BNBP PFA



# This script implements Gibbs Sampling for the Marked Beta-Negative Binomial Process
# as described in Negative Binomial Process Count and Mixture Modeling. 
# Inputs: 
# Files:
# - "BNBP_PFA_Auxiliary.R": supplementary script with required packages and functions.
# - "textNetwork.csv": .csv data frame with columns date, domain, links, words and observations as rows.
# User inputs:
# - Specifications: T_max, K_max, S, burn, thin, cores, trainProp
# - Hyperparameters: c, epsilon, e_0, f_0, a_phi
# Outputs: 
# Gibbs samples of parameters: 
# - document factor scores theta_kn 
# - factor loadings phi_kv 
# - stopping parameter r_k for NB distribution on observation factor counts
# - success probability parameter p_k for NB distribution on observation factor counts 



# Set working directory:
#setwd("~/Documents/research/2016/")
setwd("C:/Users/Derek/Documents/2018/spring/text_in_social_networks_code/dynamic_topic_and_community_detection/")

# Set seed:
set.seed(12345)

# Source functions and packages:
source("BNBP_PFA_Auxiliary.R")



# Read in Text Network data:
textNetwork = fread("textNetwork.csv") %>% select(dates,domains,links,words) %>% as.data.frame() %>% tbl_df() #%>% sample_n(1000)



# Specifications:
T_max = 10 # Number of days
N = length(unlist(lapply(sort(unique(textNetwork$dates)), function(t) return(which(textNetwork$dates==t)))[1:T_max])) # dim(textNetwork)[1] # Number of observations
K_max = 200 # Number of topics
S = 250 # Total number of samples
burn = 150 # Number of samples to burn
thin = 4 # Thinning factor (collect 1 out of every thin samples)
cores = 1 # detectCores()-1 # Number of cores to use
trainProp = .8 # Proportion of words from each document to use as training data
minProb = .Machine$double.eps



# Initialize hyperparameters:
c = 1
epsilon = 1/K_max
e_0 = 2000
f_0 = 10
a_phi = .01



# Organize data:
indices_t = lapply(sort(unique(textNetwork$dates)), function(t) return(which(textNetwork$dates==t)))[1:T_max] # Indices of observations at each time t
documents = textNetwork$words[unlist(indices_t)]
x_vn = lapply(documents, function(n) return(table(unlist(str_split(n, " "))))) # document term counts
t_vn = getTrainingData(x_vn, documents, trainProp) # training document term counts
y_vn = getTestingData(x_vn, t_vn) # testing document term counts



# Gibbs sample BNBP PFA:
BNBP_PFA_results = Gibbs_Sample_BNBP_PFA(t_vn, K_max, S, burn, thin, cores, c, epsilon, e_0, f_0, a_phi)



# Save the objects in the global environment to a .Rdata file:
save.image("BNBP_PFA.Rdata")

