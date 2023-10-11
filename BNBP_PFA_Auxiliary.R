# DPFM Auxiliary



# Required Packages:

library("data.table")
library("magrittr")
library("dplyr")
library("parallel") # , lib.loc="C:/Program Files/R/R-3.2.2/library"
library("stringr")
library("gtools")
library("abind")
library("rlist")
#library("snow")
#library("BayesLogit")
#library("shiny")
#library("RWeka")
#library("quanteda")
#library("doParallel")
#library("foreach")
#library("doSNOW")



# Helpful Functions:

# Installs packages (if necessary) and loads them:
# found this function online:
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos="http://cran.rstudio.com/")
  require(p, character.only = TRUE)
}

# Returns post indices for each vocab word, 
# given vocabulary and word counts x_vn:
getWordPostIndices <- function(Vn, Vocabulary){
  wordPostIndices = lapply(Vocabulary, function(v) return(which(unlist(lapply(Vn, function(n) return(v%in%n))))))
  names(wordPostIndices) = Vocabulary
  return(wordPostIndices)
}

# Calculates document word topic probabilities zeta_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document words v_n:
calcZETAvnk <- function(PHIvk, THETAkn, Vn){
  return(lapply(1:length(Vn), function(n){ ZETAvk = PHIvk[Vn[[n]],]%*%diag(THETAkn[,n]); return(apply(ZETAvk, 1, function(v) return(v/sum(v))))}))
} 

# Calculates document word topic probabilities zeta_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document words v_n:
calcZETAvnk2 <- function(PHIvk, THETAkn, Vn){
  tPHIvk=t(PHIvk)
  return(lapply(1:length(Vn), function(n){ ZETAkv = tPHIvk[,Vn[[n]]]*THETAkn[,n]; return(t(ZETAkv)/colSums(ZETAkv))}))
} 

# Calculates document word topic probabilities zeta_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document word counts x_vn:
calcZETAvnk3 <- function(PHIvk, THETAkn, Vn){
  LAMBDA_vn = PHIvk%*%THETAkn
  return(lapply(1:length(Vn), function(n){ return(diag(1/LAMBDA_vn[Vn[[n]],n])%*%PHIvk[Vn[[n]],]%*%diag(THETAkn[,n]))}))
} 


# Helper for sampling document word topic counts x_vnk:
MultXvn <- function(wordCount, topicProbs){
  numTopics = length(topicProbs)
  wordTopicCounts = rep(0, numTopics)
  #names(wordTopicCounts) = as.character(1:numTopics)
  wordTopicSamp = table(sample(size=as.numeric(wordCount), x=1:numTopics, prob=topicProbs, replace=TRUE))
  wordTopicCounts[as.numeric(names(wordTopicSamp))]=wordTopicSamp
  return(wordTopicCounts)
}

# Samples document word topic counts x_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document word counts x_vn:
sampXvnk <- function(Xvn, ZETAvnk, Vn){
  Xvnk = lapply(1:length(Xvn), function(n){wordTopicCounts = lapply(1:length(Xvn[[n]]), function(v){ return(MultXvn(Xvn[[n]][v], ZETAvnk[[n]][,v]))}); names(wordTopicCounts)=Vn[[n]]; return(wordTopicCounts)})
}

# Samples document word topic counts x_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document word counts x_vn:
sampXvnk2 <- function(x_vn, v_n, zeta_vnk, N, K){
  lapply(1:N, function(n) { wordTopics = unlist(lapply(v_n[[n]], function(v) return(sample(size=x_vn[[n]][[v]], x=1:K, prob=zeta_vnk[[n]][,v], replace=TRUE)))); names(wordTopics)=rep(v_n[[n]], x_vn[[n]]); return(wordTopics)})
}

# Samples document word topic counts x_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document word counts x_vn:
sampXvnk3 <- function(x_vn, v_n, zeta_vnk, N, K){
  lapply(1:N, function(n) { wordTopics = unlist(lapply(v_n[[n]], function(v) return(sample(size=x_vn[[n]][[v]], x=1:K, prob=zeta_vnk[[n]][v,], replace=TRUE)))); names(wordTopics)=rep(v_n[[n]], x_vn[[n]]); return(wordTopics)})
}

# Samples document word topic counts x_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document word counts x_vn:
sampXvnk4 <- function(x_vn, v_n, zeta_vnk, N, K, x_vnames){
  lapply(1:N, function(n) { wordTopics = unlist(lapply(v_n[[n]], function(v) return(sample(size=x_vn[[n]][[v]], x=1:K, prob=zeta_vnk[[n]][v,], replace=TRUE)))); names(wordTopics)=x_vnames[[n]]; return(wordTopics)})
}

# Calculates document word topic probabilities zeta_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document words v_n:
# Samples document word topic counts x_vnk given topic word weights
# phi_vk, document factor scores theta_kn, and document word counts x_vn:
calcZETAvnkSampXvnk <- function(x_vn, phi_vk, theta_kn, v_n, V_n, N, K, x_vnames, Cores){
  tPHIvk=t(phi_vk)
  if (.Platform$OS.type == "windows"){
    return(lapply(1:N, function(n){
      ZETAkv = as.matrix(tPHIvk[,v_n[[n]]])*theta_kn[,n];
      ZETAvk = t(ZETAkv)/colSums(ZETAkv);
      wordTopics = unlist(lapply(1:V_n[n], function(v){
        return(sample(size=x_vn[[n]][[v]], x=1:K, prob=ZETAvk[v,], replace=TRUE))
      }));
      names(wordTopics)=x_vnames[[n]]; 
      return(wordTopics)
    }))  
  } else {
    return(mclapply(1:N, function(n){
      ZETAkv = as.matrix(tPHIvk[,v_n[[n]]])*theta_kn[,n];
      ZETAvk = t(ZETAkv)/colSums(ZETAkv);
      wordTopics = unlist(lapply(1:V_n[n], function(v){
        return(sample(size=x_vn[[n]][[v]], x=1:K, prob=ZETAvk[v,], replace=TRUE))
      }));
      names(wordTopics)=x_vnames[[n]]; 
      return(wordTopics)
    }, mc.cores=Cores))
  }
}

# Helper function which takes a word and document word topic counts Xvnk
# and returns word topic counts:
getTopicCounts <- function(vocabWord, Xvnk, K){
  colSums(matrix(unlist(lapply(Xvnk, function(n){ postWord = which(names(n)==vocabWord); {return(n[postWord])}})), ncol=K, byrow=TRUE))
}

# Calculates marginal word topic counts x_vk given document 
# word topic counts x_vnk, a vocabulary, number of topics K, 
# and word post indices:
calcXvk <- function(Xvnk, Vocabulary, K, PostIndices_v){
  wordTopicCounts = lapply(Vocabulary, function(v) return(getTopicCounts(v, Xvnk[PostIndices_v[[v]]], K)))
  names(wordTopicCounts) = Vocabulary
  Xvk = do.call(rbind, wordTopicCounts)
}

# Calculates marginal word topic counts x_vk given document 
# word topic counts x_vnk, a vocabulary, number of topics K, 
# and word post indices:
calcXvk2 <- function(Xvnk, Vocabulary, K, V){
  wordCounts = rep(0, V)
  names(wordCounts) = Vocabulary
  Xvk = unlist(Xvnk)
  namesXvk = names(Xvk)
  do.call(cbind,lapply(1:K, function(k) {topicWordCounts = wordCounts; nonZeroCounts = table(namesXvk[which(Xvk==k)]); if(sum(nonZeroCounts)==0){return(topicWordCounts)} else {topicWordCounts[names(nonZeroCounts)]=nonZeroCounts; return(topicWordCounts)}}))
}

# Calculates marginal post topic counts x_nk given document 
# word topic counts x_vnk:
calcXnk <- function(Xvnk){
  do.call(rbind, lapply(Xvnk, function(n) return(colSums(do.call(rbind, n)))))
}

# Calculates marginal post topic counts x_nk given document 
# word topic counts x_vnk:
calcXnk2 <- function(Xvnk, N, K, topicNames, Cores){
  topicCounts = rep(0, K)
  names(topicCounts) = topicNames
  if (.Platform$OS.type != "windows"){
    do.call(rbind, mclapply(1:N, function(n) {postTopicCounts = topicCounts; nonZeroCounts = table(Xvnk[[n]]); if(sum(nonZeroCounts)==0){return(postTopicCounts)} else {postTopicCounts[names(nonZeroCounts)]=nonZeroCounts; return(postTopicCounts)}}, mc.cores=Cores))
  } else {
    do.call(rbind, lapply(1:N, function(n) {postTopicCounts = topicCounts; nonZeroCounts = table(Xvnk[[n]]); if(sum(nonZeroCounts)==0){return(postTopicCounts)} else {postTopicCounts[names(nonZeroCounts)]=nonZeroCounts; return(postTopicCounts)}}))
  }
}

# Samples topic word weights phi_vk given prior concentration parameter a_phi
# and marginal topic word counts X_vk:
sampPHIvk <- function(Xvk, Aphi, Vocabulary, V, K){
  PHIvk = matrix(do.call(cbind, lapply(1:K, function(k){ return(rdirichlet(1, Aphi+Xvk[,k]))})), dimnames=list(Vocabulary, as.character(1:K)), nrow=V)
  return(PHIvk)
}

# Sample once from a CRT distribution:
crt <- function(m, r){
  if(m==0){
    return(0)
  }
  if(r==0){
    return(0)
  }
  nseq = seq(1, m, 1)
  b_n = rbinom(n=m, size=1, prob=r/(nseq-1+r))
  return(sum(b_n))
}

# Vector of single samples from a crt function which takes a vector of m values
# and a vector of r values:
crt_vec <- Vectorize(crt, SIMPLIFY=TRUE)

# Samples a crt(a,b) where a is over each element of a matrix of document topic
# counts x_nk, and b is over a vector of topic rates r_k, which is repeated for each n.
sampLnk <- function(Xnk, Rk, N, Cores){
  if (.Platform$OS.type != "windows"){
    do.call(cbind, mclapply(1:N, function(n) return(crt_vec(Xnk[n,], Rk)), mc.cores=Cores))
  } else {
    do.call(cbind, lapply(1:N, function(n) return(crt_vec(Xnk[n,], Rk))))
  }
}

# Samples factor scores theta_kn given NB stopping parameter r_k, latent doc topic counts x_nk, and 
# NB probability parameter p_k.
sampTHETAkn <- function(Rk, Xnk, Pk, N, K, Cores){
  if (.Platform$OS.type != "windows"){
    do.call(cbind, mclapply(1:N, function(n) return(rgamma(K, shape=Rk+Xnk[n,], scale=Pk)), mc.cores=Cores))
  } else {
    do.call(cbind, lapply(1:N, function(n) return(rgamma(K, shape=Rk+Xnk[n,], scale=Pk))))
  }
}

# takes doc word counts x_vn, documents (string vector), and a proportion of words from each
# document to use as training data, and returns training doc word counts (list of nonzero word 
# counts for each document):
getTrainingData <- function(Xvn, Documents, Proportion){
  nonZeroTrainingCounts = lapply(Documents, function(n){wordVec=unlist(str_split(n, " ")); return(table(sample(wordVec, size=ceiling(length(wordVec)*Proportion))))})
}

# takes doc word counts x_vn, and training doc word counts t_vn, and returns 
# testing doc word counts y_vn (again, just non zero counts):
getTestingData <- function(Xvn, Tvn){
  allTestingCounts = lapply(1:length(Xvn), function(n){testingCounts=Xvn[[n]]; testingCounts[names(Tvn[[n]])]=testingCounts[names(Tvn[[n]])]-Tvn[[n]]; return(testingCounts)})
  nonZeroTestingCounts = lapply(allTestingCounts, function(n) return(n[which(n>0)]))
}

# Does gibbs sampling from BNBP PFA given data, specifications, and hyperparameters:
Gibbs_Sample_BNBP_PFA <- function(x_vn, K, S, burn, thin, cores, c, epsilon, e_0, f_0, a_phi){
  
  # Initialize start time:
  startTime = proc.time()
  
  # Obtain indices of Gibbs samples to keep (given S, burn, and thin):  
  samplesToKeep = burn+which(((burn+1):S)%%thin==0) # Indices of Gibbs samples to keep
  numberKeptSamples = length(samplesToKeep) # Number of Gibbs samples kept (after burn and thin)
  
  # Set up Data:
  v_n = lapply(x_vn, names) # unique words of each document
  V_n = unlist(lapply(v_n, length)) # number of unique words of each document
  x_vnames = lapply(x_vn, function(n) return(rep(names(n), n))) # saves time in the multinomial sampling step
  vocabulary = unique(unlist(v_n)) # vocabulary of terms 
  #postIndices_v = getWordPostIndices(v_n,vocabulary) # word specific document indices
  N = length(x_vn) # Number of observations
  V = length(vocabulary) # Size of vocabulary
  
  # Initialize output (arrays to store posterior samples): 
  p_ks = matrix(0, nrow = K, ncol = numberKeptSamples, dimnames=list(as.character(1:K), as.character(1:numberKeptSamples)))
  r_ks = matrix(0, nrow = K, ncol = numberKeptSamples, dimnames=list(as.character(1:K), as.character(1:numberKeptSamples)))
  theta_kns = array(0, dim=c(K, N, numberKeptSamples), dimnames=list(as.character(1:K), as.character(1:N), as.character(1:numberKeptSamples)))
  phi_vks = array(0, dim=c(V, K, numberKeptSamples), dimnames=list(vocabulary, as.character(1:K), as.character(1:numberKeptSamples)))
  
  # Initialize sufficient statistics:
  x_vk = matrix(0, nrow=V, ncol=K, dimnames=list(vocabulary, as.character(1:K)))
  x_nk = matrix(0, nrow=N, ncol=K, dimnames=list(as.character(1:N), as.character(1:K)))
  
  # Initialize parameters from their priors: 
  p_k = rbeta(K, c*epsilon, c*(1-epsilon))
  r_k = rgamma(K, shape=e_0, scale=1/f_0)
  theta_kn = matrix(rgamma(N*K, shape=r_k, scale=p_k/(1-p_k)), nrow=K, dimnames=list(as.character(1:K), as.character(1:N)))
  phi_vk = matrix(rdirichlet(K, rep(a_phi, V)), ncol=K, byrow=TRUE, dimnames=list(vocabulary, as.character(1:K)))
  
  # Gibbs sample:
  s=0
  while(s<S){
    s=s+1
    
    # do one iteration of gibbs sampling from each model parameter:
    #x_vnk = sampXvnk(x_vn,zeta_vnk,v_n)
    #x_vk = calcXvk(x_vnk,vocabulary,K,postIndices_v)
    #x_nk = calcXnk(x_vnk)
    #zeta_vnk = calcZETAvnk(phi_vk,theta_kn,v_n) # 12 sec
    #x_vnk = sampXvnk2(x_vn, v_n, zeta_vnk, N, K)
    #zeta_vnk = calcZETAvnk2(phi_vk,theta_kn,v_n)
    #x_vnk = sampXvnk3(x_vn, v_n, zeta_vnk, N, K)
    #x_vnk = sampXvnk4(x_vn, v_n, zeta_vnk, N, K, x_vnames)
    #l_nk = do.call(cbind,lapply(1:N, function(n) return(unlist(lapply(1:K, function(k) return(crt(x_nk[n,k], r_k[k])))))))
    x_vnk = calcZETAvnkSampXvnk(x_vn, phi_vk, theta_kn, v_n, V_n, N, K, x_vnames, cores)
    x_vk = calcXvk2(x_vnk, vocabulary, K, V)
    x_nk = calcXnk2(x_vnk, N, K, as.character(1:K), cores)
    p_k = rbeta(K, c/K+colSums(x_nk), c*(1-1/K)+N*r_k)
    l_nk = sampLnk(x_nk, r_k, N, cores)
    r_k = rgamma(K, shape=e_0+colSums(l_nk), scale=1/(f_0-N*log(1-p_k)))
    theta_kn = sampTHETAkn(r_k, x_nk, p_k, N, K, cores)
    phi_vk = sampPHIvk(x_vk, a_phi, vocabulary, V, K)
    
    # collect samples
    # if s > burn and s is in samplesToKeep, collect sample:
    if (s>burn){
      if (s%in%samplesToKeep){
        sInd = which(samplesToKeep == s)
        p_ks[,sInd] = p_k
        r_ks[,sInd] = r_k
        theta_kns[,,sInd] = theta_kn 
        phi_vks[,,sInd] = phi_vk
      }
    }
    
    # print iteration number:
    print(paste("sample number", s, "of", S))
    
    # print end time - start time:
    duration = proc.time()-startTime
    print(paste("time ellapsed: ", duration[3], "seconds"))
    
  }
  
  return(list(p_ks = p_ks, r_ks = r_ks, theta_kns = theta_kns, phi_vks = phi_vks))
  
}


