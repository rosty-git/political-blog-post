# Visualization Auxiliary

# Required Packages:

library("RColorBrewer")
library("abind")
library("igraph")
library("data.table")
library("magrittr")
library("dplyr")
library("stringr")
library("ggplot2")
library("rlist")
library("label.switching")
library("parallel")
library("nnet")



# Helpful Functions:

# get top words for each topic given samples of phi_kv:
getTopWords_k <- function(PHIvk, numTopWords){
  return(apply(PHIvk, 2, function(k) return(names(sort(k, decreasing=TRUE)[1:numTopWords]))))
}

# Takes user specified vector of topic names (if multiple words, separate
# by spaces), and a matrix of topic word probabilities, and returns the 
# topic indices which assign highest posterior probability to each topic 
# name. For multiple word topic names the posterior probability is the sum. 
getTopicIndices <- function(topNames, PHIvk){
  unlist(lapply(str_split(topNames, " "), function(v){ 
    if (length(v)==1){
      topProbs = PHIvk[v,]
      maxTopProb = which.max(topProbs)
      return(maxTopProb)
    } else {
      topProbs = PHIvk[v,]
      sumTopProbs = colSums(topProbs)
      maxTopProb = which.max(sumTopProbs)
      return(maxTopProb)
    }}))
}


# Takes character vector of topic names (can be multi word separated by spaces),
# a V by K matrix of topic word probabilities, and a number of top words, and returns
# the highest posterior probability words from each of the topics which assigned each 
# topic label the highest posterior probability.
presentTopWords <- function(PHIvk, NumTopWords, TopicNames){
  if (missing(TopicNames)){
    topwords = getTopWords_k(PHIvk, NumTopWords)
  } else {
    topwords = getTopWords_k(PHIvk, NumTopWords)[,getTopicIndices(topicNames, PHIvk)]
    colnames(topwords) = TopicNames  
  }
  return(topwords)
}

# Takes as an input a K by N matrix of document topic intensities
# and returns the most dominant topic index, from among the topic indices
# specified in specTopics, or 0, if none of the topic intensities is 
# greater than minExpCount.
getTopicAssignments <- function(THETAkn, minExpCount, specTopics){
  apply(THETAkn, 2, function(n) if (any(n[specTopics]>minExpCount)){
    return(specTopics[which.max(n[specTopics])])
  } else {
    return(0)
  })
}

# get top words for each topic given samples of phi_kv:
getSpecificWords_k <- function(PHIvks, NumTopWords){
  numTopics = dim(PHIvks)[2]
  words_k = do.call(cbind, mclapply(1:numTopics, function(k) return(rowMeans(PHIvks[,k,]))))
  words_k = apply(words_k, 1, function(x) return(x/sum(x)))
  topWords_k = do.call(cbind, mclapply(1:numTopics, function(k) return(names(sort(words_k[k,], decreasing=TRUE)[1:NumTopWords]))))
  return(topWords_k)
}

# 
getSpecificWords <- function(words_vk, NumTopWords){
  words_kv = apply(words_vk, 1, function(x) return(x/sum(x)))
  topWords_k = do.call(cbind, mclapply(1:dim(words_kv)[1], function(k) return(names(sort(words_kv[k,], decreasing=TRUE)[1:NumTopWords]))))
  return(topWords_k)
}

# Takes character vector of topic names (can be multi word separated by spaces),
# a V by K matrix of topic word probabilities, and a number of top words, and returns
# the highest posterior topic probability given word words from each of the topics which assigned each 
# topic label the highest posterior topic probability given word.
presentSpecificWords <- function(TopicNames, PHIvk, NumTopWords){
  PHIkv = apply(PHIvk, 1, function(x) return(x/sum(x)))
  topwords = getSpecificWords(PHIvk, NumTopWords)[,getTopicIndices(TopicNames, t(PHIkv))]
  colnames(topwords) = TopicNames
  return(topwords)
}

getSpecificWords2_k <- function(PHIvks, THETAkns){
  numTopics = dim(PHIvks)[2]
  pTopic_k = 
    words_k = do.call(cbind, mclapply(1:numTopics, function(k) return(rowMeans(PHIvks[,k,]))))
  words_k = apply(words_k, 1, function(x) return(x/sum(x)))
  topWords_k = do.call(cbind, mclapply(1:numTopics, function(k) return(names(sort(words_k[k,], decreasing=TRUE)[1:50]))))
  return(topWords_k)
}

# orders the words of each topic by their posterior probability of being assigned to
# that topic (using bayes theorem to calculate p(topic|word)). takes as input the
# topic word probabilities phi_vk, document topic scores theta_kn, and word document
# counts N_v:
getTopicGivenWord <- function(PHIvk, THETAkn, Nv, numTopWords){
  numTopics = dim(PHIvk)[2]
  pTopic_k = rowSums(THETAkn)
  pTopic_k = pTopic_k/sum(pTopic_k)
  pWord_v = Nv/sum(Nv)
  #do.call(cbind, lapply(1:numTopics, function(k) return(names(sort((PHIvk[,k]*pTopic_k[k]/Nv[names(PHIvk[,k])])[which(Nv[names(PHIvk[,k])]>500)],decreasing=TRUE)[1:numTopWords]))))
  do.call(cbind, lapply(1:numTopics, function(k) return(names(sort((PHIvk[,k]*pTopic_k[k]/pWord_v[names(PHIvk[,k])]),decreasing=TRUE)[1:numTopWords]))))
}

# get term scores as described by blei and lafferty in topic models paper. this takes each 
# term's topic specific usage probabilities and multiplies them by the log of the ratio of the 
# term's topic specific usage probabilities to their geometric mean over the topics. words like 
# said have a larger geometric mean, and thus get downweighted.
getBleiScores <- function(PHIvk, numTopWords){
  NumTerms = dim(PHIvk)[1]
  NumTopics = dim(PHIvk)[2]
  termScores_vk = do.call(rbind, lapply(1:NumTerms, function(v) return(PHIvk[v,]*log(PHIvk[v,]/prod(PHIvk[v,]^(1/NumTopics))))))
  rownames(termScores_vk) = rownames(PHIvk)
  return(do.call(cbind, lapply(1:NumTopics, function(k) return(names(sort(termScores_vk[,k],decreasing=TRUE))[1:numTopWords]))))
}




# calculates per word perplexity given a testing matrix y_vn of word counts,
# posterior samples for the topic word probabilities phi_vks, and the 
# document topic scores theta_kns.
calcPerWordPerplexityOld <- function(Yvn, PHIvks, THETAkns){
  numSamps = dim(PHIvks)[3]
  numObs = length(Yvn)
  LAMBDAvns = lapply(1:numSamps, function(s) return(PHIvks[,,s]%*%THETAkns[,,s]))
  LAMBDAvn = Reduce('+', LAMBDAvns)
  LAMBDAn = colSums(LAMBDAvn)
  logNormLAMBDAvn = log(LAMBDAvn%*%diag(1/LAMBDAn))
  Y.. = sum(unlist(Yvn))
  Perplexity = exp(-(1/Y..)*sum(unlist(lapply(1:numObs, function(n) return(sum(Yvn[[n]]*logNormLAMBDAvn[names(Yvn[[n]]),n]))))))
  return(Perplexity)
}



# Function which takes the learned word topic prob samples phi_vks array and 
# appends a compatible array newTerms_v*ks with any terms observed in the test set
# which did not occur in the training set. Also adds MinProb to every element of PHIvks
# to avoid 0 probabilities in perplexity calculation.
addMissingTerms <- function(PHI_vks, Yvn, nTerms, nTops, nSamps, MinProb){
  terms = dimnames(PHI_vks)[[1]]
  testTerms = unique(unlist(lapply(Yvn, names)))
  testTermsToAdd = testTerms[which(!testTerms%in%terms)] 
  # add terms to PHI_vks if they occur in test set but not training set:
  if (length(testTermsToAdd)>0){
    notInTraining = array(0, dim=c(length(testTermsToAdd), nTops, nSamps), dimnames = list(testTermsToAdd, 1:nTops, 1:nSamps))
    PHI_vks = abind(PHI_vks, notInTraining, along=1)
    PHI_vks = PHI_vks + MinProb
    PHI_vks = array(apply(PHI_vks, 3, function(s) return(s%*%diag(1/colSums(s)))), dim=c(nTerms+length(testTermsToAdd), nTops, nSamps), dimnames = list(c(terms, testTermsToAdd), 1:nTops, 1:nSamps))
    return(PHI_vks)
  } else {
    PHI_vks = PHI_vks + MinProb
    PHI_vks = array(apply(PHI_vks, 3, function(s) return(s%*%diag(1/colSums(s)))), dim=c(nTerms+length(testTermsToAdd), nTops, nSamps), dimnames = list(c(terms, testTermsToAdd), 1:nTops, 1:nSamps))
    return(PHI_vks)
  }
}



# helper function which calculates the expected word probs over samples for an 
# observation n, given phi_vks, theta_ks, and y_v.
getFvn <- function(PHI_vks_new, THETAks, Yv){
  namesYv = names(Yv)
  LAMBDAv = rep(0, length(Yv))
  names(LAMBDAv) = namesYv
  LAMBDA = 0
  numSamps = dim(PHI_vks_new)[3]
  for (s in 1:numSamps){
    phiTheta_v =  colSums(t(PHI_vks_new[,,s])*THETAks[,s])
    LAMBDAv = LAMBDAv + phiTheta_v[namesYv]/numSamps
    LAMBDA = LAMBDA + sum(phiTheta_v)/numSamps
  }
  Fv = LAMBDAv / LAMBDA
  return(Fv)
}



# calculates per word perplexity given a testing matrix (as list) y_vn of word counts,
# posterior samples for the topic word probabilities phi_vks, and the 
# document topic scores theta_kns. Adds MinProb to each probability in PHIvks
# to avoid infinite perplexity calculation.
calcPerWordPerplexity <- function(Yvn, Result, MinProb, Cores, SubsetSize){ 
  subsetIndices = sample(1:length(Yvn), SubsetSize, replace=FALSE)
  Yvn = Yvn[subsetIndices]
  PHIvks = Result$phi_vks
  THETAkns = Result$theta_kns[,subsetIndices,]
  numTerms = dim(PHIvks)[1]
  numTops = dim(PHIvks)[2]
  numSamps = dim(PHIvks)[3]
  numObs = length(Yvn)
  PHIvks_new = addMissingTerms(PHIvks, Yvn, numTerms, numTops, numSamps, MinProb)
  # THIS IS TOO BIG (3.9 gb for only 5000 posts)
  # only store fvn for yvn > 0. 
  #LAMBDAvns = lapply(1:numSamps, function(s) return(PHIvks[,,s]%*%THETAkns[,1:1000,s])) 
  #LAMBDAvn = Reduce('+', LAMBDAvns) 
  #LAMBDAn = colSums(LAMBDAvn) 
  #logNormLAMBDAvn = log(LAMBDAvn%*%diag(1/LAMBDAn)) 
  # ADD CODE TO RUN WITH LAPPLY FOR WINDOWS
  if (.Platform$OS.type != "windows"){
    Fvn = mclapply(1:numObs, function(n) return(getFvn(PHIvks_new, THETAkns[,n,], Yvn[[n]])), mc.cores=Cores)
    Y.. = sum(unlist(Yvn)) 
    Perplexity = exp(-(1/Y..)*sum(unlist(mclapply(1:numObs, function(n) return(sum(Yvn[[n]]*log(Fvn[[n]]))), mc.cores=Cores))))
  } else {
    Fvn = lapply(1:numObs, function(n) return(getFvn(PHIvks_new, THETAkns[,n,], Yvn[[n]])))
    Y.. = sum(unlist(Yvn)) 
    Perplexity = exp(-(1/Y..)*sum(unlist(lapply(1:numObs, function(n) return(sum(Yvn[[n]]*log(Fvn[[n]])))))))
  }
  return(Perplexity)
}



# returns the normally approximated expected number of terms per topic attaining a count of 
# at least minCount, given specified values of Aphi, W, N, K, and V. 
expNumTermsPerTopic <- function(Aphi, W, K, V, minCount){
  Aphi_0 = V*Aphi
  E_Aphi = Aphi/Aphi_0
  V_Aphi = Aphi*(Aphi_0-Aphi)/(Aphi_0^2*(Aphi_0+1))
  return(V*(1-pnorm((minCount*K/W-E_Aphi)/sqrt(V_Aphi))))
}

# Takes topic assignments for each post, dates for each post, topic indices, and topic labels
# and plots counts for each specified topic index over time.
plotDailyTopics_old <- function(topicAssignments, obsDates, topicIndices, topicLabels){
  relevantObs = which(topicAssignments%in%topicIndices)
  topicVec = topicLabels[match(topicAssignments[relevantObs],topicIndices)]
  dateVec = as.Date(obsDates[relevantObs], format="%m/%d/%Y")
  #df = data.frame(Topic = topicVec, Date = dateVec)
  #mdf = melt(df,id="Date")
  postCounts_kt = table(topicVec,dateVec)
  mpostCounts_kt = melt(postCounts_kt,id="Date")
  names(mpostCounts_kt) = c("Topic","Date","Value")
  mpostCounts_kt$Topic = factor(mpostCounts_kt$Topic)
  mpostCounts_kt[,2] = as.Date(mpostCounts_kt[,2])
  #mpostCounts_kt = mpostCounts_kt[which(mpostCounts_kt[,1]%in%topicIndices),]
  ggplot(mpostCounts_kt,aes(x=Date,y=Value,colour=Topic,group=Topic)) + 
    geom_line(size=1) + ylab("Number of Posts") + 
    theme(axis.text=element_text(size=15,color="black"),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=15),legend.title=element_text(size=16))
}

# Takes as an input the factor scores, factor loadings, date for each post, a date range
# of integers, topic indices (or if NULL, and labels are provided, finding the indices 
# via getIndices. If no labels provided either, defaulting to all indices), topic labels 
# (or if NULL, defaulting to topic k), and a minimum topic score for a post to be eligible
# for classification in that topic.
plotDailyTopics <- function(THETAkn, PHIvk, obsDates, dateRange, topicIndices, topicLabels, minExpCount){
  if (!is.numeric(topicIndices)){
    if (!is.character(topicLabels)){
      topicIndices = 1:dim(THETAkn)[1]
      topicLabels = paste("topic", as.character(topicIndices))
    } else {
      topicIndices = getTopicIndices(topicLabels, PHIvk)
    }
  } else {
    if (!is.character(topicLabels)){
      topicLabels = paste("topic", as.character(topicIndices))
    } 
  }
  topicAssignments = getTopicAssignments(THETAkn, minExpCount, topicIndices)
  dateList = sort(unique(obsDates))
  relevantObs = which((topicAssignments %in% topicIndices) & (obsDates %in% dateList[dateRange]))
  topicVec = topicLabels[match(topicAssignments[relevantObs],topicIndices)]
  dateVec = as.Date(obsDates[relevantObs], format="%m/%d/%Y")
  #df = data.frame(Topic = topicVec, Date = dateVec)
  #mdf = melt(df,id="Date")
  postCounts_kt = table(topicVec,dateVec)
  mpostCounts_kt = melt(postCounts_kt,id="Date")
  names(mpostCounts_kt) = c("Topic","Date","Value")
  mpostCounts_kt$Topic = factor(mpostCounts_kt$Topic)
  mpostCounts_kt[,2] = as.Date(mpostCounts_kt[,2])
  #mpostCounts_kt = mpostCounts_kt[which(mpostCounts_kt[,1]%in%topicIndices),]
  ggplot(mpostCounts_kt,aes(x=Date,y=Value,colour=Topic,group=Topic)) + 
    geom_line(size=1) + ylab("Number of Posts") + 
    theme(axis.text=element_text(size=15,color="black"),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=15),legend.title=element_text(size=16))
}

# This function takes the Gibbs samples from BNBP_PFA and reorders topic 
# labels based off the topic by iteration permutations matrix.
reorderTopicIndices <- function(Results, iterPerm_ks){
  Results[[1]] = do.call(cbind, lapply(1:dim(iterPerm_ks)[2], function(s) return(Results[[1]][iterPerm_ks[,s],s])))
  Results[[2]] = do.call(cbind, lapply(1:dim(iterPerm_ks)[2], function(s) return(Results[[2]][iterPerm_ks[,s],s])))  
  Results[[3]] = array(unlist(lapply(1:dim(iterPerm_ks)[2], function(s) return(Results[[3]][iterPerm_ks[,s],,s]))), dim=dim(Results[[3]]), dimnames=dimnames(Results[[3]]))
  Results[[4]] = array(unlist(lapply(1:dim(iterPerm_ks)[2], function(s) return(Results[[4]][,iterPerm_ks[,s],s]))), dim=dim(Results[[4]]), dimnames=dimnames(Results[[4]]))
  return(Results)
}

# This function takes the Gibbs samples from BNBP_PFA and uses method to reorder the topics 
# on each iteration based on the observations in obs_to_use and vocab words in vocab_to_use
# and lets the algorithm go maxiter times. This is very slow when using all obs and all vocab.
realignTopicLabels <- function(BNBP_PFA_results, method, obs_to_use, vocab_to_use, maxiter){
  P = N = dim(BNBP_PFA_results$theta_kns)[2]
  K = dim(BNBP_PFA_results$theta_kns)[1]
  V = dim(BNBP_PFA_results$phi_vks)[1]
  numSims = dim(BNBP_PFA_results$theta_kns)[3]
  if(method == "stephens"){
    p_snk = array(apply(BNBP_PFA_results$theta_kns, 1, function(s) return(t(s))),dim=c(numSims,N,K)) # create S by N by K array of iteration specific, observation specific, topic probabilities
    iterPerm_ks = t(stephens(p_snk[,obs_to_use,])$permutations)
  }
  if(method == "ecr.iterative.2"){
    p_snk = array(apply(BNBP_PFA_results$theta_kns, 1, function(s) return(t(s))),dim=c(numSims,N,K)) # create S by N by K array of iteration specific, observation specific, topic probabilities
    Z_sn = t(apply(BNBP_PFA_results$theta_kns, 3, function(s) return(apply(s,2,function(n_s) return(which.max(n_s)))))) # create S by N matrix of iteration specific, observation specific topic assignments
    iterPerm_ks = t(ecr.iterative.2(Z_sn[,obs_to_use], K, p_snk[,obs_to_use,], maxiter=maxiter)$permutations)
  }
  if(method == "aic"){
    mcmc_skv = array(apply(BNBP_PFA_results$phi_vks, 1, function(s) return(t(s))),dim=c(numSims,K,V)) # create S by K by V array of iteration specific, topic specific, parameter values
    iterPerm_ks = t(aic(mcmc_skv[,,vocab_to_use],1))
  }
  return(reorderTopicIndices(BNBP_PFA_results, iterPerm_ks))
}