# Text and network analysis helper functions:


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
library("ngram")
library("SnowballC")
library("lda")
library("psych")
library("stats")



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
  topNames = tolower(topNames)
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

# use ngram_tokenizer function from https://rpubs.com/brianzive/textmining:
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  options <- stringi::stri_opts_brkiter(type="word", skip_word_none = skip_word_none)
  function(x) {
    stopifnot(is.character(x))
    # Split into word tokens
    tokens <- unlist(stringi::stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stringi::stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}

# make find and replace bigrams and trigrams function that uses function
# from https://rpubs.com/brianzive/textmining:
find_replace_bigrams_trigrams <- function(doc_strings){
  # find bigrams:
  # white house is a bigram if:
  # white house occurrs at least twice in at least two documents 
  # p(white house) > p(white)p(house)
  doc_bigrams <- lapply(doc_strings, function(n) return(ngram_tokenizer(2)(n)))
  doc_bigram_counts = lapply(doc_bigrams, function(n) return(table(n)))
  doc_bigram_reps = lapply(doc_bigram_counts, function(n) return(names(n)[which(n>2)]))
  rep_bigram_doc_counts = table(unlist(doc_bigram_reps))
  common_bigram_doc_counts = sort(rep_bigram_doc_counts[which(rep_bigram_doc_counts>2)])
  bigram_candidates = names(common_bigram_doc_counts)
  p_bigram_obs = table(unlist(doc_bigrams))/length(unlist(doc_bigrams))
  doc_unigrams = lapply(doc_strings, function(n) return(ngram_tokenizer(1)(n)))
  p_unigram_obs = table(unlist(doc_unigrams))/length(unlist(doc_unigrams))
  p_bigram_indep_cands = lapply(bigram_candidates, function(n) return(prod(p_unigram_obs[unlist(strsplit(n, split=" "))])))
  p_bigram_obs_cands = p_bigram_obs[bigram_candidates]
  bigrams = bigram_candidates[which(p_bigram_obs_cands > p_bigram_indep_cands)]
  bigrams_underscore = unlist(lapply(bigrams, function(n) return(paste(unlist(strsplit(n, split=" ")), collapse="_"))))
  # find trigrams:
  doc_trigrams <- lapply(doc_strings, function(n) return(ngram_tokenizer(3)(n)))
  doc_trigram_counts = lapply(doc_trigrams, function(n) return(table(n)))
  doc_trigram_reps = lapply(doc_trigram_counts, function(n) return(names(n)[which(n>2)]))
  rep_trigram_doc_counts = table(unlist(doc_trigram_reps))
  common_trigram_doc_counts = sort(rep_trigram_doc_counts[which(rep_trigram_doc_counts>2)])
  trigram_candidates = names(common_trigram_doc_counts)
  p_trigram_obs = table(unlist(doc_trigrams))/length(unlist(doc_trigrams))
  doc_unigrams = lapply(doc_strings, function(n) return(ngram_tokenizer(1)(n)))
  p_unigram_obs = table(unlist(doc_unigrams))/length(unlist(doc_unigrams))
  p_trigram_indep_cands = lapply(trigram_candidates, function(n) return(prod(p_unigram_obs[unlist(strsplit(n, split=" "))])))
  p_trigram_obs_cands = p_trigram_obs[trigram_candidates]
  trigrams = trigram_candidates[which(p_trigram_obs_cands > p_trigram_indep_cands)]
  trigrams_underscore = unlist(lapply(trigrams, function(n) return(paste(unlist(strsplit(n, split=" ")), collapse="_"))))
  # replace all trigrams word1 word2 word3 with word1_word2_word3:
  for (i in 1:length(trigrams)){
    doc_strings = gsub(trigrams[i], trigrams_underscore[i], doc_strings)  
  }
  # replace all bigrams word1 word2 with word1_word2:
  for (i in 1:length(bigrams)){
    doc_strings = gsub(bigrams[i], bigrams_underscore[i], doc_strings)  
  }
  return(doc_strings)
}

# Create find topic pair closenesses function:
find_topic_pair_closeness <- function(topic_top_words_1, topic_top_words_2){
  K = dim(topic_top_words_1)[2]
  topic_pair_num_shared_in_num_top_words = matrix(0, nrow = K, ncol = K)
  for (k in 1:K){
    for (k2 in 1:K){
      topic_pair_num_shared_in_num_top_words[k, k2] = length(which(topic_top_words_1[, k] %in% topic_top_words_2[, k2])) 
    }
  }
  return(topic_pair_num_shared_in_num_top_words)
}

# Converts vector of element indices as a single number from 
# a matrix with num_rows to a matrix of indices where each 
# row is an element and in in (row, col) format.
ind_in_mat_2_mat_ind <- function(indices, num_rows){
  do.call(rbind,lapply(indices, function(ind) return(c(((ind - 1) %% num_rows) + 1, ceiling(ind / num_rows)))))
}

# Create function to align topics from one topic-word matrix to another:
alignTopics <- function(topic_top_words_1, topic_top_words_2, topic_pair_num_shared_in_num_top_words){
  num_topics_set = 0
  K = dim(topic_top_words_1)[2]
  sorted_closenesses_and_indices = sort(topic_pair_num_shared_in_num_top_words, decreasing = TRUE, index.return = TRUE)
  sorted_closenesses = sorted_closenesses_and_indices$x
  sorted_indices_matrix = ind_in_mat_2_mat_ind(sorted_closenesses_and_indices$ix, K)
  index = 1
  topics_repped_1 = NULL
  topics_repped_2 = NULL
  topic_map_1_2 = matrix(NA, K, 2)
  num_topics_confirmed = 0
  while ((num_topics_confirmed < K) & (index <= (K^2))){
    topic_ind_1 = sorted_indices_matrix[index, 1]
    topic_ind_2 = sorted_indices_matrix[index, 2]
    if ((!(topic_ind_1 %in% topics_repped_1)) & (!(topic_ind_2 %in% topics_repped_2))){
      num_topics_confirmed = num_topics_confirmed + 1
      topics_repped_1 = c(topics_repped_1, topic_ind_1)
      topics_repped_2 = c(topics_repped_2, topic_ind_2)
      topic_map_1_2[num_topics_confirmed, ] = sorted_indices_matrix[index, ] 
    }
    index = index + 1  
  }
  return (topic_map_1_2)
}

# Create function to permute topics of rtm results:
realign_rtm_topics <- function(rtm_results, rtm_perm){
  rtm_results$topics = rtm_results$topics[rtm_perm, ]
  rtm_results$topic_sums = rtm_results$topic_sums[rtm_perm]
  rtm_results$document_sums = rtm_results$document_sums[rtm_perm, ]
  rtm_results$assignments = lapply(rtm_results$assignments, function(n) return(match(n+1, rtm_perm)))
  return(rtm_results)
}

# Create function to permute topics of lda results:
realign_lda_topics <- function(lda_results, lda_perm){
  lda_results$topics = lda_results$topics[lda_perm, ]
  lda_results$topic_sums = lda_results$topic_sums[lda_perm]
  lda_results$document_sums = lda_results$document_sums[lda_perm, ]
  lda_results$document_expects = lda_results$document_expects[lda_perm, ]
  lda_results$assignments = lapply(lda_results$assignments, function(n) return(match(n+1, lda_perm)))
  return(lda_results)
}

# Function computes the mode from https://www.tutorialspoint.com/r/r_mean_median_mode.htm:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Removes vector of suffixes as strings from vector of urls as strings: 
removeSuf <- function(xxx,dotXs){
  for (dotX in dotXs){
    xxx[grep(dotX, xxx)] <- gsub(dotX,"",xxx[grep(dotX, xxx)]) #remove ".com"
  }
  return(xxx)
}

# Create function to make a visualization of the network over time,
# showing when nodes are active, when they link to each other, 
# what they post about and what community they are in:
# This function takes all posts during the specifed time, by the specified domains,
# occurring in the specified blocks, and counts them in a tensor. It Also obtains 
# the daily counts of links between any of the nodes specified or those occurring 
# in their links:
visualize_blog_network <- function(textNet, document_block_assignments, timesToView, nodesToView, blocksToView, network_topic, plot_each_time){
  
  # Subset data to the dates to view:
  #timeList = sort(unique(textNet$dates))
  #topicTimeIndices = which(textNet$dates %in% timeList[timesToView])
  #textNet = textNet[topicTimeIndices]
  
  # Get vectors of dates, domains, links for full data set:
  timeRange = sort(unique(textNet$dates))
  #domainList = sort(unique(textNet$domains))
  #linkList = sort(unique(unlist(lapply(textNet$links, function(n) return(strsplit(n, " "))))))
  #nodeList = sort(unique(c(domainVec, linkVec)))
  #topicList = sort(unique(document_block_assignments))
  
  # Subset data to the dates to view:
  #topicPostIndices = which(document_block_assignments %in% topicList[blocksToView])
  #topicTimeIndices = which(textNet$dates %in% timeList[timesToView])
  #topicNodeIndices = which(unlist(lapply(1:dim(textNet)[1], function(n) return(any(nodeList[nodesToView]%in%c(textNet$domains,unlist(strsplit(textNet$links, " "))))))))
  #nodesToViewChar = sort(unlist(textNet$domains))[nodesToView]
  #indices_to_view = which((document_block_assignments %in% sort(unique(document_block_assignments))[blocksToView]) & (textNet$dates %in% sort(unique(textNet$dates))[timesToView]) & unlist(lapply(1:dim(textNet)[1], function(n) return(any(c(textNet$domains[n], unlist(strsplit(textNet$links, " "))) %in% nodesToViewChar)))))
  #indices_to_view = which((document_block_assignments %in% sort(unique(document_block_assignments))[blocksToView]) & (textNet$dates %in% sort(unique(textNet$dates))[timesToView]) & (textNet$domains %in% sort(unique(textNet$domains))[nodesToView]))
  indices_to_view = which((document_block_assignments %in% sort(unique(document_block_assignments))[blocksToView]) & (textNet$dates %in% sort(unique(textNet$dates))[timesToView]) & (textNet$domains %in% sort(unique(textNet$domains))[nodesToView]))
  
  #if (length(indices_to_view)==0){ return("No documents satisfy these specifications.")}
  textNet = textNet[indices_to_view,]
  document_block_assignments = document_block_assignments[indices_to_view]
  
  # Get vectors of dates, domains, links for subset of data set:
  timeVec = sort(unique(textNet$dates))
  domainVec = sort(unique(textNet$domains))
  linkVec = sort(unique(unlist(lapply(textNet$links, function(n) return(strsplit(n, " "))))))
  nodeVec = sort(unique(c(domainVec, linkVec)))
  topicVec = sort(unique(document_block_assignments))
  
  # Collect counts for every node at each time and put in matrix:
  postCounts_kdt = array(0, dim=c(length(topicVec),length(nodeVec),length(timeVec)), dimnames=list(topicVec, nodeVec, timeVec))
  #postCounts_dt = matrix(0, nrow=length(nodeVec), ncol=length(timeVec), dim=list(nodeVec, timeVec))
  #postCountsDF = as.data.frame(table(textNet$dates, textNet$domains))
  #postCountsDFNonZero = postCountsDF[postCountsDF$Freq>0,]
  #for (i in 1:dim(textNet)[1]){
  #  postCounts_dt[postCountsDFNonZero$Var2[i], postCountsDFNonZero$Var1[i]] = postCountsDFNonZero$Freq[i]
  #}
  for (i in 1:dim(textNet)[1]){
    postCounts_kdt[match(document_block_assignments[i],topicVec), textNet$domains[i], textNet$dates[i]] = postCounts_kdt[match(document_block_assignments[i],topicVec), textNet$domains[i], textNet$dates[i]] + 1
  }
  postCounts_dt = apply(postCounts_kdt, 3, colSums)
  
  # Collect edges matrix with columns sender, receiver, time, weight, topic:
  linksArray <- array(0, dim=c(length(nodeVec),length(nodeVec),length(timeVec)), dimnames=list(nodeVec,nodeVec,timeVec))
  # Make links matrix (with edge weights proportionate to link multiplicity):
  links = lapply(textNet$links, function(n) return(unlist(strsplit(n, " "))))
  domains = textNet$domains
  dates = textNet$dates
  for (i in 1:length(links)){
    linksArray[ which(nodeVec==unlist(domains[[i]])) , which(nodeVec %in% unlist(links[[i]])) , which(timeVec==unlist(dates[[i]])) ] = linksArray[ which(nodeVec==unlist(domains[[i]])) , which(nodeVec %in% unlist(links[[i]])) , which(timeVec==unlist(dates[[i]])) ] + 1
  }
  edges_srtwk = do.call(rbind, lapply(1:length(timeVec), function(t){
    if (sum(postCounts_dt[,t])>0){
      edges_chunk = cbind(matrix(rep(rep(match(names(which(postCounts_dt[,t]>0)), nodeVec),
                                       postCounts_dt[which(postCounts_dt[,t]>0),t]),2),ncol=2), 
                        matrix(t, nrow=sum(postCounts_dt[,t]), ncol = 1), 
                        matrix(1, nrow=sum(postCounts_dt[,t]), ncol = 1),
                        matrix(0, nrow=sum(postCounts_dt[,t]), ncol = 1));                                                                                   
    if (sum(linksArray[,,t]) > 0){
      edges_chunk = rbind(edges_chunk, cbind(matrix(apply(get.edgelist(graph.adjacency(linksArray[,,t])),2,function(c) return(match(c, nodeVec))), nrow=sum(linksArray[,,t])), 
                                             rep(t, sum(linksArray[,,t])), rep(1, sum(linksArray[,,t])), 
                                             rep(1, sum(linksArray[,,t]))))
    }
    #return(edges_chunk)
  }}
  )
  )
  colnames(edges_srtwk) = c("sender", "receiver", "date", "count", "topic")
  edge_counts = as.data.frame(table(unlist(lapply(1:dim(edges_srtwk)[1], function(e) return(paste(edges_srtwk[e,], collapse=" "))))), stringsAsFactors = FALSE)
  new_edges_srtwk = do.call(rbind,lapply(1:dim(edge_counts)[1], function(e) return(unlist(strsplit(edge_counts$Var1[e], " ")))))
  new_edges_srtwk[,4] = edge_counts$Freq
  edges_srtwk = new_edges_srtwk
  colnames(edges_srtwk) = c("sender", "receiver", "date", "count", "topic")
  
  
  # plot graph over all time:
  layout_overall = layout_in_circle(graph.edgelist(as.matrix(edges_srtwk[,c(1,2)]),directed=TRUE))
  g <- graph.edgelist(as.matrix(edges_srtwk[,c(1,2)]),directed=TRUE)
  V(g)$name = nodeVec[as.numeric(V(g)$name)]
  V(g)$size = as.integer(rowSums(matrix(postCounts_dt[V(g)$name,], nrow=length(V(g)$name))))+1
  V(g)$label.size = 1.4
  E(g)$weight = as.numeric(ifelse(edges_srtwk[,1]==edges_srtwk[,2],0,edges_srtwk[,4]))
  E(g)$color = ifelse(edges_srtwk[,5]=="0","transparent","blue")
  plot(g,layout=layout_overall, vertex.size = V(g)$size, 
       vertex.label="", edge.width=.1+E(g)$weight, edge.color = E(g)$color,
       edge.arrow.size=.2, vertex.label.cex=V(g)$label.size,
       xlim = c(-1.0, 1.0), ylim = c(-1.5, 1.5),rescale=FALSE,asp=9/16, margin=-.1,
       main = paste(network_topic, "Network from ",timeRange[min(timesToView)]," to ",timeRange[max(timesToView)])
  )
  # Add labels:
  la = layout_overall
  x = la[,1]*1.5
  y = la[,2]*1.5
  #create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
  angle = ifelse(atan(-(la[,1]/la[,2]))*(180/pi) < 0,  90 + atan(-(la[,1]/la[,2]))*(180/pi), 270 + atan(-la[,1]/la[,2])*(180/pi))
  V(g)$name = removeSuf(V(g)$name, c(".com", ".net", ".org", ".blogspot", "blogs.", ".typepad", ".wordpress", "blog.", ".us", ".blogs", ".co.uk", ".talkingpointsmemo"))
  #Apply the text labels with a loop with angle as srt
  for (i in 1:length(x)) {
    text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, pos=NULL, cex=.7, col="black", srt=angle[i], xpd=T)
  }
  
  if (plot_each_time == TRUE){
    # plot graph at each time:
    for (t in 1:length(timeVec)){
      g <- graph.edgelist(as.matrix(edges_srtwk[,c(1,2)]),directed=TRUE)
      V(g)$name = removeSuf(V(g)$name, c(".com", ".net", ".org", ".blogspot", "blogs.", ".typepad", ".wordpress", "blog.", ".us", ".blogs", ".co.uk", ".talkingpointsmemo"))
      V(g)$size = as.integer(rowSums(matrix(postCounts_dt[V(g)$name, ceiling(t)], nrow=length(nodeVec))))+1
      V(g)$label.size = 1.4
      E(g)$weight = as.numeric(ifelse((edges_srtwk[,1]==edges_srtwk[,2]) | (edges_srtwk[,3]!=ceiling(t)),0,edges_srtwk[,4]))
      E(g)$color = ifelse((edges_srtwk[,5]=="0") | (edges_srtwk[,3]!=ceiling(t)),"transparent","blue")
      plot(g,layout=layout_overall, vertex.size = V(g)$size, 
           vertex.label="", edge.width=.1+E(g)$weight, edge.color = E(g)$color,
           edge.arrow.size=.2, vertex.label.cex=V(g)$label.size,
           xlim = c(-1.0, 1.0), ylim = c(-1.5, 1.5),rescale=FALSE,asp=9/16, margin=-.1
      )
      # Add labels:
      la = layout_overall
      x = la[,1]*1.5
      y = la[,2]*1.5
      #create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
      angle = ifelse(atan(-(la[,1]/la[,2]))*(180/pi) < 0,  90 + atan(-(la[,1]/la[,2]))*(180/pi), 270 + atan(-la[,1]/la[,2])*(180/pi))
      #Apply the text labels with a loop with angle as srt
      for (i in 1:length(x)) {
        text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, pos=NULL, cex=.7, col="black", srt=angle[i], xpd=T)
      }
    }
  }
}

