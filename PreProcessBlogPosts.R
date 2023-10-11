# Derek Owens-Oas
# 5/23/2016



###############################################################################################
###############################################################################################
# Pre-process text network:
###############################################################################################
###############################################################################################



# Set working directory:
#setwd("~/Documents/research/researchwithbanks/")
setwd("C:/Users/Derek/Documents/2018/spring/text_in_social_networks_code/dynamic_topic_and_community_detection")

# Set seed:
set.seed(12345)

# Source functions and packages:
source("PreProcessBlogPosts_Auxiliary.R")



# Read in raw data:
fl <- gzfile("for_tim.json.gz")
txt <- readLines(fl)
posts <- sapply(txt, fromJSON, USE.NAMES = F)



# Work with a subsample:
#sampleSize = ceiling(dim(posts)[2]) # subsample size
#posts = posts[,sample(seq(1,dim(posts)[2],1),sampleSize,replace=F)]



# Organize posts into dates, domains, linksList, tokensList, and raw text:
dates <- unlist(posts[7,]) 
domains <- unlist(posts[1,])
linksList <- posts[2,] 
tokensList <- posts[3,] 
rawText <- unlist(posts[6,])



# Read in raw data:
fl <- gzfile("prospect_posts_2016.json.gz")
txt <- readLines(fl)
posts <- sapply(txt, fromJSON, USE.NAMES = F)



# Work with a subsample:
#sampleSize = ceiling(dim(posts)[2]) # subsample size
#posts = posts[,sample(seq(1,dim(posts)[2],1),sampleSize,replace=F)]



# Organize posts into dates, domains, linksList, tokensList, and raw text:
dates <- unlist(posts[3,]) 
domains <- unlist(posts[1,])
linksList <- posts[2,] 
rawText <- unlist(posts[4,])



# Read in new raw data:
fl <- gzfile("blogs_2016.json.gz")
txt <- readLines(fl)
posts <- sapply(txt, fromJSON, USE.NAMES = F)



# Work with a subsample:
sampleSize = 10000 # ceiling(dim(posts)[2]) # subsample size
posts = posts[,sample(seq(1,dim(posts)[2],1),sampleSize,replace=F)]



# Organize posts into dates, domains, linksList, and raw text:
dates <- unlist(posts[2,])
domains <- unlist(posts[5,])
linksList <- posts[1,]
rawText <-unlist(posts[4,])


# edit domains: 
# (ie. get rid of "www."). This decreases the number of domains from 485 to 467.
domains[grep("www.", domains)] <- gsub("www.","",domains[grep("www.", domains)]) #remove "www."



# edit links:
# (ie. within each post, sort linksList alphabetically and remove duplicate linksList, linksList to 
# domains which don't post, and self linksList).

# remove blank from posts with links, and keep a blank for posts without any links.
linksList = lapply(linksList, removeBlankLinks) # initial total link count = 339914

# sort the links within each post alphabetically (if ordering of links within a post doesn't matter):
linksList = lapply(linksList, function(x) return(sort(unlist(x),decreasing=FALSE)))

# Remove 2198 duplicated links within each post (if duplicates of links within a single post don't count):
linksList = lapply(linksList,function(x) return(unique(unlist(x)))) # current total link count = 337716 

# Remove 46704 links to domains not among those posting (if external links don't count):
linksList = lapply(linksList,removeExternalLinks,unique(domains)) # current total link count = 291012 

# remove 9576 self links (if self links don't count):
linksList=lapply(1:length(linksList),removeSelfLinks,linksList,domains) # current total count = 281436

# make linksList into a vector of linksList sets where each link set is a single string:
links = unlist(lapply(linksList,function(x) return(paste(x,collapse=" ")))) 
# 55663 posts with "valid" links (excludes ""), 
# 81 unique "valid" links (excludes ""), 
#and 3369 "valid" link sets (including "")



# edit words:
# Clean raw text (to obtain strings of words separated by spaces):
text = rawTextToText(rawText)  
#wordsList2 = tokenize(rawText %>% str_replace_all(pattern="'",replacement="") %>% str_replace_all("[:digit:]",""), what="word", removePunct = TRUE, removeSymbols = TRUE, removeNumbers = TRUE, removeTwitter = TRUE, removeURL = TRUE, removeHyphens = TRUE) %>% toLower() 

# Change strings to word vectors:
initialWordsList = text %>% str_split(" ") 

# obtain word counts, post word counts, word document counts, 
# domain word counts, word domain counts, and initial vocabulary:
wordCounts = table(unlist(initialWordsList))
postWordCounts = lapply(initialWordsList,table)
wordDocCounts = table(unlist(lapply(postWordCounts,names)))
domainWordCounts = lapply(unique(domains), function(d) return(table(unlist(text[which(domains==d)]))))
wordDomCounts = table(unlist(lapply(domainWordCounts,names)))
initialVocabulary = names(wordCounts)

# read in alphabet letters, days, months, and numbers:
alphabet = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
daysOfWeek = c("monday","mon","mo","tuesday","tue","tu","wednesday","wed","we","thursday","thu","th","friday","fri","fr","saturday","sat","sa","sunday","sun","su")
monthsOfYear = c("january","jan","february","feb","march","mar","april","apr","may","june","jun","july","jul","august","aug","september","sep","october","oct","november","nov","december","dec")
numbers = c("one","two","three","four","five","six","seven","eight","nine","ten")
# read in stop words:
stopWords = as.character(read.csv("StopWords.csv")[,2])
# obtain infrequent words with frequency thresholding:
frequentThreshold = 10 #250
infrequentWords = names(which(wordCounts<frequentThreshold))
# obtain uncommon words with document count thresholding:
commonThreshold = 4 # 100
uncommonWords = names(which(wordDocCounts<commonThreshold))
# obtain unpopular words with domain count thresholding: 
popularThreshold = 2 # 50
unpopularWords = names(which(wordDomCounts<popularThreshold))
# combine to get words to remove and their indices in initial vocabulary:
wordsToRemove = unique(c(alphabet,daysOfWeek,monthsOfYear,numbers,stopWords,infrequentWords,uncommonWords,unpopularWords))
indicesToRemove = which(initialVocabulary%in%wordsToRemove)
# get final vocabulary:
finalVocabulary = initialVocabulary[-indicesToRemove]



# remove words not in vocabulary:
wordsList = removeNonVocabWords(initialWordsList,finalVocabulary)


# change word vectors back to strings:
words = unlist(lapply(wordsList,function(x) return(paste(x,collapse=" "))))

# Remove duplicate posts:
postVector = apply(cbind(dates,domains,links,words),1, function(x) return(paste(x,collapse=" ")))
postIndices = match(unique(postVector),postVector)
dates = dates[postIndices]
domains = domains[postIndices]
links = links[postIndices]
words = words[postIndices]
wordsList = wordsList[postIndices]



# Get sparse tfidf and tfidf_means:
P = length(wordsList)
wordDocCounts = table(unlist(lapply(wordsList,function(x) return(sort(unique(unlist(x)))))))
wordFreqs = table(unlist(wordsList))
vocabulary = names(wordFreqs)
idf = log(P/wordDocCounts)
twordsList = lapply(wordsList,function(x) return(table(unlist(x)))) # table wordsList for each post
getTfidf <- function(twrds,Idf){
  tab = unlist(twrds)
  return( tab * Idf[names(tab)] )    
}
sparse_tfidf = lapply(twordsList, getTfidf,idf)
tfidf_means = wordFreqs*idf/P

# Clear more space:
rm(twordsList)

# For each word, get tfidf squared deviations from the mean(tfidf)
getSquaredDevs <- function(sprsTfidf,tfidfMns){
  tab = unlist(sprsTfidf)
  return( (tab - tfidfMns[names(tab)])^2 )    
}
squaredDevs = lapply(sparse_tfidf, getSquaredDevs, tfidf_means)

# For each word: get var tfidf by taking the mean of the squared deviations from the mean tfidf
tfidf_var = rep(0,length(vocabulary))
names(tfidf_var) = vocabulary
# first add the squared devs for all posts which dont use each word
tfidf_var = (P-wordDocCounts)*(tfidf_means-0)^2
for (i in 1:P){
  usd = unlist(squaredDevs[i])
  tfidf_var[names(usd)] = tfidf_var[names(usd)] + usd
}
tfidf_var = tfidf_var/P
stfidf_var = sort(tfidf_var,decreasing=TRUE)
swordFreqs = sort(table(unlist(wordsList)),decreasing=TRUE)
swordDocCounts = sort(table(unlist(lapply(wordsList,function(x) return(sort(unique(unlist(x))))))),decreasing=TRUE)
View(cbind(swordDocCounts,stfidf_var[names(swordDocCounts)]))
View(cbind(swordDocCounts[names(stfidf_var)],stfidf_var))



# Discard words with tfidf variance lower than .3: 
lowVarTFIDF = which(tfidf_var<.5)
vocabulary = vocabulary[-lowVarTFIDF]


# remove words which are not in the vocabulary from documents:
wordsList = removeNonVocabWords(wordsList,vocabulary)



# change word vectors back to strings:
words = unlist(lapply(wordsList,function(x) return(paste(x,collapse=" "))))



# Make text network data frame:
textNetwork = data.frame(dates = dates,domains = domains,links = links,words = words) 



# Remove posts with fewer than 5 remaining words:
shortPosts = which(unlist(lapply(wordsList, function(n) return(length(n)<10))))
textNetwork = textNetwork[-shortPosts,]



# Order posts chronologically, and then alphabetically by domain, then links, then words:
orderedPostIndices = order(unlist(lapply(1:dim(textNetwork)[1], function(n) return(paste(textNetwork[n,],collapse=" ")))))
textNetwork = textNetwork[orderedPostIndices,]



# Write text network data frame to file: 
write.csv(textNetwork,"textNetwork.csv")



# Read text network data frame:
X = fread("textNetwork.csv")
