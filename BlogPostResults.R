# Derek Owens-Oas



# Set working directory:
#setwd("~/research/banks/2015/viz")
#setwd("F:/months")
#setwd("/home/grad/dmo11/Documents/research/researchwithbanks/months")
#setwd("/media/PQI_H567L/months")
#setwd("C:/Users/Derek/Desktop/new")
#setwd("~/Documents/research/2016/")
setwd("C:/Users/Derek/Documents/2018/spring/text_in_social_networks_code/dynamic_topic_and_community_detection/")



# Source functions and packages:
source("BlogPostResults_Auxiliary.R")



# Load specifications, hyperparameters, data, and results from BNBP PFA into workspace:
load("BNBP_PFA.Rdata")



# Use re-alignment algorithms to permute topic labels across mcmc iterations:
# new_results = realignTopicLabels(BNBP_PFA_results, method = "ecr.iterative.2", obs_to_use = 1:10, vocab_to_use = 1:10, maxiter = 5)



# Set up results from BNBP PFA and Marked Beta Negative Binomial Process
# analysis (posterior samples for p_k, r_k, phi_vk, and theta_kn):
p_ks = BNBP_PFA_results$p_ks
r_ks = BNBP_PFA_results$r_ks
theta_kns = BNBP_PFA_results$theta_kns
phi_vks = BNBP_PFA_results$phi_vks
#rm(BNBP_PFA_results)



# Calculate per word perplexity on held out test data based on all Gibbs samples:
Perplexity = calcPerWordPerplexity(y_vn, BNBP_PFA_results, minProb, cores, N)
Perplexity


# Posterior means:
p_k_post_mean = apply(BNBP_PFA_results$p_ks,1,mean)
r_k_post_mean = apply(BNBP_PFA_results$r_ks,1,mean)
theta_kn_post_mean = apply(BNBP_PFA_results$theta_kns,2,rowMeans)
phi_vk_post_mean = apply(BNBP_PFA_results$phi_vks,2,rowMeans)
write.csv(p_k_post_mean, "p_k_150.csv")
write.csv(r_k_post_mean, "r_k_150.csv")
write.csv(phi_vk_post_mean, "phi_vk_150.csv")
write.csv(theta_kn_post_mean, "theta_kn_150.csv")



# Determine which topics are active:
activeTopicIndices = which(p_k_post_mean>.95) # Threshold posterior means of NB probability parameter
activeTopicIndices = which(rowSums(theta_kn_post_mean)>1) # Threshold factor scores parameter, marginalized over all observations



# Get top words from each topic:
#topicNames = c("hurricane sandy", "immigration", "voting", "president", "global warming", "health insurance", "sports", "education", "tech companies", "london olympics")
topicNames = c("election", "economy", "marriage", "education")
topWords = presentTopWords(phi_vk_post_mean, 10)
selectedTopWords = presentTopWords(phi_vk_post_mean, 10, topicNames)
selectedTopWords
selectedSpecificWords = presentSpecificWords(topicNames, phi_vk_post_mean, 10)
selectedSpecificWords
write.csv(selectedTopWords, "selected_topic_top_words_150.csv")
allTopics = getTopWords_k(phi_vk_post_mean, 100)
activeTopics = allTopics[,activeTopicIndices]
write.csv(allTopics, "topWords150.csv")
exampleTopics = allTopics[,getTopicIndices(topicNames, phi_vk_post_mean)]



# Get topic assignments for each observation:
dateRange23 = 2:3
dateRange17 = 1:10
topicIndices = c(3,17,97,196)
topicAssignments = getTopicAssignments(theta_kn_post_mean, 1, getTopicIndices(topicNames, phi_vk_post_mean))
dailyExpectedTopicRates = do.call(cbind,lapply(indices_t, function(n_t) return(rowSums(theta_kn_post_mean[getTopicIndices(topicNames, phi_vk_post_mean),n_t]))))
#plotDailyTopics(theta_kn_post_mean, phi_vk_post_mean, textNetwork$dates[unlist(indices_t)], dateRange23, NULL, c("abortion", "sex", "news", "iraq"), 5)
plotDailyTopics(theta_kn_post_mean, phi_vk_post_mean, textNetwork$dates[unlist(indices_t)], dateRange17, topicIndices, topicNames, 5)
write.csv(theta_kn_post_mean, "documentTopicScores150.csv")



# Do each batch in parallel:
#batches = cores # Number of parallel simulations
#TperB = ceiling(T_max/batches) # Number of days per batch
#indices_t = lapply(sort(unique(textNetwork$dates)), function(t) return(which(textNetwork$dates==t)))[1:T_max] # Indices of observations at each time t
#indices_b = lapply(1:batches,function(b) return(which(textNetwork$dates%in%sort(unique(textNetwork$dates))[(1+TperB*(b-1)):ifelse(TperB*(b-1)+TperB>T_max,T_max,TperB*(b-1)+TperB)]))) # Indices of observations in batch b
#x_vn_b = lapply(indices_b, function(b) return(x_vn[b]))
#BNBP_PFA_results_b <- mclapply(x_vn_b, function(x_b) return(Gibbs_Sample_BNBP_PFA(x_b, K, S, burn, thin, c, epsilon, e_0, f_0, a_phi)),mc.cores=cores)
#p_k_post_mean_b = mclapply(1:batches, function(b) return(apply(BNBP_PFA_results_b[[b]]$p_ks,1,mean)))
#phi_vk_post_mean_b = mclapply(1:batches, function(b) return(apply(BNBP_PFA_results_b[[b]]$phi_vks,2,rowMeans)))
#activeTopics_b = mclapply(1:batches, function(b) return(getTopWords_k(phi_vk_post_mean_b[[b]],100)[,which(p_k_post_mean_b[[b]]>.99)]))



# Obtain specific words for each topic:
specificWords_k = getSpecificWords_k(BNBP_PFA_results$phi_vks, 10)
activeSpecificWords_k = specificWords_k[,which(p_k_post_mean>.95)]



# Obtain posterior probabilities of topics given words:
N_v = table(unlist(lapply(x_vn, names)))
N_v = table(unlist(str_split(textNetwork$words[unlist(indices_t)], " ")))
topicGivenWord = getTopicGivenWord(phi_vk_post_mean,theta_kn_post_mean, N_v, 100)
activeTopicGivenWord = topicGivenWord[,which(p_k_post_mean>.95)]



# View p_k and r_k boxplots of posterior samples:
K = K_max
sampSize = length(burn+which(((burn+1):S)%%thin==0))
#boxplot(as.numeric(BNBP_PFA_results[[1]])~rep(1:K,sampSize))
#boxplot(as.numeric(BNBP_PFA_results[[2]])~rep(1:K,sampSize))
#boxplot(as.numeric(N*BNBP_PFA_results[[2]])*as.numeric(BNBP_PFA_results[[1]])/(1-as.numeric(BNBP_PFA_results[[1]]))~rep(1:K,sampSize))
boxplot(as.numeric(t(apply(BNBP_PFA_results[[3]],1,colSums)))~rep(1:K,sampSize), main="Topic Prevalence Posterior Distributions", xlab="Topic", ylab="Expected Word Count")
#boxplot(1/(1-as.numeric(BNBP_PFA_results[[1]]))~rep(1:K,sampSize))



#source("auxiliary.R")

# date list:
dateSeq = seq(from=as.Date("2012-01-01"),by=1,length.out=367)

# read in topic assignments:
#topAssign = read.csv("assignRun.csv")[,2]
#topAssign = read.csv("16TopicAssign.csv")[,2]
#topAssign = sample(1:K,N,replace=TRUE) # added this part recently since topAssign was wrong length
topAssign = topicAssignments
topNames = paste("topic", 1:K)



# label events:
sensCrimeEvents = read.csv("sensCrimeEvents.csv",header=FALSE)
sensCrimeEvents = data.frame(eventDate=as.Date(sensCrimeEvents[,1],format = "%d-%b-%y"),event=sensCrimeEvents[,2])
#presElecEvents = matrix(as.character(read.csv("presElecEvents.csv",header=FALSE)$V1),ncol=2,byrow=TRUE)
#presElecEvents = data.frame(eventDate=as.Date(presElecEvents[,1],format = "%d-%b-%y"),event=presElecEvents[,2])
#presElecEvents$eventDate = match(presElecEvents$eventDate,dateSeq)
eventDates = match(sensCrimeEvents$eventDate,dateSeq)
events = as.character(sensCrimeEvents$event)
#eventDates= c(11,14,18,21,25,27,29,30) #15 #22
#events = c(
#  "Gunman Opens Fire in Oregon Mall"
#  ,"Sandy Hook Elementary School Shooting"
#  ,"Health Workers Killed in Pakistan"
#  ,"Gunman and Police Killed in Highway Shootout"
#  ,"Irish Catholics fight to prevent abortion legislation"
#  ,"Legislators criticize anti-abortion Catholics"
#  ,"Suicide bomb Iraq and re-elect in China"
#  ,"SOPA protest"
#)

# days of the year info:
daysToView = seq(1,T_max,1)
daysPerMonth = c(31,29,31,30,31,30,31,31,30,31,30,31)
names(daysPerMonth) = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")



#####################
#####################
#####################
### add this part ###
# Organize Data:
# Posts:  
textNetwork = textNetwork[unlist(indices_t),]
N = dim(textNetwork)[1]
# Dates:
t_n = textNetwork[[1]]
N_t = table(t_n)
dates = names(N_t)
T = length(dates)
ind_t = lapply(dates, function(t) return(which(t_n==t)))
names(ind_t) = dates
# Domains: 
d_n = textNetwork[[2]]
N_d = table(d_n)
domains = names(N_d)
D = length(domains)
ind_d = lapply(domains, function(d) return(which(d_n==d)))
names(ind_d) = domains
# Links:
l_n = textNetwork[[3]]
d_ln = lapply(l_n, function(n) return(unlist(str_split(n," "))))
d_l = unlist(d_ln)
L_n = unlist(lapply(d_ln, function(n) return(length(which(nchar(n)>0)))))
L = sum(L_n)
L_d = table(d_l)
links = names(L_d)
L = length(links[-1]) # added this part 
ind_l = lapply(links, function(l) return(which(unlist(lapply(d_ln,function(n) return(l%in%n))))))
names(ind_l) = links
# Words:
w_n = textNetwork[[4]]
v_wn = lapply(w_n,function(n) return(unlist(str_split(n," "))))
v_w = unlist(v_wn)
W_n = unlist(lapply(v_wn, function(n) return(length(which(nchar(n)>0)))))
W = sum(W_n)
W_v = table(v_w)
vocabulary = names(W_v)
V = length(vocabulary)
W_vn = lapply(v_wn, table)
v_n = lapply(W_vn, names)
V_n = unlist(lapply(v_n, length))
I_vn = lapply(v_n, function(n) return(match(n,vocabulary)))
Iv_wn = lapply(v_wn, function(n) return(match(n,vocabulary)))


lnkmat=do.call(rbind,lapply(d_ln,function(n) return(as.numeric(links[-1]%in%n))))
posts = cbind(rep(1,N),t_n,d_n,lnkmat=do.call(rbind,lapply(d_ln,function(n) return(as.numeric(links[-1]%in%n)))))
linkList=links[-1]
#####################
#####################
#####################


colnames(posts) = c("topic","date","domain",linkList)
P = nrow(posts)
#topAssign = topAssign[1:P]
#posts[,1] = topAssign
posts=as.data.frame(posts)
posts[,3] = as.character(posts[,3])
posts[,3][grep("-",posts[,3])] <- gsub("-",".",posts[,3][grep("-",posts[,3])])
#posts$topic = as.character(posts$topic)
#posts$topic = topicID[posts$topic]

#posts$topic = as.factor(posts$topic)
#posts$domain = as.character(posts$domain)
posts$date = as.Date(as.character(posts$date), format = "%m/%d/%Y")
posts[,(1+1+1+1):(1+1+1+L)] = apply(posts[,(1+1+1+1):(1+1+1+L)],2,as.numeric)
posts[,linkList]=matrix(as.numeric(as.matrix(posts[,linkList])),nrow=P,ncol=L)

postsToView = which(posts$date%in%sort(unique(posts$date))[daysToView])
posts = posts[postsToView,]
#posts$topic = factor(posts$topic)
P = nrow(posts)
posts$topic = topAssign
#write.csv(posts,file="yearPosts.csv")

# can just skip to this point and read in posts: 
# posts = read.csv("yearPosts.csv")


#topAssign = topicID[topAssign]
#posts$topic = topicAssignment[posts$topic]
#topAssign = factor(topAssign[postsToView])

#levels(posts$topic)



# tabulate posts by topic, domain, date:
tab = table(posts$topic,posts$domain,posts$date)
K = length(unique(topAssign))
D = length(unique(posts[,3]))
T = length(unique(posts[,2]))
KDT = array(as.numeric(tab),dim=c(K,D,T),dimnames=dimnames(tab))
KT = apply(KDT,3,rowSums)
DT = apply(KDT,3,colSums)
DK = apply(KDT,2,rowSums)
rm(tab)

topicList = as.character(unique(posts$topic))
domainList = sort(as.character(unique(posts$domain)))
dateList = unique(as.Date(as.character(posts$date)))
linkedDomains = which(colSums(posts[,(1+1+1+1):(1+1+1+L)])>0)
linkList = names(posts)[(1+1+1+1):(1+1+1+L)][linkedDomains]
nodeList = sort(unique(c(domainList,linkList)))




# pick and plot desired topics:
#desiredTopics = head(names(sort(rowSums(KT),decreasing=TRUE)))
data=posts
#rm(posts)





# for plotting
#library(reshape2)
#library(ggplot2)

# make function to plot topic activity over time for any subset of topics
plotTOT <- function(posts, postCounts_kt, tops, topicIndex, days, dateIndex){
  topicVec = posts$topic[which(posts$topic%in%topicIndex[tops])]
  timeVec = as.Date(posts$date[which(posts$topic%in%topicIndex[tops])])
  df = data.frame(Topic = topicVec, Date = timeVec)
  mdf = melt(df,id="Date")
  mpostCounts_kt = melt(postCounts_kt,id="Date")
  names(mpostCounts_kt) = c("Topic","Date","Value")
  mpostCounts_kt$Topic = factor(mpostCounts_kt$Topic)
  mpostCounts_kt[,2] = as.Date(mpostCounts_kt[,2])
  mpostCounts_kt = mpostCounts_kt[which(mpostCounts_kt[,1]%in%topicIndex[tops]),]
  ggplot(mpostCounts_kt,aes(x=Date,y=Value,colour=Topic,group=Topic)) + 
    geom_line(size=1.4) + ylab("Number of Posts") + 
    theme(axis.text=element_text(size=15,color="black"),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=15),legend.title=element_text(size=16))
}
plotTOT2 <- function(postCounts_kt,tops){
  topicVec = topicIndex[posts$topic[which(posts$topic%in%tops)]]
  timeVec = as.Date(dateIndex[posts$date[which(posts$topic%in%tops)]])
  df = data.frame(Topic = topicVec, Date = timeVec)
  mdf = melt(df,id="Date")
  mpostCounts_kt = melt(postCounts_kt,id="Date")
  names(mpostCounts_kt) = c("Topic","Date","Value")
  mpostCounts_kt$Topic = factor(mpostCounts_kt$Topic)
  mpostCounts_kt[,2] = as.Date(mpostCounts_kt[,2])
  mpostCounts_kt = mpostCounts_kt[which(mpostCounts_kt[,1]%in%tops),]
  ggplot(mpostCounts_kt,aes(x=Date,y=Value,colour=Topic,group=Topic)) + 
    geom_line(size=1.4) + ylab("Number of Posts") + 
    theme(axis.text=element_text(size=15,color="black"),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=15),legend.title=element_text(size=16))
}
# try it
#topicIndex = 1:K
dateIndex = dateList
topicIndex = as.numeric(names(table(posts$topic)))
#plotTOT(KT,topicID[19])
tops = c(2,3,4,5)
days = c(1,2,3,4,5)
plotTOT(data, KT, tops, topicIndex, days, dateList)
#plotTOT(KT,topicID[15])
#plotTOT(KT,topicID[c(52,60)])
#plotTOT(KT,topicID[6])
#active = head(names(sort(rowSums(KT),decreasing=TRUE)))
#plotTOT(KT,active)
#plotTOT(log(KT),active)

##########################################################################################
############ NOTE TO SELF: UPDATE THE BELOW FUNCTION TO BETTER MATCH WITH plotTOT ########
##########################################################################################

# make function to plot topic activity over time for any subset of topics
# make function to plot topic activity over time for any subset of topics
plotDOT <- function(posts, postCounts_dt, doms, domainIndex, days, dateIndex){
  domainVec = posts$domain[which(posts$domain%in%domainIndex[doms])]
  timeVec = as.Date(posts$date[which(posts$domain%in%domainIndex[doms])])
  df = data.frame(Domain = domainVec, Date = timeVec)
  mdf = melt(df,id="Date")
  mpostCounts_dt = melt(postCounts_dt,id="Date")
  names(mpostCounts_dt) = c("Domain","Date","Value")
  mpostCounts_dt$domain = factor(mpostCounts_dt$Domain)
  mpostCounts_dt[,2] = as.Date(mpostCounts_dt[,2])
  mpostCounts_dt = mpostCounts_dt[which(mpostCounts_dt[,1]%in%domainIndex[doms]),]
  ggplot(mpostCounts_dt,aes(x=Date,y=Value,colour=Domain,group=Domain)) + 
    geom_line(size=1.4) + ylab("Number of Posts") + 
    theme(axis.text=element_text(size=15,color="black"),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=15),legend.title=element_text(size=16))
}
# try it
domainIndex = domainList
#plotTOT(KT,topicID[19])
doms = c(4,6,8)
days = c(2,3,4,5)
#plotTOT(data, KT, tops, topicIndex, days, dateList)
plotDOT(data,DT,doms,domainIndex, days, dateList)
#active = head(names(sort(rowSums(DT),decreasing=TRUE)))
#plotDOT(DT,active)
#plotDOT(log(DT),active)
#dev.off()

#write.csv(posts,file="yearPosts.csv")



#data$date = as.Date(as.character(data$date))
#data$domain = as.character(data$domain)

# Get dateList
dateList = sort(unique(data$date))
T = length(dateList) # Number of days

# decide which days to view:
#dateList = sort(unique(data$date))

# select events in this time span:
events = events[which(eventDates%in%daysToView)]
eventDates = eventDates[which(eventDates%in%daysToView)]

# Specifications:
P = dim(data)[1] # Number of posts
L = L # Number of unique links
V = V # Number of unique vocabulary items

# Get domainList, linkList, and wordList:
domainList = sort(unique(as.character(data$domain)))
D = length(domainList) # Number of domains
linkedDomains = which(colSums(data[,(1+1+1+1):(1+1+1+L)])>0)
linkList = names(data)[(1+1+1+1):(1+1+1+L)][linkedDomains]
E = sum(data[,(1+1+1+1):(1+1+1+L)]) # Number of total links
#wordList = names(data)[(1+1+1+L+1):(1+1+1+L+V)]
#W = sum(data[,(1+1+1+L+1):(1+1+1+L+V)]) # Number of total words
nodeList = sort(unique(c(domainList,linkList)))

# use word occurrences to create and select topics to view:
# Find pres candidates and name 5 topics as follows: 
# other, obama, romney, santorum, paul

#wds = data[,(1+1+1+L+1):(1+1+1+L+V)]
#economy = which(colnames(wds)=="econom")
#environment = which(colnames(wds)=="environment")
#defense = which(colnames(wds)=="defens")
#wd = c("guns","abortion")
#wd = "guns"
#wd=c("religion","science")
#religion = which(colnames(wds)=="scienc")
#science = which(colnames(wds)=="religi")

#guns = which(colnames(wds)=="gun")
#abortion = which(colnames(wds)=="abort")
#sex = which(colnames(wds)=="sex")
#technology = which(colnames(wds)=="technolog") 
#music = which(colnames(wds)=="music")
#sports = which(colnames(wds)=="sport")
#religion = which(colnames(wds)=="religi")
#pres = wds[,c(economy,environment,education,defense,health,sex,technology,music,sports,religion)]
#topNames = c("economy","environment","education","defense","health","sex","technology","music","sports","religion","other")
#pres = wds[,c(religion,science)]
#pres = wds[,c(guns)]
#topNames = c("religion","science")
#K = length(topNames)
#topColors = c(brewer.pal(K,"Spectral"),"grey")
#topColors = c(brewer.pal(K+2,"Dark2")[seq(1,K,1)])


#wds = data[,(1+1+1+L+1):(1+1+1+L+V)]
#obama = which(colnames(wds)=="obama")
#romney = which(colnames(wds)=="romney")
#santorum = which(colnames(wds)=="santorum")
#paul = which(colnames(wds)=="paul")
#pres = wds[,c(obama,romney,santorum,paul)]
#topNames = c("obama","romney","santorum","paul","other")
#topColors = c(brewer.pal(K-1,"Spectral"),"grey")
#K = length(topNames)

# Add topics column to data:
#tops = apply(as.matrix(pres,ncol=dim(wds)[1]),1,function(x) return(ifelse(sum(x)==0,K+1,which.max(x))))
#tops = apply(pres,1,function(x) return(which.max(x)))
#tops = apply(pres,1,function(x) return(sample(1:K,size=1,prob=rep(1/K,K),replace=TRUE)))
#data[,1] = tops
#colnames(data) = c("topic",colnames(data)[-1])

##### optional part to take subset #####
########################################
# work with a desirable subset: that is, all domains that post about or are linked about a specified topic.
# try health for now:

#ind=which(topNames%in%wd)
#topNames = topNames[ind]
#topColors = topColors[ind]

#packages for color
#library(RColorBrewer)
#library(scales)
#library(igraph)

#desiredTopics = topicID[c(19)]# and 52, and 60 (for months after jan)
#desiredTopics = topicID[c(15)]
#topNames = desiredTopics
#K = length(topNames)
#topColors = c(brewer.pal(K+2,"Dark2")[seq(1,K,1)])

# I BELIEVE WE WILL WANT TOPNAMES = TOPNAMES

#IMPORTANT:
# make topics numeric:
#data$topic=match(data$topic,desiredTopics)

# from among topNames, numerically select the desired Topics:
desiredTopics = topicIndex[c(3,5)]
#desiredTopics = c(6)
#desiredTopics = c(3,19,31)
healthPosts = which(data$topic%in%desiredTopics)
healthData = data[healthPosts,]

# discard posts by domains with only one (x) post(s) on the topic and who were linked only once (y times) or fewer
goodDoms = names(which(sort(table(healthData$domain),decreasing=TRUE)>9)) # a stricter version of domain and link filtering for smaller networks.
goodLinks = names(which(colSums(healthData[,4:(3+L)])>9)) 
goodNodes = sort(unique(c(goodDoms,goodLinks)))
#zero the bad links and delete posts by bad domains
healthData[,names(healthData[4:(3+L)])[!names(healthData[,4:(3+L)])%in%goodNodes]] = 0
healthData = healthData[which(healthData$domain%in%goodNodes),]

healthLinks = which(colSums(healthData[,4:(3+L)])>0)
healthLinkList = sort(unique(names(healthData[,4:(3+L)])[healthLinks]))
healthDomainList = sort(unique(healthData$domain))
healthNodeList = sort(unique(c(healthDomainList,healthLinkList)))
data=healthData
rm(healthData)
domainList=healthDomainList
linkList=healthLinkList
nodeList=healthNodeList
D= length(domainList)
P=dim(data)[1]

data$topic = match(data$topic,desiredTopics)



library(RColorBrewer)
topNames = topNames[desiredTopics]
K = length(topNames)
#topColors = c(brewer.pal(K,"Spectral"),"grey")
topColors = c(brewer.pal(K+2,"Dark2")[seq(1,K,1)])

#library(scales)
#library(igraph)


########################################

# Make array of post counts for each domain each day on each topic:

# This returns the count of posts on each node on each day, in days to view:
# getPostCounts_dt <- function(data, daysToView, nodeList, domainList, linkList){
#   #dateDom = matrix(table(data[,3],data[,2]),nrow=length(unique(data[,3])))
#   #dateDom = matrix(table(data$domains,data$dates),nrow=length(unique(data$domains)))
#   if(dim(dateDom)[2]<length(daysToView)){
#     cols2add = matrix(0,nrow=dim(dateDom)[1],ncol=length(daysToView)-dim(dateDom)[2])
#     dateDom = cbind(dateDom,cols2add)
#     colsNames = match(dateList[which(!dateList[daysToView]%in%sort(unique(data$date)))],dateList)
#     newColNames = c(match(sort(unique(data$date)),dateList),colsNames)
#     od = sort(newColNames,index.return=TRUE)$ix
#     dateDom = dateDom[,od]
#   }
#   if (length(unique(data$domain))<length(nodeList)){
#     rows2add = matrix(0,nrow=length(which(!linkList%in%domainList)),ncol=T)
#     rownames(rows2add) = linkList[which(!linkList%in%domainList)]   
#     colnames(rows2add) = as.character(dateList)
#     dateDom = rbind(dateDom,rows2add)
#     rownames(dateDom) = c(domainList,rownames(rows2add))
#     dateDom = dateDom[sort(rownames(dateDom)),]
#     colnames(dateDom) = as.character(dateList)  
#   }
#   return(dateDom)
# }
#dateDom = getPostCounts_dt(textNetwork, daysToView, nodeList, domainList, linkList)
#dimnames(dateDom) = list(domainList,dateList)
# Get vectors of dates, domains, links:
timeVec = sort(unique(textNetwork$dates))
domainVec = sort(unique(textNetwork$domains))
linkVec = sort(unique(unlist(lapply(textNetwork$links, function(n) return(strsplit(n, " "))))))
nodeVec = sort(unique(c(domainVec, linkVec)))

# Collect counts for every node at each time and put in matrix:
postCounts_dt = matrix(0, nrow=length(nodeVec), ncol=length(timeVec), dim=list(nodeVec, timeVec))
#postCountsDF = as.data.frame(table(textNetwork$dates, textNetwork$domains))
#postCountsDFNonZero = postCountsDF[postCountsDF$Freq>0,]
#for (i in 1:dim(textNetwork)[1]){
#  postCounts_dt[postCountsDFNonZero$Var2[i], postCountsDFNonZero$Var1[i]] = postCountsDFNonZero$Freq[i]
#}
for (i in 1:dim(textNetwork)[1]){
  postCounts_dt[textNetwork$domains[i], textNetwork$dates[i]] = postCounts_dt[textNetwork$domains[i], textNetwork$dates[i]] + 1
}
dateDom = postCounts_dt


#install.packages("abind")
library(abind)
# Get node colors matrix:
#KDT = array(table(data$topic,data$domain,data$date),dim=c(K,D,length(unique(data$date))),dimnames=list(topNames,domainList,as.character(sort(unique(data$date)))))
#KDT = array(table(topNames[match(data$topic,desiredTopics)],data$domain,data$date),dim=c(K,D,length(unique(data$date))),dimnames=list(sort(topNames),domainList,as.character(sort(unique(data$date)))))
KDT = array(table(topNames[data$topic],data$domain,data$date),dim=c(K,D,length(unique(data$date))),dimnames=list(sort(topNames),domainList,as.character(sort(unique(data$date)))))
#getMainTop <- function(domTops) {
#return(apply(domTops,2,return(ifelse(sum(x[-K])==0,K,which.max(x[-K])))))
#}

# add matrices of 0s to the array for each day in the range to view which had no posts
if(length(unique(data$date))<length(dateList)){
  dates2add = which(!dateList%in%data$date)
  #mat2add = matrix(0,nrow=K,ncol=D)
  mat2add = array(0,dim=c(K,D,length(dates2add)),dimnames=list(sort(topNames),domainList,as.character(dateList[dates2add])))
  #rownames(mat2add)=topNames
  #colnames(mat2add)=domainList
  KDT2 = abind(KDT,mat2add)
  #dimnames(KDT2)[[3]][[seq(length(dateList)-length(dates2add)+1,length(dateList),1)]] = as.character(dateList[dates2add])
  ord= sort(c(match(sort(unique(data$date)),dateList),dates2add),index.return=TRUE)$ix
  KDT2=array(KDT2[,,ord],dim=c(K,D,T))
  dimnames(KDT2) = list(sort(topNames),domainList,as.character(dateList))
  KDT=KDT2
}

KDT = array(KDT[topNames,,],dim=c(K,D,T),dimnames=list(sort(topNames),domainList,as.character(dateList)))



#DT = apply(KDT,3,getMainTop)
DT = apply(KDT,3,function(x) return(apply(matrix(x,ncol=D),2,function(y) return(ifelse(sum(y)==0,K+1,which.max(y)))))) 
#DT = matrix(apply(KDT,3,function(x) return( apply(matrix(x,ncol=D),2,function(y) return(ifelse(sum(y)==0,K+1,which.max))) )),nrow=D)
if(length(domainList)<length(nodeList)){
  rownames(DT) = domainList
  rows2add = matrix(K+1,nrow=length(which(!linkList%in%domainList)),ncol=T)
  rownames(rows2add) = linkList[which(!linkList%in%domainList)]   
  colnames(rows2add) = as.character(dateList)
  DT = rbind(DT,rows2add)
  rownames(DT) = c(domainList,rownames(rows2add))
  DT = DT[sort(rownames(DT)),]
  colnames(DT) = as.character(dateList)
}

# Each day, get top day specific vocab for each topic:
#tWords = data[,c(1,(4+L):dim(data)[2])]





# Write data to edge matrix function:
dataToEdgeMatrix <- function(data){
  link2node = match(names(data[,(1+1+1+1):(1+1+1+L)]),nodeList)
  ndomains = match(data[,(1+1+1)],nodeList)
  ndates = match(as.Date(as.character(data[,(1+1)])),dateList)
  nlinks = as.matrix(data[,(1+1+1+1):(1+1+1+L)],ncol=L)
  linksPerPost = rowSums(nlinks)+1 # add 1 and give each post a self link
  nlinks = cbind(nlinks,ndomains)
  cnames = c("posting","linked","date","weight","topic")
  eposting = rep(ndomains,linksPerPost)
  etopic = rep(data[,1],linksPerPost)
  eposting = rep(ndomains,linksPerPost)
  elinked = unlist(apply(nlinks,1,function(x) return(c(x[(L+1)],link2node[which(x[-(L+1)]>0)]))))
  edate = rep(ndates,as.integer(linksPerPost))
  eweight = as.integer(eposting!=elinked)
  edgeMat = matrix(cbind(eposting,elinked,edate,eweight,etopic), ncol=length(cnames))
  colnames(edgeMat) = cnames
  return(edgeMat)
}
edgeMatrix = dataToEdgeMatrix(data)

# aggregate identical links and multiply their weight (0 for self 1 for other) by their count:
eM= apply(edgeMatrix,1,function(x) (paste(x,collapse=" ")))
ueM = unique(eM)
teM = table(eM)[ueM]
lueM = strsplit(names(teM),split=" ")
mueM = do.call(rbind, lueM)
mueM = apply(mueM,2,as.integer)
mueM[,4] = mueM[,4]*as.integer(teM)
edgeMatrix2 = mueM
edges = edgeMatrix2

rm(edgeMatrix)
rm(edgeMatrix2)
rm(mueM)
rm(teM)
rm(ueM)

### Here's one way to do it.
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

# a few more functions:
#startDay= min(edges[,3])
startDay = min(daysToView)
#totalTime = max(edges[,3])-startDay+1
totalTime = max(daysToView)-startDay+1
fpt = 1
totalFrame = totalTime*fpt

Pos <- function(ti, totalTime) {
  #Calculates position on the timeline
  #Args:
  # ti: the time we want to plot on timeline
  # totalTime: timeline length
  pos <- 2*(ti)/(totalTime) - 1
  return(pos)
}

TimelineTM <- function(ti, events, eventDates, startDay, totalTime, fpt, dateSeq) {
  size <- 1.25
  lines(y=c(-1.4,-1.4), x=c(-1, 1))
  strt = lines(y=c(-1.43,-1.37), x=rep(Pos(0, totalTime),2))
  fnsh = lines(y=c(-1.43,-1.37), x=rep(Pos(totalTime, totalTime),2))  
  lines(y=c(-1.43,-1.37),x=rep(Pos(ti-1/fpt, totalTime),2),lwd=3)
  if(length(events)>0){
    lE = length(events)
    for(j in 1:lE){
      lines(y=c(-1.41,-1.39), x=rep(Pos(eventDates[j]-startDay, totalTime),2)) # day 25
      #text(y=-1.42, x=Pos(eventDates[j]-startDay, totalTime),paste("Dec.",sep=" ",eventDates[j]), pos = 1, cex = size)
      #text(y=-1.42, x=Pos(eventDates[j]-startDay, totalTime),format(dateSeq[eventDates][j],format="%b %d"), pos = 1, cex = size)
    }  
  }
  text(y=-1.42, x=Pos(0, totalTime),format(dateSeq[startDay],format="%b %d %Y"), pos = 1, cex = size)
  auxind = cumsum(daysPerMonth)+1
  auxind = auxind[seq(2,10,2)]
  text(y=-1.42, x=Pos(auxind[1], totalTime),format(dateSeq[auxind[1]],format="%b %d %Y"), pos = 1, cex = size)
  text(y=-1.42, x=Pos(auxind[2], totalTime),format(dateSeq[auxind[2]],format="%b %d %Y"), pos = 1, cex = size)
  text(y=-1.42, x=Pos(auxind[3], totalTime),format(dateSeq[auxind[3]],format="%b %d %Y"), pos = 1, cex = size)
  text(y=-1.42, x=Pos(auxind[4], totalTime),format(dateSeq[auxind[4]],format="%b %d %Y"), pos = 1, cex = size)
  text(y=-1.42, x=Pos(auxind[5], totalTime),format(dateSeq[auxind[5]],format="%b %d %Y"), pos = 1, cex = size)
  text(y=-1.42, x=Pos(totalTime, totalTime),format(dateSeq[startDay+totalTime],format="%b %d %Y"), pos = 1, cex = size)
}



#Event <- function(ti) {
#  eventDates
#events <- c("1. Trayvon Martin killed", "2. Justice Department investigation begins", 
#            "3. \"he would look like Trayvon Martin\"", "4. Zimmerman charged", 
#            "5. Zimmerman pleads not guilty", "6. New evidence released",
#            "7. Zimmerman's wife arrested", "8. New evidence released")
#  index <- which(eventDates <= ti)
#  index <- index[length(index)]
#  text(0, -1.25, events[index], cex = 2.5)
#}

# read in political affiliations:
#domInfo = read.csv("Trayvon_Blogs_validation2.csv")
domainAffils = read.csv("domainAffils.csv")[,-1]
nodeAffils = as.character(domainAffils[match(nodeList,domainAffils[,1]),2])
nodeAffils[which(is.na(nodeAffils))] = "unknown"
affilColors = c("red","blue","green4","dim grey")
names(affilColors) = c("conservative","liberal","moderate","unknown")
vLabelColor = affilColors[nodeAffils]
names(vLabelColor) = nodeList
# manually make balloon juice moderate since r changed the - in it's name to a . 
#vLabelColor["balloon.juice.com"]=affilColors["moderate"]

# tidy up names:
#nchar(nodeList)


# add extra color for domains not posting
topColors = c(topColors,"black")
topNames = c(topNames,"NA")




Counter <- function(ti,KDT) {
  lE = length(events)
  size <- 1.25
  #lines(y=c(-1.4,-1.4), x=c(-1, 1))
  lines(y=c(-.8,-1.35),x=c(1.3,1.3))
  Maxxx = max(apply(KDT,3,rowSums))
  topLevels = apply(KDT,3,rowSums)/Maxxx
  points(x=rep(1.3,dim(KDT)[1])+(seq(0,1,length.out=dim(KDT)[1])/20),y=-1.35+topLevels[,ceiling(ti)]*.55,cex=3,col=topColors[-length(topColors)],pch=16)
  text(x=rep(1.33,dim(KDT)[1])+(seq(0,1,length.out=dim(KDT)[1])/20),y=-1.35+topLevels[,ceiling(ti)]*.55,topNames[-length(topNames)],pos=4,cex=1.3)
  text(x=rep(1.27,2),y=c(-1.35,-.8),as.character(c(0,Maxxx)),pos=2,cex=1.3)
  text(x=1.3,y=-.75,"Post Counter:",pos=3,cex=1.3)
}



#generate the full graph:
#library(scales)
#library(igraph)
library(igraph)

g <- graph.edgelist(as.matrix(edges[,c(1,2)]),directed=TRUE)
E(g)$weight = sqrt(edges[,4]) + .Machine$double.eps
E(g)$color = ifelse(E(g)$weight>.Machine$double.eps,topColors[edges[,5]],rgb(0,0,0,0))
eColorMat = col2rgb(E(g)$color,TRUE)
layout.old <- layout_in_circle(g) # ,params=list(weights=E(g)$weight) 
#layout.old <- layout.circle(g,params=list(weights=E(g)$weight)) 
comms = edge.betweenness.community(g, weights = E(g)$weight,
                                   directed = TRUE, edge.betweenness = TRUE, merges = TRUE,
                                   bridges = TRUE, modularity = TRUE, membership = TRUE)$membership
#layout.old = layout.old[ord$membership,ord$membership]
#ord = match(seq(1,length(V(g)),1),sort(comms,index.return=TRUE)$ix)
#ord = sort(comms,index.return=TRUE)$ix
ord = sort(vLabelColor,index.return=TRUE)$ix


#g=permute.vertices(g, ord) 

#dateDom = apply(dateDom,2,function(x) return(ord[x]))
#DT = apply(DT,2,function(x) return(ord[x]))
nodeList = nodeList[ord]
dateDom = dateDom[ord,]
DT=DT[ord,]
edges[,1] = match(edges[,1],ord)
edges[,2] = match(edges[,2],ord)
#edges[,5] = match(edges[,5],desiredTopics)
vLabelColor = vLabelColor[ord]
g <- graph.edgelist(as.matrix(edges[,c(1,2)]),directed=TRUE)
E(g)$weight = sqrt(edges[,4]) + .Machine$double.eps
E(g)$color = ifelse(E(g)$weight>.Machine$double.eps,topColors[edges[,5]],rgb(0,0,0,0))
eColorMat = col2rgb(E(g)$color,TRUE)
layout.old <- layout_in_circle(g) # ,params=list(weights=E(g)$weight) 
#layout.old <- layout.circle(g,params=list(weights=E(g)$weight)) 

# edit igraph function to not change node label positions depending on label length (lines 315 and 317):
# remove two parts where mult by log of nchar

#trace(plot.igraph,edit=TRUE)

# get rid of .com .net and .org
removeSuf <- function(xxx,dotX){
  xxx[grep(dotX, xxx)] <- gsub(dotX,"",xxx[grep(dotX, xxx)]) #remove ".com"
  return(xxx)
}
nodeList = removeSuf(nodeList,".com")
nodeList = removeSuf(nodeList,".net")
nodeList = removeSuf(nodeList,".org")
nodeList = removeSuf(nodeList,".blogspot")
nodeList = removeSuf(nodeList,"blogs.")
nodeList = removeSuf(nodeList,".typepad")
nodeList = removeSuf(nodeList,".wordpress")
#nodeList = removeSuf(nodeList,".talkingpointsmemo")

# shorten long node names:
lchar = 14
longNodes = which(nchar(nodeList)>lchar)
nodeList[longNodes]=substr(nodeList[longNodes],1,lchar)

lab.locs <- radian.rescale(x=1:length(nodeList), direction=-1, start=0)
E(g)$time = edges[,3]
png(file="RecentVid%03d.png", width=1600,height=900)
#times = seq(from = as.integer(0), to = totalTime, length.out = totalTime/sPF+1)
#times[1] = times[1] + .Machine$double.eps
times=seq(1,fpt*totalTime,1)/fpt
#times = seq(from = 1/fpt, to = totalTime, length.out = totalTime*fpt)
times[length(times)] = times[length(times)]-.Machine$double.eps
specify_topic_names = TRUE # set this to true to put topic labels in the key
topicLabels = c(topicNames[match(desiredTopics, topicIndices)],"other")
for (t in times){
  vSize = 2*sqrt(as.integer(dateDom[,ceiling(t)]))+1
  #vColor = topColors[ifelse(DT[,t]>0,DT[,t],K+1)]
  vColor = topColors[DT[,ceiling(t)]]
  vLabel = nodeList
  vLabelSize = 1.4
  #vLabelSize = .67*dateDom[,ceiling(t)]^(1/4)+.26
  #vLabelSize = .67*dateDom[,ceiling(t)]^(1/4)+.26
  #vLabelSize = .4*sqrt(as.integer(dateDom[,ceiling(t)]))+.01
  #vLabelSize=.8
  #vLabelColor = rep("black",length(nodeList))
  #vLabelColor = ifelse(as.integer(dateDom[,ceiling(t)])>0,"black",rgb(0,0,0,0))
  #vLabelColor = ifelse(as.integer(dateDom[,ceiling(t)])>sort(as.integer(dateDom[,ceiling(t)]),decreasing=TRUE)[11],"black",rgb(0,0,0,0))
  #vLabelColor = "black"
  vLabelColor = vLabelColor
  #vLabelColor = ifelse(,,)
  eColor = ifelse(E(g)$time==ceiling(t),E(g)$color,ifelse(E(g)$time==ceiling(t-1),apply(eColorMat,2,function(x) return(rgb(x[1],x[2],x[3],x[4]*.5,maxColorValue=max(eColorMat)))),ifelse(E(g)$time==ceiling(t-2),apply(eColorMat,2,function(x) return(rgb(x[1],x[2],x[3],x[4]*.5^2,maxColorValue=max(eColorMat)))),rgb(0,0,0,0))))
  plot(g,layout=layout.old, vertex.size = vSize, vertex.color=vColor,
       vertex.label=vLabel, edge.color = eColor, edge.width=2*E(g)$weight,
       edge.arrow.size=1, vertex.label.cex=vLabelSize, vertex.label.dist=1,
       vertex.label.degree=lab.locs, vertex.label.color = vLabelColor
       ,xlim = c(-1.08, 1.08), ylim = c(-1.23, 1.23),rescale=FALSE,asp=9/16, margin=-.1
       #,asp=9/16,margin=.05 #,rescale=FALSE
  )
  legend(x=-1.5, y=-.7+(length(topNames)-1)*.075, ifelse(rep(specify_topic_names, length(topicLabels)),topicLabels,topNames), pch=21,col="#777777", pt.bg=topColors, 
         pt.cex=2, cex=1.3, bty="o", ncol=1,title="Theme:",title.col="black")
  #legend(x=-1.5, y=-.7+(length(topNames)-1-1)*.075, topNames[-length(topNames)], pch=21,col="#777777", pt.bg=topColors[-length(topNames)], 
  #       pt.cex=2, cex=1.3, bty="o", ncol=1,title="Theme:",title.col="black")
  legend(x=-1.5, y=-.95, names(affilColors),text.col=affilColors, #, col="white", pch=21,pt.cex=2,
         cex=1.3, bty="o", ncol=1,title="Political Ideology:",title.col="black")
  #legend(x=1.3, y=-.8, topNames, pch=21,col="#777777", pt.bg="black", 
  #       pt.cex=2, cex=1.2, bty="n", ncol=1)
  
  #ttl = ifelse(daysToView[ceiling(t)]%in%eventDates,as.character(events[match(daysToView[ceiling(t)],eventDates)]),format(dateList[ceiling(t)],format="%B %d %Y"))
  ttl = ifelse(daysToView[ceiling(t)]%in%eventDates,as.character(events[match(daysToView[ceiling(t)],eventDates)]),ifelse(daysToView[ceiling(t)]%in%(eventDates+1),as.character(events[match(daysToView[ceiling(t)],(eventDates+1))]),ifelse(daysToView[ceiling(t)]%in%(eventDates+2),as.character(events[match(daysToView[ceiling(t)],(eventDates+2))]),format(dateList[ceiling(t)],format="%B %d %Y"))))
  #ttl = ifelse(daysToView[ceiling(t)]%in%eventDates,as.character(events[match(daysToView[ceiling(t)],eventDates)]),format(dateList[ceiling(t)],format="%B %d %Y"))
  title(ttl,cex.main=2.8)
  #text(0, 1.25,"Conservative", pos = 1, cex = 2)
  #text(-1.2, -.55,"Moderate", pos = 1, cex = 2)
  #text(.65, -1,"Liberal", pos = 1, cex = 2)
  TimelineTM(t, events, eventDates, startDay, totalTime, fpt, dateSeq)
  #Counter(t,KDT)
  #Event(t)
  #lines(y=c(-1.25,-1.35), x=rep(Pos(t, totalTime), 2), lwd = 3)
  #legend(1,-1.1, paste(paste("Dec.",eventDates)[-c(1,length(events))],sep=": ",events[-c(1,length(events))]), pch = ".", 
  #        cex = 1,col="black") # , col = c("black", "blue", "red", "green", "black"),pt.bg = c("black", "blue", "red", "green", "transparent"),
}
dev.off()
dev.off()
dev.off()



# This returns the count of posts on each node on each day, in days to view:
getPostCounts_dt <- function(data, dateList, daysToView, nodeList, domainList, linkList){
  dateDom = matrix(table(data[,2],data[,1]),nrow=length(unique(data[,2])))
  if(dim(dateDom)[2]<length(daysToView)){
    cols2add = matrix(0,nrow=dim(dateDom)[1],ncol=length(daysToView)-dim(dateDom)[2])
    dateDom = cbind(dateDom,cols2add)
    colsNames = match(dateList[which(!dateList[daysToView]%in%sort(unique(data$date)))],dateList)
    newColNames = c(match(sort(unique(data$date)),dateList),colsNames)
    od = sort(newColNames,index.return=TRUE)$ix
    dateDom = dateDom[,od]
  }
  if (length(unique(data$domain))<length(nodeList)){
    rows2add = matrix(0,nrow=length(which(!linkList%in%domainList)),ncol=T)
    rownames(rows2add) = linkList[which(!linkList%in%domainList)]   
    colnames(rows2add) = as.character(dateList)
    dateDom = rbind(dateDom,rows2add)
    rownames(dateDom) = c(domainList,rownames(rows2add))
    dateDom = dateDom[sort(rownames(dateDom)),]
    colnames(dateDom) = as.character(dateList)  
  }
  return(dateDom)
}

# Read in data:
textNetwork2 = textNetwork
textNetwork = textNetwork2
topicAssignments = as.character(sample(size=dim(textNetwork)[1], 1:10, replace=TRUE))

# Specify times, nodes, and topics to view:
dateList = sort(unique(textNetwork$dates))
timesToView = c(1,2,3)
nodeList = sort(unique(c(textNetwork$domains, unlist(strsplit(textNetwork$links, split=" ")))))
nodesToView = c(2,7,5,4,34,62,100,12,53,55,1)
topicList = as.character(sort(unique(topicAssignments)))
topicsToView = c(2,5,8)

# Create function to make a visualization of the network over time,
# showing when nodes are active, when they link to each other, 
# what they post about and what community they are in:
visualize_blog_network <- function(textNetwork, topicAssignments, timesToView, nodesToView, topicsToView){
  
  # Subset data to the dates to view:
  #timeList = sort(unique(textNetwork$dates))
  #topicTimeIndices = which(textNetwork$dates %in% timeList[timesToView])
  #textNetwork = textNetwork[topicTimeIndices]
  
  # Get vectors of dates, domains, links for full data set:
  #timeList = sort(unique(textNetwork$dates))
  #domainList = sort(unique(textNetwork$domains))
  #linkList = sort(unique(unlist(lapply(textNetwork$links, function(n) return(strsplit(n, " "))))))
  #nodeList = sort(unique(c(domainVec, linkVec)))
  #topicList = sort(unique(topicAssignments))
  
  # Subset data to the dates to view:
  #topicPostIndices = which(topicAssignments %in% topicList[topicsToView])
  #topicTimeIndices = which(textNetwork$dates %in% timeList[timesToView])
  #topicNodeIndices = which(unlist(lapply(1:dim(textNetwork)[1], function(n) return(any(nodeList[nodesToView]%in%c(textNetwork$domains,unlist(strsplit(textNetwork$links, " "))))))))
  #nodesToViewChar = sort(unlist(textNetwork$domains))[nodesToView]
  #indices_to_view = which((topicAssignments %in% sort(unique(topicAssignments))[topicsToView]) & (textNetwork$dates %in% sort(unique(textNetwork$dates))[timesToView]) & unlist(lapply(1:dim(textNetwork)[1], function(n) return(any(c(textNetwork$domains[n], unlist(strsplit(textNetwork$links, " "))) %in% nodesToViewChar)))))
  indices_to_view = which((topicAssignments %in% sort(unique(topicAssignments))[topicsToView]) & (textNetwork$dates %in% sort(unique(textNetwork$dates))[timesToView]) & (textNetwork$domains %in% sort(unique(textNetwork$domains))[nodesToView]))
  textNetwork = textNetwork[indices_to_view,]
  topicAssignments = topicAssignments[indices_to_view]
  
  # Get vectors of dates, domains, links for subset of data set:
  timeVec = sort(unique(textNetwork$dates))
  domainVec = sort(unique(textNetwork$domains))
  linkVec = sort(unique(unlist(lapply(textNetwork$links, function(n) return(strsplit(n, " "))))))
  nodeVec = sort(unique(c(domainVec, linkVec)))
  topicVec = sort(unique(topicAssignments))
  
  # Collect counts for every node at each time and put in matrix:
  postCounts_kdt = array(0, dim=c(length(topicVec),length(nodeVec),length(timeVec)), dimnames=list(topicVec, nodeVec, timeVec))
  #postCounts_dt = matrix(0, nrow=length(nodeVec), ncol=length(timeVec), dim=list(nodeVec, timeVec))
  #postCountsDF = as.data.frame(table(textNetwork$dates, textNetwork$domains))
  #postCountsDFNonZero = postCountsDF[postCountsDF$Freq>0,]
  #for (i in 1:dim(textNetwork)[1]){
  #  postCounts_dt[postCountsDFNonZero$Var2[i], postCountsDFNonZero$Var1[i]] = postCountsDFNonZero$Freq[i]
  #}
  for (i in 1:dim(textNetwork)[1]){
    postCounts_kdt[topicAssignments[i], textNetwork$domains[i], textNetwork$dates[i]] = postCounts_kdt[topicAssignments[i], textNetwork$domains[i], textNetwork$dates[i]] + 1
  }
  postCounts_dt = apply(postCounts_kdt, 3, colSums)
  
  # Collect edges matrix with columns sender, receiver, time, weight, topic:
  linksArray <- array(0, dim=c(length(nodeVec),length(nodeVec),length(timeVec)), dimnames=list(nodeVec,nodeVec,timeVec))
  # Make links matrix (with edge weights proportionate to link multiplicity):
  links = lapply(textNetwork$links, function(n) return(unlist(strsplit(n, " "))))
  domains = textNetwork$domains
  dates = textNetwork$dates
  for (i in 1:length(links)){
    linksArray[ which(nodeVec==unlist(domains[[i]])) , which(nodeVec %in% unlist(links[[i]])) , which(timeVec==unlist(dates[[i]])) ] = linksArray[ which(nodeVec==unlist(domains[[i]])) , which(nodeVec %in% unlist(links[[i]])) , which(timeVec==unlist(dates[[i]])) ] + 1
  }
  edges_srtwk = do.call(rbind, lapply(1:length(timeVec), function(t){
    edges_chunk = cbind(matrix(rep(rep(match(names(which(postCounts_dt[,t]>0)), nodeVec),
                                       postCounts_dt[which(postCounts_dt[,t]>0),t]),2),ncol=2), 
                        matrix(t, nrow=sum(postCounts_dt[,t]), ncol = 1), 
                        matrix(1, nrow=sum(postCounts_dt[,t]), ncol = 1),
                        matrix(0, nrow=sum(postCounts_dt[,t]), ncol = 1));                                                                                   
    if (sum(linksArray[,,t]) > 0){
      edges_chunk = rbind(edges_chunk, cbind(apply(get.edgelist(graph.adjacency(linksArray[,,t])),2,function(c) return(match(c, nodeVec))), 
                                             rep(t, sum(linksArray[,,t])), rep(1, sum(linksArray[,,t])), 
                                             rep(1, sum(linksArray[,,t]))))
    }
    return(edges_chunk)
  }
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
  V(g)$size = as.integer(rowSums(matrix(postCounts_dt[V(g)$name,], nrow=length(nodeVec))))+1
  V(g)$label.size = 1.4
  E(g)$weight = as.numeric(ifelse(edges_srtwk[,1]==edges_srtwk[,2],0,edges_srtwk[,4]))
  E(g)$color = ifelse(edges_srtwk[,5]=="0","transparent","blue")
  plot(g,layout=layout_overall, vertex.size = V(g)$size, 
       vertex.label="", edge.width=.1+E(g)$weight, edge.color = E(g)$color,
       edge.arrow.size=.2, vertex.label.cex=V(g)$label.size,
       xlim = c(-1.08, 1.08), ylim = c(-1.23, 1.23),rescale=FALSE,asp=9/16, margin=-.1
  )
  # Add labels:
  la = layout_overall
  x = la[,1]*1.4
  y = la[,2]*1.4
  #create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
  angle = ifelse(atan(-(la[,1]/la[,2]))*(180/pi) < 0,  90 + atan(-(la[,1]/la[,2]))*(180/pi), 270 + atan(-la[,1]/la[,2])*(180/pi))
  #Apply the text labels with a loop with angle as srt
  for (i in 1:length(x)) {
    text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, pos=NULL, cex=.7, col="black", srt=angle[i], xpd=T)
  }
  
  # plot graph at each time:
  for (t in 1:length(timeVec)){
    g <- graph.edgelist(as.matrix(edges_srtwk[,c(1,2)]),directed=TRUE)
    V(g)$name = nodeVec[as.numeric(V(g)$name)]
    V(g)$size = as.integer(rowSums(matrix(postCounts_dt[V(g)$name, ceiling(t)], nrow=length(nodeVec))))+1
    V(g)$label.size = 1.4
    E(g)$weight = as.numeric(ifelse((edges_srtwk[,1]==edges_srtwk[,2]) | (edges_srtwk[,3]!=ceiling(t)),0,edges_srtwk[,4]))
    E(g)$color = ifelse((edges_srtwk[,5]=="0") | (edges_srtwk[,3]!=ceiling(t)),"transparent","blue")
    plot(g,layout=layout_overall, vertex.size = V(g)$size, 
         vertex.label="", edge.width=.1+E(g)$weight, edge.color = E(g)$color,
         edge.arrow.size=.2, vertex.label.cex=V(g)$label.size,
         xlim = c(-1.08, 1.08), ylim = c(-1.23, 1.23),rescale=FALSE,asp=9/16, margin=-.1
    )
    # Add labels:
    la = layout_overall
    x = la[,1]*1.4
    y = la[,2]*1.4
    #create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
    angle = ifelse(atan(-(la[,1]/la[,2]))*(180/pi) < 0,  90 + atan(-(la[,1]/la[,2]))*(180/pi), 270 + atan(-la[,1]/la[,2])*(180/pi))
    #Apply the text labels with a loop with angle as srt
    for (i in 1:length(x)) {
      text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, pos=NULL, cex=.7, col="black", srt=angle[i], xpd=T)
    }
  }
  
}

a = lapply(textNetwork$words, function(n) return(table(unlist(strsplit(n, split=" ")))))
b = lapply(a, function(m) return(names(m[topicNames][!is.na(m[topicNames])])[which.is.max(m[topicNames][!is.na(m[topicNames])])]))
c = as.character(match(unlist(lapply(b, function(l) return(ifelse(identical(l, character(0)),"other",l)))), topicNames))
visualize_blog_network(textNetwork, c, timesToView, nodesToView, 1:10)
visualize_blog_network(textNetwork, topicAssignments, timesToView, nodesToView, topicsToView)






# Other plot details:
legend(x=-1.5, y=-.7+(length(topNames)-1-1)*.075, topNames[-length(topNames)], pch=21,col="#777777", pt.bg=topColors[-length(topNames)], 
       pt.cex=2, cex=1.3, bty="o", ncol=1,title="Theme:",title.col="black")
legend(x=-1.5, y=-.95, names(affilColors),text.col=affilColors, #, col="white", pch=21,pt.cex=2,
       cex=1.3, bty="o", ncol=1,title="Political Ideology:",title.col="black")
#legend(x=1.3, y=-.8, topNames, pch=21,col="#777777", pt.bg="black", 
#       pt.cex=2, cex=1.2, bty="n", ncol=1)

#ttl = ifelse(daysToView[ceiling(t)]%in%eventDates,as.character(events[match(daysToView[ceiling(t)],eventDates)]),format(dateList[ceiling(t)],format="%B %d %Y"))
ttl = ifelse(daysToView[ceiling(t)]%in%eventDates,as.character(events[match(daysToView[ceiling(t)],eventDates)]),ifelse(daysToView[ceiling(t)]%in%(eventDates+1),as.character(events[match(daysToView[ceiling(t)],(eventDates+1))]),ifelse(daysToView[ceiling(t)]%in%(eventDates+2),as.character(events[match(daysToView[ceiling(t)],(eventDates+2))]),format(dateList[ceiling(t)],format="%B %d %Y"))))
#ttl = ifelse(daysToView[ceiling(t)]%in%eventDates,as.character(events[match(daysToView[ceiling(t)],eventDates)]),format(dateList[ceiling(t)],format="%B %d %Y"))
title(ttl,cex.main=2.8)
#text(0, 1.25,"Conservative", pos = 1, cex = 2)
#text(-1.2, -.55,"Moderate", pos = 1, cex = 2)
#text(.65, -1,"Liberal", pos = 1, cex = 2)
TimelineTM(t)
#Counter(t,KDT)
#Event(t)
#lines(y=c(-1.25,-1.35), x=rep(Pos(t, totalTime), 2), lwd = 3)
#legend(1,-1.1, paste(paste("Dec.",eventDates)[-c(1,length(events))],sep=": ",events[-c(1,length(events))]), pch = ".", 
#        cex = 1,col="black") # , col = c("black", "blue", "red", "green", "black"),pt.bg = c("black", "blue", "red", "green", "transparent"),



# specifications:
# data = read.csv("janData.csv") # commented this out recently


#linkList = as.character(read.csv("linkList.csv")[,2])
L = 81 # number of unique links
linkList = names(data[,(1+1+1+1):(1+1+1+81)])
#vocab = as.character(read.csv("vocab.csv")[,2])
L = 81 # number of unique links
V = dim(data)[2] - 3 - L
rm(data)
# get entire year of posts, remove long posts for teague, 
posts = as.character(c())
for (i in 1:12){
  # datai = read.csv(paste(names(daysPerMonth)[i],sep="","Data.csv")) # commented this out recently
  datai = datai[-which(rowSums(datai[,(3+L+1):(3+L+V)])>3000),]
  datai = datai[,-seq((3+L+1),(3+L+V),1)]
  dataim = as.matrix(datai)
  dataim = apply(dataim,2,as.character)
  dataim = t(dataim)
  posts = c(posts,dataim)
}
posts = t(matrix(posts,nrow = (3+L)))
rm(datai)
rm(dataim)


# read in topic names and top words:
#topicWords = as.matrix(read.csv("Run16Words.csv"))
topicWords = topicWords[,-1]
topicWords = topicWords[,sort(as.integer(as.matrix(topicWords[1,])),index.return=TRUE)$ix]

#topicID = names(topicWords[1,])
#names(topicID) = topicWords[1,]
#topicCounts = as.integer(topicWords[dim(topicWords)[1],])
#topicWords = topicWords[-1,]
#topicWords = topicWords[-dim(topicWords)[1],]
#topicWords = t(topicWords)
#write.csv(topicWords,file="topicWords.csv")
#write.csv(topicWords,file="topicWords.csv")

#topicID = as.character(sort(unique(topAssign)))
#names(topicID) = topicID
#topicID[c(19,52,60)]=c("Presidential Election","Guns and Self Defense","Trayvon Martin Shooting")

topNames = colnames(topicWords)

