# Pre process blog posts auxiliary



# Load packages: 
library(rjson)
library(magrittr)
library(stringr)
#library(quanteda)
#library(data.table)
#library(dplyr)
#library(parallel)
#library(qdap)
#library(tm)

  
# Functions:
  
# removes "" link from a post with other links but leaves it for a post with no links:
removeBlankLinks <- function(linkrow){
  if(length(linkrow)>1){
    return(linkrow[which(linkrow!="")])
  }
  else{
    return(linkrow)
  }
}
  
# removes links from a post to domains not in the domain list:
removeExternalLinks <- function(linkrow,domainlist){
  domainindices = which(linkrow%in%domainlist)
  if(length(domainindices)>0){
    return(linkrow[domainindices])
  }
  else{
    return("")
  }
}
  
# removes self links from a post (links that are equivilent to the post's domain)
removeSelfLinks <- function(linkindex,linksvector,domainvector){
  link = linksvector[[linkindex]]
  domainindex = which(!link==domainvector[linkindex])
  if(length(domainindex)>0){
    return(link[domainindex])
  }
  else{
    return("")
  }
}
  
# converts raw text (with new lines, punctuation, etc) to a clean string
# of lower case words (with no numbers) separated by spaces:
rawTextToText <- function(RawText){
  Text = RawText %>% str_replace_all("\n"," ") # replace new lines with a space
  #Text = Text %>% str_replace_all(" ' ", " ") # replace non-nested (commas) with a space
  Text = Text %>% str_replace_all("[^[:alpha:] ]","")  # remove non (alphabet or space or apostrophe). 
  Text = Text %>% str_replace_all("[:space:]+"," ") # replace any number of spaces with a single space
  Text = Text %>% str_trim() # remove white space from beginning and end of string
  Text = Text %>% tolower() # change upper case to lower case
  return(Text)
}
  
# removes words from documents which are not in the vocabulary:
removeNonVocabWords <- function(WordsList,Vocabulary){
  words1 = lapply(WordsList, function(x) match(unlist(x),Vocabulary)) # give words numeric id in vocab
  words2 = lapply(words1,function(x) return( x[!is.na(x)])) # remove words not in vocab
  words3 = lapply(words2, function(x) return(Vocabulary[x])) # map back to words. total words now: 35274853. unique: 34973
  return(words3)
}
  
