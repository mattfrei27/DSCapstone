# This file contains functions for cleaning data, formatting data.tables for word
# prediction, and a function for prediction.

library(data.table)

cleanCorpus <- function(corpus_in,profanity){
  corpus <- tm_map(corpus_in, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, removeWords, profanity$words)
  tm_map(corpus, stripWhitespace)
}


# Convert DTM to data.frame with prefix, word, and frequency.
getFreqTable <- function(dtm,ngram.size){
  freqs <- sort(slam::col_sums(dtm,na.rm=TRUE),decreasing=TRUE)
  if (ngram.size == 1) {
    data.table(prefix='',word=names(freqs),freq=freqs,ngram.size)
  }
  else {
    # dt <- data.table(gram=names(freqs),cnt=freqs)
    toks <- strsplit(names(freqs),'\\s',perl=TRUE)
    prefix <- sapply(lapply(toks,function(x) x[1:(ngram.size-1)]),paste,collapse = ' ')
    word <- sapply(toks,last)
    data.table(prefix,word,freq=freqs,ngram.size)
  }
}


predictNextNWords <- function(sentence,n=5){
  if(n >5){
    n <- 5
  }
  cleanSentence <- as.character(cleanCorpus(VCorpus(VectorSource(sentence)),profanity = profanity)[[1]])
  cleanSentence <- stringi::stri_trans_general(cleanSentence,"latin-ascii")
  toks <- tail(unlist(strsplit(cleanSentence,'\\s',perl=TRUE)),3)
  pred <- vector(mode='character')
  for (i in 1:length(toks)){
    selectedPrefix <- toks[i:length(toks)]
    prefixStr <- paste(selectedPrefix, collapse = ' ')
    predAtCurLength <- sub.pred[(sub.pred$prefix == prefixStr) & (sub.pred$ngram.size == (length(selectedPrefix) + 1)),word]
    pred <- unique(append(pred,predAtCurLength[!is.na(predAtCurLength)]))
     if(sum(!is.na(pred)) >=n)
      return(data.frame(rank=seq(1,n),prediction=pred[1:n]))
  }
  return(data.frame(rank=seq(1,n),prediction=sub.pred[(sub.pred$prefix == '') & (sub.pred$ngram.size == 1),word][1:n]))
}



# format(object.size(sub.pred),units="Mb")

