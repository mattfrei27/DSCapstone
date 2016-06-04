# This file reads in, cleans and samples from the original files.
# It also creates n-grams from unigrams to 4-grams and ultimately
# creates a data.table that can be used for word prediction.

setwd('Z:/Coursera DS Capstone')
ptm <- proc.time()

library(tm)
library(stringi)
library(ggplot2)
library(ggthemes)
library(slam) # Sparse vectors and matrices useful to calculate rowsums of document-term matrix
library(RWeka)

# Read raw files
blogs <- readLines('./data/en_US/en_US.blogs.txt',skipNul=TRUE)
# There are substitue characters in the news corpus. Read them in raw/binary mode:
news.con <- file('./data/en_US/en_US.news.txt','rb')
news <- readLines(news.con)
close(news.con)
tweets <- readLines('./data/en_US/en_US.twitter.txt',skipNul=TRUE)

# Sample from the raw text and convert non ASCII characters to ASCII.
set.seed(98324154)
blogs.sample <- stringi::stri_trans_general(sample(blogs,200000),"latin-ascii")
news.sample <- stringi::stri_trans_general(sample(news,200000),"latin-ascii")
tweets.sample <- stringi::stri_trans_general(sample(tweets,200000),"latin-ascii")

#Create corpus

blogs.corpus <- VCorpus(VectorSource(blogs.sample))
meta(blogs.corpus,'source') <- 'Blogs'

news.corpus <- VCorpus(VectorSource(news.sample))
meta(news.corpus,'source') <- 'News'

tweets.corpus <- VCorpus(VectorSource(tweets.sample))
meta(tweets.corpus,'source') <- 'Tweets'

corpus <- c(blogs.corpus,news.corpus,tweets.corpus)

profanity <- read.csv('Terms-to-Block.csv',header = FALSE, skip=4,col.names=c('junk','words')) #List of profane words taken from: http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/
profanity$words <- gsub(',','',profanity$words)

rm(list = c('blogs','news','tweets','blogs.sample','news.sample',
            'tweets.sample','blogs.corpus','news.corpus','tweets.corpus'))
gc()


corpus <- cleanCorpus(corpus,profanity)


# n-grams
options(mc.cores=4)

uniGramTok <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
biGramTok <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
triGramTok <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadGramTok <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

unigrams <- DocumentTermMatrix(corpus, control = list(tokenize = uniGramTok, bounds =  list(global = c(10, Inf))))
bigrams <- DocumentTermMatrix(corpus, control = list(tokenize = biGramTok, bounds =  list(global = c(10, Inf))))
trigrams <- DocumentTermMatrix(corpus, control = list(tokenize = triGramTok, bounds =  list(global = c(10, Inf))))
quadgrams <- DocumentTermMatrix(corpus, control = list(tokenize = quadGramTok, bounds =  list(global = c(10, Inf))))

unigrams.dt <- getFreqTable(unigrams,1)
bigrams.dt <- getFreqTable(bigrams,2)
trigrams.dt <- getFreqTable(trigrams,3)
quadgrams.dt <- getFreqTable(quadgrams,4)
prediction.dt <- rbindlist(list(unigrams.dt,bigrams.dt,
                                trigrams.dt,quadgrams.dt))


#Keep only the 5 most frequent n-grams by size and prefix. 
setkey(prediction.dt,ngram.size,prefix)
sub.pred <- prediction.dt[,.SD[head(order(-freq),5)],by=c('ngram.size','prefix')]


save(list='sub.pred',file='sub.pred3.RData',compress = 'bzip2')
# load('sub.pred.RData')

proc.time() - ptm





# # Junkyard



# 
# 
# freq.words <- findFreqTerms(unigrams, 2000)
# rs <- slam::col_sums(unigrams[,freq.words],na.rm=TRUE)
# unigram.freq <- data.frame(unigram=freq.words, frequency=rs)
# 
# ggplot(unigram.freq, aes(x=reorder(unigram, frequency), y=frequency)) +
#   geom_bar(stat = "identity",fill='#990000') +
#   coord_flip() +
#   xlab("Unigram") +
#   ylab("Frequency") +
#   labs(title = "Top Unigrams by Frequency") +
#   theme_solarized(light = FALSE) +
#   theme(axis.text=element_text(size=12),
#         title=element_text(size=14,face="bold")
#        )
# 
# 
# 
# 
# 
# 
# # Potential Additional Transformations
# # tm_map(corpus, stemDocument)
# 
# 


# dtm <- DocumentTermMatrix(corpus)
# 
# # Explore the DT Matrix
# inspect(dtm[1:10,1:10])
# findFreqTerms(dtm,1000)
# findAssocs(dtm, "house", 0.5)
# 
# 
# rs <- slam::row_sums(dtm,na.rm=TRUE)
# hist(rs)
# # The distribution of words is extremely skewed.

# Remove terms appearing is less then 0.1% of documents
# sub_dtm <- removeSparseTerms(dtm, 0.999)

# rs <- slam::row_sums(sub_dtm,na.rm=TRUE)
# hist(rs)



# 
# 
# options(mc.cores=4)
# #This function includes n-grams of size 1 and 2 by default:
# n.g.dtm <- DocumentTermMatrix(corpus, control=list(tolower = TRUE,
#                                                    stripWhitespace = TRUE,
#                                                    removePunctuation = TRUE,
#                                                    removeNumbers = TRUE,
#                                                    stopwords = profanity$words,
#                                                    tokenize = NGramTokenizer))
# findFreqTerms(n.g.dtm,500)
# 
# 
# #Custom tokenizer to get bigrams only
# BigramTokenizer <-
#   function(x)
#     unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
# 
# bg.dtm <- DocumentTermMatrix(corpus, control = list(tolower = TRUE,
#                                                     stripWhitespace = TRUE,
#                                                     removePunctuation = TRUE,
#                                                     removeNumbers = TRUE,
#                                                     stopwords = profanity$words,
#                                                     tokenize = BigramTokenizer))
# findFreqTerms(bg.dtm,500)
# 
