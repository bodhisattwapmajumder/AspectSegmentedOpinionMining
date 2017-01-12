####------------------DSL Project: Opinion Mining---------------------####
####---------Code for Document Summarization using LexRank------------####

library(tm)
library(lsa)
library(igraph)

#reviews=read.csv('C:/Users/Prahita/Anaconda/Aspect_tagged_sentences_218778.csv', header=T)
reviews = read.csv(file.choose())
reviews$sentence<- sapply(reviews$sentence, as.character)

#Build corpus corresponding to a particular aspect
aspect_names=c('value', 'room','location','cleanliness','checkin', 'service','business service' )

#select aspect and make subset of reviews corresponding to that aspect
matches=grepl('business service',reviews$aspect)
reviews_sub=reviews[matches,]

#Apply general cleaning operations
corpus= Corpus(VectorSource(reviews_sub$sentence))
corpus = tm_map(corpus, removeWords,stopwords("english"))
corpus=tm_map(corpus, removeNumbers)
corpus= tm_map(corpus, stemDocument)
corpus=tm_map(corpus, stripWhitespace)
corpus=tm_map(corpus, removePunctuation)

##Analyzing term frequency matrix
dtm=TermDocumentMatrix(corpus)
dtm=weightTfIdf(dtm, normalize = TRUE)
#inspect(dtm[10:15,50:55])
findFreqTerms(dtm)

dtm_matrix=as.matrix(dtm)   #convert this to matrix

##Compute similarity between sentences and store in d [nxn] matrix
d=diag(0, nrow=dim(dtm_matrix)[2],ncol=dim(dtm_matrix)[2])  

for(i in 1:ncol(dtm_matrix)){
  for(j in 1:ncol(dtm_matrix)){
    if(i!=j){
      d[i,j]=cosine(dtm_matrix[,i],dtm_matrix[,j])          #if i=j, cosine sim=1, we don't want this
      }
  }
}

####----Convert to graph-----------####

g=graph.adjacency(d, weighted=T)
g

##Compute page rank recursively and display top-5 sentences with highest page rank
##Ignore warnings messages##

for(i in 1:5){
  page=page_rank(g, algo = "power",directed = FALSE, damping = 0.85)
  print(reviews_sub[which(page$vector==max(page$vector)),2])
  g=delete.vertices(g,which(page$vector==max(page$vector)))
}
