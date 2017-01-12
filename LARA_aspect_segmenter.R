####------------------DSL Project: Opinion Mining----------------------------------####
####---------Code for Latent Aspect Rating Regression (Aspect segmenter)-----------####

library(tm)
#library(SnowballC)
#reviews = Review_rating_218778
reviews = read.csv(file.choose())
reviewid = seq(1:nrow(reviews))
reviews = cbind(reviewid, reviews)
#reviews=reviews[,-1]
stopwords = read.csv("stopwords.csv")

aspects = list(value = c("range","price","value","quality","worth"), room = c("room","suite","view","bed"), 
               location = c("location", "traffic","minute","resturant"), cleanliness = c("clean","dirty","maintain","smell"),
                            checkin = c("stuff","check","help","reservation"), service = c("service","food","breakfast","buffet"),
                            business_service = c("wifi","computer","internet","business","centre"))
#aspects = read.csv("aspects.csv")
#aspects = as.list(aspects)
num_aspects = length(aspects)

#for(i in 1:nrow(reviews)){
sentences = c()
review_id = c()

num_reviews = nrow(reviews)
for(i in 1:num_reviews){
  d = as.character(reviews[i,3])
  d = tolower(d)
  d = gsub("[^[:alnum:][:space:].]", "", d)
  X = strsplit(d, "\\.")[[1]]
  remove = which(X == "")
  if(length(remove)!=0){
    X = X[-remove]
  }
  review_id = append(review_id,rep(i,length(X)))
  sentences = append(sentences,X)
}

write.csv(sentences,"sentences.csv")
myCorpus = Corpus(VectorSource(sentences))
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myStopwords = as.character(stopwords$stopwords)
myCorpus = tm_map(myCorpus, removeWords, c(myStopwords,stopwords('english')))
#myCorpus = tm_map(myCorpus, stemDocument)
myCorpus = tm_map(myCorpus, PlainTextDocument)
myDtm = DocumentTermMatrix(myCorpus, control = list(wordLengths=c(2,Inf), 
                                                     weighting=weightTf))
C = apply(myDtm,2,sum)

myDtm_matrix=as.matrix(myDtm)
#write.csv(myDtm_matrix,"dtm.csv")

num_words = ncol(myDtm_matrix)
chi_square = matrix(0,nrow = num_words, ncol = num_aspects)
#-----------------------------------#

#the following code for each hotel

for(iteration in 1:3){
  sentence_aspect = c()
  for(m in 1:length(sentences)){
    tokens = strsplit(sentences[m], " ")[[1]]
    tokens = tokens[!tokens %in% myStopwords]
    count = matrix(0,nrow = num_aspects, ncol = 1)
    for(k in 1:num_aspects){
      count[k] = sum(table(tokens[tokens %in% as.vector(aspects[[k]])]))   
    }
    a = which.max(count)
    sentence_aspect = append(sentence_aspect,a)
  }
  table(sentence_aspect)

  for(k in 1:num_aspects){
    ai = myDtm[sentence_aspect == k,]
    ai_bar = myDtm[sentence_aspect != k,]
    C1 = apply(ai,2,sum)
    C2 = C - C1
    for(word in 1:num_words){
      v3 = as.vector(ai[,word])
      C3 = length(which(v3==0))
      v4 = as.vector(ai_bar[,word])
      C4 = length(which(v4==0))
      chi_square[word,k] = (C[word]*(C1[word]*C4 - C2[word]*C3)^2)/
        ((C1[word]+C3)*(C2[word]+C4)*(C[word])*(C3+C4))
    }
  }

  for(k in 1:num_aspects){
    p = order(chi_square[,k],decreasing = TRUE)
    words_add  = colnames(myDtm[,p[1:5]])
    aspects[[k]] = append(as.vector(aspects[[k]]),words_add)
    aspects[[k]] = unique(aspects[[k]])
    aspects[[k]] = aspects[[k]][aspects[[k]] != ""]
  }
}
list_Wd = list()
for(review in 1:num_reviews){
  review_corpus = myDtm[review_id == review,]
  Wd = matrix(0,nrow = num_aspects,ncol = num_words)
  for(k in 1:num_aspects){ 
    Ai = review_corpus[sentence_aspect == k,]
    word_freq = apply(Ai,2,sum)
    if(sum(word_freq) == 0){
      Wd[k,] = 0
    }
    else
      {
        norm_word_freq = word_freq/sum(word_freq)
        Wd[k,] = norm_word_freq
      }
  }
  list_Wd = append(list_Wd,list(Wd))
}
  
#vocab size
dim(list_Wd[[3]])
