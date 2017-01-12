####------------------DSL Project: Opinion Mining---------------------####
####---------Code for Sentiment Mapping using NLP Parser--------------####

#volatiles = new.env(parent=emptyenv())
library(coreNLP)

load("score_cleaned.Rdata")
reviews=read.csv("Review_rating_218778.csv")
str(reviews)
#reviews=read.csv("reviews.csv")
#reviews=reviews[,-1]

num_reviews = nrow(reviews)

aspects = read.csv("aspects.csv")
num_aspects = length(aspects)
aspects_keywords = list()


for(k in 1:num_aspects){
  ai = as.vector(aspects[,k])
  ai = unique(ai)
  ai = ai[ai != ""]
  aspects_keywords = append(aspects_keywords,list(ai))
}
names(aspects_keywords) = names(aspects)

libLoc = paste0(system.file("extdata",package="coreNLP"),"/stanford-corenlp-full-2015-12-09")
initCoreNLP(libLoc,mem = "6g")

start_index=1
end_index = 94
aspect_score = matrix(0,nrow = 1,ncol = num_aspects)
aspect_score = aspect_score_id(1,94,reviews,score_cleaned,aspect_score)
# spliting service and business service as two aspects
# Due to high corelation, it has not been considered in vocabulary
aspect_score_b_service = aspect_score[6] - 10
aspect_score[6] = aspect_score[6] - aspect_score_b_service
aspect_score = append(aspect_score, aspect_score_b_service)
print(aspect_score)
write.csv(aspect_score, "aspect_score.csv")


