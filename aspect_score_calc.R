####------------------DSL Project: Opinion Mining---------------------####
####---------Code for Sentiment Mapping using NLP Parser--------------####

aspect_score_id = function(start_index,end_index,reviews,score_cleaned,aspect_score){
 # denom = matrix(0,nrow=1,ncol = num_aspects)
  for(i in start_index:end_index){
    print(i)
    #i=7
    if(reviews$review_content[i] == ""){
      next
    }else{
      str=as.character(reviews$review[i])
      #very important - str should be character not any other format
      annotation = annotateString(text = str,format = c("obj"))
      dependency=getDependency(annotation)
      for(k in 1:num_aspects){
        keywords = aspects_keywords[[k]]
        word_list = c()
        first_hop_words = c()
        first_hop_index = c()
        second_hop_words = c()
        second_hop_index = c()
        index_explored = c()
        index_unexplored = c(1:nrow(dependency))
        
        governor_match = which(dependency$governor %in% keywords)
        first_hop_words = append(first_hop_words,dependency$dependent[governor_match])
        first_hop_index = append(first_hop_index,dependency$depIndex[governor_match])
        index_explored = append(index_explored,governor_match)
        
        dependent_match = which(dependency$dependent %in% keywords)
        first_hop_words = append(first_hop_words,dependency$governor[dependent_match])
        first_hop_index = append(first_hop_index,dependency$govIndex[dependent_match])
        index_explored = append(index_explored,dependent_match)
        
        first_hops = cbind(first_hop_words,first_hop_index)
        first_hops = unique(first_hops)
        if(length(which(is.na(first_hops[,2]))) > 0){
          first_hops = first_hops[-which(is.na(first_hops[,2])),]
        }
        
        if(!(is.matrix(first_hops))){
          first_hops = t(as.matrix(first_hops))
        }
        index_unexplored = index_unexplored[-which(index_unexplored %in% index_explored)]
        if(nrow(first_hops) > 0){
          for(l in 1:nrow(first_hops)){
            for(j in index_unexplored){
              if((first_hops[l,1] == dependency$governor[j]) & (as.numeric(first_hops[l,2]) == as.numeric(dependency$govIndex[j]))){
                second_hop_words = append(second_hop_words,dependency$dependent[j])
                index_unexplored = index_unexplored[-which(index_unexplored %in% j)]
              }
              if((first_hops[l,1] == dependency$dependent[j]) & (as.numeric(first_hops[l,2]) == as.numeric(dependency$depIndex[j]))){
                second_hop_words = append(second_hop_words,dependency$governor[j])
                index_unexplored = index_unexplored[-which(index_unexplored %in% j)]
              }
            }
          }
        }
        word_list = append(word_list,c(first_hops[,1],second_hop_words))
        senti_indices = match(word_list,score_cleaned[,3])
        senti_indices = senti_indices[-which(is.na(senti_indices))]
        if(length(senti_indices)>0){
          objective_scores = c()
          for(index in senti_indices){
            word_score = (as.numeric(score_cleaned[index,1]) - as.numeric(score_cleaned[index,2]))
            objective_scores = append(objective_scores,word_score)
          }
          ai_score = sum(objective_scores)
           
          aspect_score[1,k] = aspect_score[1,k] + ai_score/length(senti_indices)
          
        }
      }
    }
  }
 
  return (aspect_score)
}