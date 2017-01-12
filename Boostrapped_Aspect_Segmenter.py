##DSL Project: Opinion Mining
##Code for Bootstrapped Aspect Segmentation


import json
import pandas as pd
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.corpus import stopwords
import string
#from nltk.stem import PorterStemmer
#stemmer=PorterStemmer()

english_stops = set(stopwords.words('english'))
###Reading data
with open('G:/PGDBA/Opinion Mining/json/218778.json') as data_file:    
    data = json.load(data_file)
    #pprint(data)

##Extracting hotel name and id
#[data['HotelInfo'][x] for x in ("Name", "HotelID")][0]

##Extracting reviews
list_of_reviews=[data['Reviews'][i]['Content'] for i in range(len(data['Reviews']))]
review_ids=[data['Reviews'][i]['ReviewID'] for i in range(len(data['Reviews']))]
reviews=pd.DataFrame({'review_ids':review_ids, 'review_content':list_of_reviews} )
reviews['review_content']=reviews['review_content'].str.lower()

##reviews tokenized in sentences
review_sent=map(sent_tokenize, reviews['review_content'])

#reviews_tokenized_in_words=[]   ##Contains list of lists. [[review1], [[sentence1],.. [w1, w2,..]], ]
#for r in review_sent:
#    ##for 1 review  
#    words=[]
#    for sentence in r:
#        #print(s)
#        words.append(filter(lambda x: (x not in english_stops) & (x not in string.punctuation), 
#                            stemmer.stem(word_tokenize(sentence))))
#    reviews_tokenized_in_words.append(words)   


#Building vocabulary and initializing aspect key words

keywords={'value':['range', 'price','value', 'quality', 'worth'], 
          'room':['room', 'suite', 'view', 'bed'], 
          'location':['location', 'traffic', 'minute', 'restaurant'], 
          'cleanliness':['clean', 'dirty', 'maintain', 'smell'], 
          'checkin':['stuff', 'check', 'help', 'reservation'], 
          'service':['service', 'food', 'breakfast', 'buffet'], 
          'business service':['wifi', 'computer', 'internet', 'business', 'centre']}

aspect_numeric={'value':'1', 'room':'2','location':'3', 'cleanliness':'4','checkin':'5','service':'6','business service':'7',
                '1':'value', '2':'room','3':'location', '4':'cleanliness','5':'checkin','6':'service','7':'business service'}

##Initializing vocabulary

aspect_names=pd.Series(keywords.keys())
word_list=[]
word_list=filter(lambda x: (x not in word_list),sum(keywords.values(), []))
vocab=pd.DataFrame(0,index=aspect_names,columns=word_list, dtype='int')
for e in vocab.columns:
    for k in keywords.keys():
        if(e in keywords[k]):
            vocab.loc[k][e]=1

N_iter=10

for i in range(N_iter+1):
    all_review_list=[]
    W_parent_list=[]
    for r in review_sent:
        one_review_list=[]
        #sent_matrix=pd.DataFrame(0,index=range(len(r)+1), columns=[])     
        W_matrix=pd.DataFrame(0, index=aspect_names, columns=vocab.columns,dtype='int')   
        for sent in r:
            words=filter(lambda x: (not x.isdigit()) &(x not in english_stops) & (x not in string.punctuation),word_tokenize(sent))
            aspect_decision_series=pd.Series(0, index=aspect_names )        
            ##Tagging sentence to aspect
            for k in keywords.keys():
                words_in_aspect_k=filter(lambda x: x in keywords[k], words)
                aspect_decision_series[k]+=len(words_in_aspect_k)
            aspect_assign=aspect_decision_series.loc[ aspect_decision_series==max(aspect_decision_series)].index
            if(len(aspect_assign)!=len(keywords)):   ##delete all sentences not associated with any aspect
                sent_dict={'words':words, 'aspect':aspect_assign,'sentence':sent}
                print(sent_dict)
                one_review_list.append(sent_dict)
            
            for a in aspect_assign:
                words_in_aspect_k=filter(lambda x: (not x.isdigit()) &(x not in english_stops) & (x not in string.punctuation),word_tokenize(sent))
                for w in words_in_aspect_k:
                    if(w not in vocab.columns):        ##augment word in vocab if not present
                        vocab[w]=[0]*len(vocab)
                    vocab.loc[a,w]=(vocab.loc[a][w])+1
                    if(w not in W_matrix.columns):
                        W_matrix[w]=[0]*len(W_matrix)
                    W_matrix.loc[a,w]=(W_matrix.loc[a][w])+1            
            #one_review_list.append(sent_dict)
        
                           
        #all_review_list.append(one_review_list)  
        all_review_list.append(one_review_list) 
        W_parent_list.append(W_matrix)
    
    ###Making sentence_word matrix
    
    sent_word_matrix=pd.DataFrame(0, index=range(len(sum(review_sent,[]))+1), columns=vocab.columns, dtype='int')
    aspect_cols=pd.DataFrame(0, index=range(len(sum(review_sent,[]))+1), columns=['1','2','3','4','5','6','7'], dtype='int')
    sent_word_matrix=pd.concat([sent_word_matrix, aspect_cols],axis=1, join='inner')
    i=0     
    for rev in all_review_list:
        for sent in rev:
            for w in sent['words']:
                sent_word_matrix.loc[i,w]+=1
            for a in sent['aspect']:
                sent_word_matrix.loc[i,aspect_numeric[a]]=1
            i=i+1
       
               
    ##Appending seedwords

    for k in keywords.keys():
        
        m=pd.Series(map(chisq_test, vocab.columns,[aspect_numeric[k]]*len(vocab.columns)))
        m.sort(ascending=False)
        #print(m.value_counts())
        #print("****")
        indexes=m[1:6].index
        for i in indexes:
            print(vocab.columns[i])
            if(vocab.columns[i] not in keywords[k] ):
                keywords[k].append(vocab.columns[i])
    
    
    

##Chi square test 

def chisq_test(w, A):
    c1=vocab.loc[aspect_numeric[A],w]
    c2=sum(vocab.loc[:,w])-c1
    #sent_temp=sent_word_matrix[:,sent_word_matrix[A]>=0]
    c3=len(sent_word_matrix[(sent_word_matrix[A]>0) &(sent_word_matrix[w]==0)])
    #c=sum(vocab.sum(axis=1))
    c=(len(sum(review_sent,[]))+1)
    c4=len(sent_word_matrix)-c1
    return(c*(c1*c4-c2*c3)**2/((c1+c3)*(c2+c4)*(c1+c2)*(c3+c4)))


###Saving sentences tagged to aspects in csv

sent=pd.DataFrame([], columns=['aspect','sentence'])
#print(sent.columns)
for r in all_review_list:
    #print(r)
    for s in r:
        df=pd.DataFrame.from_dict(r, orient='columns', dtype=None)
        #print(df.iloc[:,1:2])
        #sent.append(df.iloc[:,1:2], ignore_index=True)
        #sent=sent.append(df.iloc[:,0:2])
    sent=pd.concat([sent,df.iloc[:,0:2]],join='inner')
print(sent)

sent.to_csv('Aspect_tagged_sentences.csv', encoding='utf-8')
