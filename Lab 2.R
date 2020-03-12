#Write a function in R to compute the cosine similarity between a document and a query

#Use:
library(RTextTools)
library(tm)

documents=c("the grey cat is nice","how to feed a cats","dogs make great pets")
q1="feeding cats"

#function 
cosinesim = function(query,document){
  r = dim(document)[1]
  c = sqrt(sum(query[1,]^2))
  s = 0
  print(r);print(c)
  for (i in 1:r){
    a = sum(query[1,]*document[i,])
    b = sqrt(sum(document[i,]^2))
    s[i] = a/(b*c)
  }
  print(s)
  return(s)
}

#Question 1: Show your results without stemming and stopword removal
corpus = create_matrix(documents,removeStopwords = F,stemWords = F)
corpus = as.matrix(corpus)
q1matrix = create_matrix(q1,originalMatrix = corpus,removeStopwords = F,stemWords = F)
q1matrix = as.matrix(q1matrix)
scores = cosinesim(q1matrix, corpus)
#scores
#[1] 0.0000000 0.5773503 0.0000000

#Question 2: Show your results with stemming and stop word removal
corpus = create_matrix(documents,removeStopwords = T,stemWords = T)
corpus = as.matrix(corpus)
q1matrix = create_matrix(q1,originalMatrix = corpus,removeStopwords = T,stemWords = T)
q1matrix = as.matrix(q1matrix)
scores = cosinesim(q1matrix, corpus)
#> scores
#[1] 0.0000000 0.7071068 0.0000000