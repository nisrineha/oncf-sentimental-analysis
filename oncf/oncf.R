
#Extract database from Twitter, make text analytics on tweets

echo=TRUE
eval=TRUE
#------------------------------------------ Install new packages
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("caTools")
library(caTools)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)


#-------------------------------------------------------------------------------

#export data
oncf = read.csv("ONCF3.csv", header = TRUE, sep = ";" )

#view data 
str(oncf)
summary(oncf)
attach(oncf)
summary(Number_of_likes)
table(Number_of_likes)

#--------------------------------------convert to a corpus for pre-processing 
corpus = VCorpus(VectorSource(oncf$Text)) 
corpus
corpus[[1]]$content
# Convert to lower-case
corpus = tm_map(corpus, content_transformer(tolower))
# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[3]]$content
# Remove stopwords 
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
# Stem document 
corpus = tm_map(corpus, stemDocument)



#------------------------------------------------Create matrix---------------------------
frequencies = DocumentTermMatrix(corpus)
frequencies
# Check for sparsity
findFreqTerms(frequencies, lowfreq=20)
# Remove sparse terms
sparse = removeSparseTerms(frequencies, 0.995)
sparse

# --------------------------------------------Convert to a data frame----------------
oncfSparse = as.data.frame(as.matrix(sparse))
colnames(oncfSparse) = make.names(colnames(oncfSparse))

# -------------------------------------------Add dependent variable--------------
oncf$Negative = as.factor(oncf$Number_of_likes <= 40)
table(oncf$Negative)
oncfSparse$Negative = oncf$Negative


# ------------------------------------------------Split the data------------------------
set.seed(12)
split = sample.split(oncfSparse$Negative, SplitRatio = 0.6)
trainSparse = subset(oncfSparse, split==TRUE)
testSparse = subset(oncfSparse, split==FALSE)

#----------------------------------------------Build a CART model------------------------

oncfCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(oncfCART)
# Evaluate the performance of the model
predictCART = predict(oncfCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)
#model's accuracy
table(testSparse$Negative)




