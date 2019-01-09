
oncf = read.csv("ONCF3.csv", header = TRUE, sep = ";" )
str(oncf)

summary(oncf)
model = lm(Number_of_likes ~ .- Image_URL - Link - Created_Time- Text, data = oncf)
plot(model)


summary(model)







plot(oncf$Created.Time,oncf$Username)
plot(oncf$longitude, oncf$latitude)
boxplot







library(tm)
library(SnowballC)

oncf$Username = as.factor(oncf$Username )
corpusOncf = VCorpus(VectorSource(oncf$Username))
corpusOncf
corpusOncf$content
corpusOncf[[1]]$content
corpusOncf = tm_map(corpusOncf, tolower)
corpusOncf$content
corpusOncf = tm_map(corpusOncf, removePunctuation)
corpusOncf[[1]]


oncf$Location = as.factor(oncf$Location )
corpusOncf2 = VCorpus(VectorSource(oncf$Location))
corpusOncf2
corpusOncf2$content

corpusOncf2[[20]]$content
corpusOncf2 = tm_map(corpusOncf2, tolower)
corpusOncf2$content
corpusOncf2[[20]]
corpusOncf2 = tm_map(corpusOncf2, removePunctuation)
corpusOncf2$content
corpusOncf2[[20]]
corpusOncf2 = tm_map(corpusOncf2, removePunctuation)
stopwords("french")[1:10]
length(stopwords("french"))
corpusOncf2 = tm_map(corpusOncf2, stemDocument)
corpusOncf2[[20]]




