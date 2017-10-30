library(tm)
library(plyr)
library(stringr)
library(ggplot2)
library(wordcloud)

df=read.csv(file.choose(), stringsAsFactors = F)
##subset the data
lisa=subset(df,df$raw_character_text=='Lisa Simpson')
bart=subset(df,df$raw_character_text=='Bart Simpson')
marge=subset(df,df$raw_character_text=='Marge Simpson')
homer=subset(df,df$raw_character_text=='Homer Simpson')
lisa_corpus=Corpus(VectorSource(lisa$normalized_text))
bart_corpus=Corpus(VectorSource(bart$normalized_text))
marge_corpus=Corpus(VectorSource(marge$normalized_text))
homer_corpus=Corpus(VectorSource(homer$normalized_text))
#created custom stopwords for this dataset
myStopwords <- c('youre','dont','like','theyre','theres', 'thats','whats','let','two','youve','ill','didnt','just','cant','youll','isnt','get','got','can','ive')
clean_lisa_corpus=tm_map(lisa_corpus, tolower)
clean_lisa_corpus <- tm_map(lisa_corpus, content_transformer(tolower))
#remove numbers
clean_lisa_corpus=tm_map(clean_lisa_corpus, removeNumbers)
#remove punctuation
clean_lisa_corpus=tm_map(clean_lisa_corpus, removePunctuation)
#remove non content words or "stop words"
stopwords()[1:10]
clean_lisa_corpus=tm_map(clean_lisa_corpus,removeWords,stopwords())
clean_lisa_corpus <- tm_map(clean_lisa_corpus, removeWords, myStopwords)
dtm=DocumentTermMatrix(clean_lisa_corpus)
#remove white space
clean_lisa_corpus=tm_map(clean_lisa_corpus, stripWhitespace)
#inspect it
inspect(clean_lisa_corpus[1:3])
#tokenize the now cleaned corpus
lisa_dtm=DocumentTermMatrix(clean_lisa_corpus)
inspect(lisa_dtm[1:4, 30:35])
lisa2_wordcloud=wordcloud(clean_lisa_corpus, min.freq=50,colors='mediumorchid4')


##BART
clean_bart_corpus=tm_map(bart_corpus, tolower)
clean_bart_corpus <- tm_map(bart_corpus, content_transformer(tolower))
#remove numbers
clean_bart_corpus=tm_map(clean_bart_corpus, removeNumbers)
#remove punctuation
clean_bart_corpus=tm_map(clean_bart_corpus, removePunctuation)
#remove non content words or "stop words"
stopwords()[1:10]
clean_bart_corpus=tm_map(clean_bart_corpus,removeWords,stopwords())
clean_bart_corpus <- tm_map(clean_bart_corpus, removeWords, myStopwords)
#remove white space
clean_bart_corpus=tm_map(clean_bart_corpus, stripWhitespace)
#inspect it
inspect(clean_bart_corpus[1:3])
#tokenize the now cleaned corpus
bart_dtm=DocumentTermMatrix(clean_bart_corpus)
inspect(bart_dtm[1:4, 30:35])
bart2_wordcloud=wordcloud(clean_bart_corpus, min.freq=50,colors="orange2")

##Marge
clean_marge_corpus=tm_map(marge_corpus, tolower)
clean_marge_corpus <- tm_map(marge_corpus, content_transformer(tolower))
#remove numbers
clean_marge_corpus=tm_map(clean_marge_corpus, removeNumbers)
#remove punctuation
clean_marge_corpus=tm_map(clean_marge_corpus, removePunctuation)
#remove non content words or "stop words"
stopwords()[1:10]
clean_marge_corpus=tm_map(clean_marge_corpus,removeWords,stopwords())
clean_marge_corpus <- tm_map(clean_marge_corpus, removeWords, myStopwords)

#remove white space
clean_marge_corpus=tm_map(clean_marge_corpus, stripWhitespace)
#inspect it
inspect(clean_marge_corpus[1:3])
#tokenize the now cleaned corpus
marge_dtm=DocumentTermMatrix(clean_marge_corpus)
inspect(marge_dtm[1:4, 30:35])
marge_wordcloud=wordcloud(clean_marge_corpus, min.freq=50,colors='steelblue')

##Homer
clean_homer_corpus=tm_map(homer_corpus, tolower)
clean_homer_corpus <- tm_map(homer_corpus, content_transformer(tolower))
#remove numbers
clean_homer_corpus=tm_map(clean_homer_corpus, removeNumbers)
#remove punctuation
clean_homer_corpus=tm_map(clean_homer_corpus, removePunctuation)
#remove non content words or "stop words"
stopwords()[1:10]
clean_homer_corpus=tm_map(clean_homer_corpus,removeWords,stopwords())
clean_homer_corpus <- tm_map(clean_homer_corpus, removeWords, myStopwords)

#remove white space
clean_homer_corpus=tm_map(clean_homer_corpus, stripWhitespace)
#inspect it
inspect(clean_homer_corpus[1:3])
#tokenize the now cleaned corpus
homer_dtm=DocumentTermMatrix(clean_homer_corpus)
inspect(homer_dtm[1:4, 30:35])
homer_wordcloud=wordcloud(clean_homer_corpus, min.freq=50,colors='firebrick')
simpsons = rbind(lisa,homer,bart,marge)
simpsons=na.omit(simpsons)
summary(simpsons)
lisa_count=ggplot(lisa, aes(x=lisa$word_count))+geom_bar()