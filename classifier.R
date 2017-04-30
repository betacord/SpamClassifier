library(tm)
library(ggplot2)

get.msg <- function(path) {
  con <- file(path, open = "rt")
  text <- readLines(con)
  msg <- text[seq(which(text == "")[1] + 1, length(text), 1)] #from first empty line to end
  close(con)
  return(paste(msg, collapse = "\n"))
}

get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE,
                  minDocFreq = 2) # word min 2 times, without numbers
  doc.tdm <- TermDocumentMatrix(doc.corpus, control) #i - word, j - document
  return(doc.tdm)
}

spam.path <- "data/spam/" #training
spam2.path <- "data/spam_2/" #test
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

#training for spam set
spam.docs <- dir(spam.path) #filenames from directory
spam.docs <- spam.docs[which(spam.docs != "cmds")] #without cmds files
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep = ""))) #returns vector of messages
spam.tdm <- get.tdm(all.spam)
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix) #how many messages contains this word
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), 
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequrency")
spam.df$frequrency <- as.numeric(spam.df$frequrency)
spam.occurence <- sapply(1:nrow(spam.matrix), function(i) {
  length(which(spam.matrix[i,] > 0)) / ncol(spam.matrix)
}) #documents percent with word / all documents
spam.density <- spam.df$frequrency / sum(spam.df$frequrency)
spam.df <- transform(spam.df, density = spam.density, occurence = spam.occurence)
#head(spam.df[with(spam.df, order(-occurence)),])


#training for easy ham set
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")] 
all.easyham <- sapply(easyham.docs, function(p) get.msg(paste(spam.path, p, sep = ""))) 
easyham.tdm <- get.tdm(all.easyham)
easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts), as.numeric(easyham.counts)), 
                      stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequrency")
easyham.df$frequrency <- as.numeric(easyham.df$frequrency)
easyham.occurence <- sapply(1:nrow(easyham.matrix), function(i) {
  length(which(easyham.matrix[i,] > 0)) / ncol(easyham.matrix)
})
easyham.density <- easyham.df$frequrency / sum(easyham.df$frequrency)
easyham.df <- transform(easyham.df, density = easyham.density, occurence = easyham.occurence)
head(easyham.df[with(easyham.df, order(-occurence)),])

