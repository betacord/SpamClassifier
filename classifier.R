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
                  minDocFreq = 2) # word min 2 chars, without numbers
  doc.tdm <- TermDocumentMatrix(doc.corpus, control) #i - word, j - document
  return(doc.tdm)
}

spam.path <- "data/spam/" #training
spam2.path <- "data/spam_2/" #test
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

spam.docs <- dir(spam.path) #filenames from directory
spam.docs <- spam.docs[which(spam.docs != "cmds")] #without cmds files
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep = ""))) #returns vector of messages
spam.tdm <- get.tdm(all.spam)

