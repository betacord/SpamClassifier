library(tm)
library(ggplot2)
spam.path <- "data/spam/" #training
spam2.path <- "data/spam_2/" #test
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

get.msg <- function(path) {
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)
  msg <- text[seq(which(text=="")[1] + 1, length(text), 1)]
  close(con)
  return(paste(msg, collapse = "\n"))
}
