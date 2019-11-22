install.packages("NLP")
install.packages("pdftools")
install.packages("tm")
install.packages("proxy")
install.packages("topicmodels")
install.packages("mime")
install.packages("stringr")
install.packages("tesseract")
install.packages("textclean")
install.packages("SnowballC")
install.packages("tidytext")
install.packages("purrr")
install.packages("udpipe")

setwd("C:/Users/despina/Google Drive/ricerca/Jel codes/analysis")

library(pdftools)
library(purrr)
library(NLP)
library(tm)
library(proxy) #require last version of R
library(topicmodels)
library(httpuv)
library(mime)
#library(servr)
library(stringr)
library(tesseract)
library(textclean)

#install.packages("tesseract", dependencies = TRUE)
#try it
#prova <- ocr("M19.pdf")
#change working directory
#setwd("C:/Users/despina/Google Drive/ricerca/myprojects/jelcodes")
#setwd("~/Marco Dropbox/marco guerzoni/TUTTI")
a <- list.files(pattern = "pdf$")
#a<- a[1:500]

# b <- lapply(txt2, nchar)
# b<- unlist(b, use.names=TRUE)
# c <- 1:1652
# d <- as.data.frame(cbind(b,c))
# d <- cbind(d,c)
# row.names(d)<-c
# colnames(d)<- c("num", "id")
# d <- d[order(d$num),] 
# 
# file_list2[499]
# 
# nchar(txt[2])

#txt <- lapply(a, ocr)
txt <- lapply(a[100:300], pdf_text)
#Sys.time()
#save.image()
txt <- lapply(txt, gsub, patter="[\r\n]", replacement=" ")



prova <-pdf_text("g20.pdf")
prova <-ocr("g20.pdf")
file_list2<-substring(file_list, 3)

txt2<-txt

for (i in 1:18){
  
  nick<-d$id[i] 
  txt2[nick]<-ocr(file_list2[nick])  
  
}

txt2[1419]<-ocr(file_list2[1419]) 

txt3<-txt2

#1462 documenti. 
summary(d)

###change directory two analysis 
###upload stopwords and words to erase

stopA<-read.csv("stopA.csv", sep=";", fileEncoding="latin1")
stopA1<-read.csv("stopA1.csv", sep=";", fileEncoding="latin1")
stop<-read.csv("stopwordseng.csv", sep=";", fileEncoding="latin1")
stopEM<-read.csv("stopEM.csv", sep=";",fileEncoding="latin1")
stopF<-read.csv("stopF.csv", sep=";")
stopGH<-read.csv("stopGH.csv", sep=";")
stopI<-read.csv("stopI.csv", sep=";")
stopL<-read.csv("stopL.csv", sep=",")

stop <- toString(stop[,1])
#stop <- removePunctuation(stop ,preserve_intra_word_dashes = TRUE)
stop <- strsplit(stop, "[[:space:]]+")
stop <- unlist(stop)
stop <- removePunctuation(stop ,preserve_intra_word_dashes = TRUE)
stop <- removeNumbers(stop)
stop<- tolower(stop)

stopA <- apply(stopA, 2, toString)
stopA<- stripWhitespace(stopA)
#stopA <- removePunctuation(stopA ,preserve_intra_word_dashes = TRUE)
stopA<- stripWhitespace(stopA)
stopA <- strsplit(stopA, "[[:space:]]+")
stopA <- unlist(stopA)
stopA <- removePunctuation(stopA ,preserve_intra_word_dashes = TRUE)
stopA <- removeNumbers(stopA)
stopA<- tolower(stopA)


stopA1 <- apply(stopA1, 2, toString)
stopA1<- stripWhitespace(stopA1)
#stopA1 <- removePunctuation(stopA1 ,preserve_intra_word_dashes = TRUE)
stopA1 <- strsplit(stopA1, "[[:space:]]+")
stopA1 <- unlist(stopA1)
stopA1 <- removePunctuation(stopA1 ,preserve_intra_word_dashes = TRUE)
stopA1 <- removeNumbers(stopA1)
stopA1<- tolower(stopA1)


stopEM <- apply(stopEM, 2, toString)
stopEM<- stripWhitespace(stopEM)
#stopEM <- removePunctuation(stopEM ,preserve_intra_word_dashes = TRUE)
stopEM <- strsplit(stopEM, "[[:space:]]+")
stopEM<- unlist(stopEM)
stopEM <- removePunctuation(stopEM ,preserve_intra_word_dashes = TRUE)
stopEM <- removeNumbers(stopEM)
stopEM<- tolower(stopEM)


stopF <- apply(stopF, 2, toString)
stopF<- stripWhitespace(stopF)
#stopF <- removePunctuation(stopF ,preserve_intra_word_dashes = TRUE)
stopF<- strsplit(stopF, "[[:space:]]+")
stopF<- unlist(stopF)
stopF <- removePunctuation(stopF ,preserve_intra_word_dashes = TRUE)
stopF<- removeNumbers(stopF)
stopF<- tolower(stopF)

stopGH <- apply(stopGH, 2, toString)
stopGH<- stripWhitespace(stopGH)
#stopGH <- removePunctuation(stopGH ,preserve_intra_word_dashes = TRUE)
stopGH<- strsplit(stopGH, "[[:space:]]+")
stopGH<- unlist(stopGH)
stopGH <- removePunctuation(stopGH ,preserve_intra_word_dashes = TRUE)
stopGH<- removeNumbers(stopGH)
stopGH<- tolower(stopGH)


stopI <- apply(stopI, 2, toString)
stopI<- stripWhitespace(stopI)
#stopI <- removePunctuation(stopI ,preserve_intra_word_dashes = TRUE)
stopI<- strsplit(stopI, "[[:space:]]+")
stopI<- unlist(stopI)
stopI <- removePunctuation(stopI ,preserve_intra_word_dashes = TRUE)
stopI<- removeNumbers(stopI)
stopI<- tolower(stopI)


stopL <- apply(stopL, 2, toString)
stopL<- stripWhitespace(stopL)
stopL <- removePunctuation(stopL ,preserve_intra_word_dashes = TRUE)
stopL<- strsplit(stopL, "[[:space:]]+")
stopL<- unlist(stopL)
stopL <- removePunctuation(stopL ,preserve_intra_word_dashes = TRUE)
stopL<- removeNumbers(stopL)
stopL<- tolower(stopL)

####stop and stopA### word to remove

txt<-txt3
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt<-gsub("http[^[:space:]]*", "", txt)
txt<-gsub("www[^[:space:]]*", "", txt)
txt<-gsub('[\u2013:\u2016]', "", txt)
txt<-stringi::stri_trans_general(txt, "latin-ascii")
txt <- gsub("[^\x20-\x7E]", "", txt)

txt <- replace_non_ascii(txt, replacement = "", remove.nonconverted = TRUE)

txt <- gsub("-","", txt,ignore.case=T)
txt <- removePunctuation(txt,preserve_intra_word_dashes = FALSE)
txt <- removeNumbers(txt)
txt <- stripWhitespace(txt)
txt<- tolower(txt)


txt1<-txt






#reviews <- txt1
# doc.list <- strsplit(reviews, "[[:space:]]+")
#utils::View(doc.list[[3]])
# 
# # compute the table of terms:
# term.table <- table(unlist(doc.list))
# term.table <- sort(term.table, decreasing = TRUE)
# del <- names(term.table) %in% stop | term.table < 6
# term.table <- term.table[!del]
# del <- names(term.table) %in% stopA | term.table < 6
# term.table <- term.table[!del]
# del <- names(term.table) %in% stopA1 | term.table < 6
# term.table <- term.table[!del]
# del <- names(term.table) %in% stopEM | term.table < 6
# term.table <- term.table[!del]
# del <- names(term.table) %in% stopF | term.table < 6
# term.table <- term.table[!del]
# del <- names(term.table) %in% stopGH | term.table < 6
# term.table <- term.table[!del]
# del <- names(term.table) %in% stopI | term.table < 6
# term.table <- term.table[!del]
# 
# 
# #term.table <- term.table[!del]
# utils::View(term.table)
# 
# vocab <- names(term.table)
# 
# # now put the documents into the format required by the lda package:
# get.terms <- function(x) {
#   index <- match(x, vocab)
#   index <- index[!is.na(index)]
#   rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
# }
# documents <- lapply(doc.list, get.terms)
# ##################lda
# 
# #Compute some statistics related to the data set:
# D <- length(documents)  # number of documents (2,000)
# W <- length(vocab)  # number of terms in the vocab (14,568)
# doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
# N <- sum(doc.length)  # total number of tokens in the data (546,827)
# term.frequency <- as.integer(term.table)  
# 
# K <- 27 #number of topics
# G <- 5000
# alpha <- 0.02
# eta <- 0.02
# 
# # Fit the model:
# 
# set.seed(357)
# t1 <- Sys.time()
# fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
#                                    num.iterations = G, alpha = alpha, 
#                                    eta = eta, initial = NULL, burnin = 0,
#                                    compute.log.likelihood = TRUE)
# t2 <- Sys.time()
# t2 - t1  # 
# 
# #topwords and top documents per topic
# top.words <- top.topic.words(fit$topics, 30, by.score=TRUE)
# top.document<-top.topic.documents(fit$document_sums, num.documents = 86, alpha = 0.1)
# #change working directory if not yet
# write.csv(top.words, file="topwordsQ.csv")

##############?
corp <- Corpus(VectorSource(txt))
#corp  <- tm_map(corp , removeWords, stop)
corp  <- tm_map(corp , removeWords, stopA)
corp  <- tm_map(corp , removeWords, stopA1)
corp  <- tm_map(corp , removeWords, stopEM)
corp  <- tm_map(corp , removeWords, stopF)
corp  <- tm_map(corp , removeWords, stopGH)
corp  <- tm_map(corp , removeWords, stopI)
corp  <- tm_map(corp , removeWords, stopL)
save.image("corp0707")
dtm <- DocumentTermMatrix(corp,control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers= TRUE,stemming = TRUE ,stopwords = TRUE,minWordLength = 3))
dtm1<-removeSparseTerms(dtm, 0.998)

library(topicmodels)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] 
ap_lda1 <- LDA(dtm1, 70, method = "Gibbs",control = list(iter = 100, seed = 33))

save(ap_lda, file = "JEL_model-lda-gibbs-27topics_new.RData")









#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

# topicmodels_json_ldavis <- function(fitted, doc_term){
#   require(LDAvis)
#   require(slam)
#   
#   # Find required quantities
#   phi <- as.matrix(posterior(fitted)$terms)
#   theta <- as.matrix(posterior(fitted)$topics)
#   vocab <- colnames(phi)
#   term_freq <- slam::col_sums(doc_term)
#   
#   # Convert to json
#   json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
#                                  vocab = vocab,
#                                  doc.length = as.vector(table(doc_term$i)),
#                                  term.frequency = term_freq)
#   
#   return(json_lda)
# }


json_res <- topicmodels_json_ldavis(ap_lda1, dtm1)

serVis(json_res)



#library(tidytext)

#ap_topics <- tidy(ap_lda, matrix = "beta")

#library(ggplot2)
#library(dplyr)

# ap_top_terms <- ap_topics %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# ap_top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()
# 
# ####
# file_list <- list.files(".", full.names = TRUE, pattern = '.pdf$')
# 
# s_pdf_text <- safely(pdf_text) # helps catch errors
# 
# walk(file_list, ~{                                     # iterate over the files
#   
#   res <- s_pdf_text(.x)                                # try to read it in
#   if (!is.null(res$result)) {                          # if successful
#     
#     message(sprintf("Processing [%s]", .x))
#     
#     txt_file <- sprintf("%stxt", sub("pdf$", "", .x))  # make a new filename
#     
#     unlist(res$result) %>%                             # cld be > 1 pg (which makes a list)
#       tolower() %>%                                    
#       paste0(collapse="\n") %>%                        # make one big text block with line breaks
#       cat(file=txt_file)                               # write it out
#     
#   } else {                                             # if not successful
#     message(sprintf("Failure converting [%s]", .x))    # show a message
#   }
#   
# })
# a<-a[!a=="g32.pdf"]


# > file.copy(from="stopA1.csv", to="~/", 
#             +           overwrite = TRUE, recursive = FALSE, 
#             +           copy.mode = TRUE)



coffee_m <- as.matrix(dtm1)

# Calculate the rowSums: term_frequency
term_frequency <- colSums(coffee_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = F)

# View the top 10 most common words
term_frequency[1:100]
