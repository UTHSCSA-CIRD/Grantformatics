# init libraries etc
libs <-c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

#Set options
options(stringsAsFactors = FALSE)

#Set parameters
pfa_label <- c("disparities","dissemination", "methods","options", "systems") # where each fileid == category label
pathname <-  "C:\\Users\\sergan\\Documents\\AMIA_CURRENT\\pcori_separate_files" #enter path of abstracts

# clean text-scrub out nasty html

cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  return(corpus.tmp)
}

# build TermDocumentMatrix(where the magic happens)text2quantative

generateTDM <- function(pfa, path) {
  s.dir <- sprintf("%s/%s", path, pfa)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF-8"))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)

  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <-list(name = pfa, tdm = s.tdm)
}

tdm <- lapply(pfa_label, generateTDM, path = pathname)

# take tda and attach to pfa name
bindPFAtoTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetpfa"
  return(s.df)
}

pfaTDM <- lapply(tdm, bindPFAtoTDM)

# then stack outputted matrices
tdm.stack <- do.call(rbind.fill, pfaTDM)
tdm.stack[is.na(tdm.stack)] <- 0

# hold-out(trainer)-portion of abstracts to teach subsets,dont tell it which one it was awarded compare to actual award
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack)) [- train.idx]

# model - PFA
tdm.pfa <- tdm.stack[, "targetpfa"]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "targetPFA"]

knn.pred <- knn(tdm.stack.nl[train.idx, -686], tdm.stack.nl[test.idx, -686], tdm.pfa[train.idx])

# accuracy
