# init libraries etc
libs <-c("RTextTools","plyr", "class", "tm");
lapply(libs, require, character.only = TRUE);

#Set options
#options(stringsAsFactors = FALSE);

#Set parameters
source("localsettings.R");
# the built-in one seems to be broken
source("create_analytics.R");

# clean text-scrub out nasty html
# except that we don't use stopwords for, you know, html...
# maybe we should find/make a stopwords file for that. 
# Also, maybe consider using the SMART set instead of english
cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation);
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace);
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower));
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("SMART"));
  return(corpus.tmp)
}

# Read all the subdirectories (pfa_label) in the path (pathname) separated
# by the sepchar path separator, all specified in localsettings.R
# From each directory construct a corpus, and create a "pfa" label on it
# with the same value as the name of the directory
corpora <- lapply(pfa_label,function(xx){
  oo <- Corpus(DirSource(paste(pathname,xx,sep=sepchar),encoding=enc));
  meta(oo,"pfa") <- xx;
  oo;
});

# Now, use the c() method for Corpus objects to combine them into one giant
# corpus. Notice that the "pfa" labels follow along
corpus <- do.call(c,corpora);

# Randomize their order, and store the corpus length in the lc variabe
# while we are at it
corpus <- corpus[sample(1:(lc<-length(corpus)),lc)];

# clean our corpus
corpus <- cleanCorpus(corpus);

# create a document term matrix
# The control list is something we can play with and see how it affects results
dm <- DocumentTermMatrix(corpus,control=list(removeNumbers=T,stemWords=T));

# Now we use the pfa labels we created earlier to create the correct classes
# Told you the lc variable (length of the corpus, i.e. number of documents) 
# would come in handy. And no, not a virgin, not yet. Wait, what?
# Note that we convert the second argument to numbers-- the labels must be numeric
# otherwise certain columns come out as all-NA and there are errors and warnings.
dc <- create_container(dm,as.numeric(factor(meta(corpus,"pfa")[,1])),trainSize = tsz <- 1:round(lc/2),testSize = seq(lc)[-tsz],virgin=F);

# these three algorithms are the fast ones, that wont lock up your computer
trained2 <- train_models(dc,c("SVM","GLMNET","MAXENT"));
classed2 <- classify_models(dc,trained2);
anals <- create_analytics(dc,classed2);
print(summary(anals));
consensus_labels<-factor(create_scoreSummary(dc,classed2)$BEST_PROB,levels=levels(dc@testing_codes));
confmatrix<-addmargins(table(Real=dc@testing_codes,Predicted=consensus_labels));
print(confmatrix);
cat('Recall\n')
print(diag(confmatrix)/confmatrix[,'Sum']);
cat('Precision\n')
print(diag(confmatrix)/confmatrix['Sum',]);