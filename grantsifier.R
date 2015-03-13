# init libraries etc
libs <-c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

#Set options
options(stringsAsFactors = FALSE)

#Set parameters
source("localsettings.R")

# clean text-scrub out nasty html
# except that we don't use stopwords for, you know, html...
# maybe we should find/make a stopwords file for that. 
# Also, maybe consider using the SMART set instead of english
cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
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
dm <- DocumentTermMatrix(corpus,control=list(removeNumbers=T));

# Now we use the pfa labels we created earlier to create the correct classes
# Told you the lc variable (length of the corpus, i.e. number of documents) 
# would come in handy. And no, not a virgin, not yet. Wait, what?
dc <- create_container(dm,meta(corpus,"pfa")[,1],trainSize = 1:round(lc/2),virgin=F);
