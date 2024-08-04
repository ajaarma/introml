train_set = list()
test_set = list()
word_list = c()
pwg_list = c()
docID_master = c()
word_doc = list()
load(file="word_doc")
for(i in 1:20)
{
  docID = which(y==i)
  perc = round(90*length(docID)/100)
  docID_90 = docID[1:perc]
  docID_10 = docID[perc+1:(length(docID)-perc)]
  train_set[[i]] <- docID_90
  test_set[[i]] <- docID_10

}

for(i in 1:20)
{

  cat("TRaining Set of newsgroup",i,"\n")
  print(train_set[[i]])
  cat("\n")
  cat("Test set of newsgroup:",i,"\n")
  print(test_set[[i]])
  cat("\n")
  
}
  

