load("word_doc")
load("dg_mat_log")


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
  docID = which(y==i)
  total_set[[i]] = docID
}

binMat = mat.or.vec(11269,20)
for(i in 1:11269)
{
  index = as.vector(which(dg_mat[i,]==max(dg_mat[i,])))
  binMat[i,index] = 1

}
conf.train = mat.or.vec(20,20)
for(i in 1:20)
{
  print(i)
  docID = train_set[[i]]
  for(ele in docID)
  { 
    index = which(binMat[ele,]==1)
    count = conf.train[i,index]
    conf.train[i,index] = count+1

  }
}

conf.test = mat.or.vec(20,20)
for(i in 1:20)
{
 docID = test_set[[i]]
 for(ele in docID)
 {
   index = which(binMat[ele,]==1)
   count = conf.test[i,index]
   conf.test[i,index] = count+1
 } 
}

write.csv(conf.train,file="conf.train.csv")
write.csv(conf.test,file="conf.test.csv")
