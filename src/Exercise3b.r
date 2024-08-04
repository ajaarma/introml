#loadnews()
#source("sourcedir.R")
#sourceDir("./")
source("loadnews.R")
loadnews()


word_list = c()
docID_master = c()
word_doc = list()
load("word_doc")
pwg_list = c()

total_set = list()

for(i in 1:20)
{
  docID = which(y==i)
  total_set[[i]] = docID


}

wordID = unique(Xs[,2])
wg_mat = mat.or.vec(length(wordID),20)
rownames(wg_mat) = paste(wordID)
colnames(wg_mat) = paste(1:20)
for(i in 1:20)
{
  docID = total_set[[i]]
  data.wg = c()
  pwg_list = c()
  word_list = c()
  for(k in 1:length(wordID))
  {
    doc = word_doc[[wordID[k]]]
    docID_tmp = intersect(docID,doc)
    Nwg = length(docID_tmp)
    Ng = length(docID)
    word_list = c(word_list,Nwg)
    Pwg =  (Nwg+1)/(Ng+2)
    if(Pwg ==0)
    {
      cat("yes",k,i,"\n")
    }
    pwg_list = c(pwg_list,Pwg)
    wg_mat[k,i] = Pwg
  
  }
  data.wg = data.frame(wordID,pwg_list)
  sort_data.wg = data.wg[with(data.wg,order(-pwg_list,wordID)),]
  first_200 = sort_data.wg$wordID[1:200]
  voc_word = voc[first_200]
  cat("Most probable word list of newsgroup :", i,"\n")
  cat(voc_word)#,"\t",sort_data.wg$pwg_list[1:200])
  cat("\n")
  
}
mrg.prob = list()
for(i in 1:20)
{
  docs = total_set[[i]]
  prob = length(docs)/11269
  mrg.prob[[i]] = prob

}

master_doc = unique(Xs[,1])
dg_mat = mat.or.vec(length(master_doc),20)
colnames(dg_mat) = paste(1:20)
rownames(dg_mat) = paste(master_doc)

for(i in 1:20)
{ 
  cat(i,"\n")
  for(ele in master_doc)
  {
    words = Xs[which(Xs[,1]==ele),2]
    scr_wg = wg_mat[words,i]
    log.scr_wg = log(scr_wg)
    sum.log = sum(log.scr_wg)
    #scr = exp(sum.log)
    dg_mat[ele,i] = log(mrg.prob[[i]])+ sum.log

  }

}

save(dg_mat,file="dg_mat_log")
