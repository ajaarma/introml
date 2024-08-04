#Part 2- Hierarchical Clustering
#Selecting 20 movies based on the the genere only - ACTION, SCI-FI, WAR,ROMANTIC,MUSCIAL, UNKNOWN

#Generating 20 movies data set

jaccard_coeff <- function(item_1,item_2,gener.item)
     {
       m0_1 = length(which((gener.item[item_1,]==0)==TRUE))              
       m1_2 = length(which((gener.item[item_2,]==1)==TRUE))
       m01 = m0_1+m1_2
       m1_1 = length(which((gener.item[item_1,]==1)==TRUE))
       m11 = m1_1+m1_2
       m0_2 = length(which((gener.item[item_2,]==0)==TRUE))
       m10 = m1_1+m0_2
       m00 = m0_1+m0_2
       jc_coeff = m11/(m01+m10+m11)
       jc_dist = 1 - jc_coeff
       return (jc_dist)
       #return(jc_coeff)

     }

m.item <- read.delim('data/u.item',header=FALSE,sep="|")
gener.item <- m.item[,c(6,7,13,18,21,23)]
m.action = m.item[m.item[,7]==1,1][1:4]
m.doc = m.item[m.item[,13]==1,1][1:4]
m.musical = m.item[m.item[,18]==1,1][1:4]
m.scifi = m.item[m.item[,21]==1,1][1:4]
m.war = m.item[m.item[,23]==1,1][1:4]
m.unknown = m.item[m.item[,6]==1,1]

m.id = unique(c(m.action,m.doc,m.musical,m.scifi,m.war,m.unknown))

gener.item = gener.item[m.id,]
#gener.item$m.id = m.id

data.matrix = mat.or.vec(length(m.id),length(m.id))
colnames(data.matrix) <- m.id
rownames(data.matrix) <- m.id

for(i in 1:length(m.id))
{
  for(j in 1:length(m.id))
  {
    #print(i)
    dist_value = jaccard_coeff(i,j,gener.item)
    #print(dist_value)
    data.matrix[i,j] <-  dist_value
    
  }
}

d <- as.dist(data.matrix)
fit_single <- hclust(d,method="single")
#plot(fit_single)

fit_complete <- hclust(d,method="complete")
#plot(fit_complete)
par(mfrow = c(2,2))
plot(fit_single,main="Dendogram of Single Link method",xlab="movie ids",ylab="dissimlarity measure")
plot(fit_complete,main = "Dendogram of Complete Link method",xlab="movie ids",ylab="dissimilarity measure")









