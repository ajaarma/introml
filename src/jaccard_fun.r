user.data <- read.table('data/u.data')
movie.item <- read.delim('data/u.item',header=FALSE,sep="|")

jaccard_fun <- function(item_id1,item_id2)
  {

    #item_id_1 = movie.item[movie.item[,2]=='Three Colors: Red (1994)',1]
    #item_id_2 = movie.item[movie.item[,2]=='Three Colors: Blue (1993)',1]
    
    user_id_1 = user.data[user.data[,2]==item_id1,1]
    user_id_2 = user.data[user.data[,2]==item_id2,1]
    common = intersect(user_id_1,user_id_2)
    jaccard_coeff = length(common)/(length(user_id_1)+length(user_id_2)-length(common))
print(jaccard_coeff)

  }

