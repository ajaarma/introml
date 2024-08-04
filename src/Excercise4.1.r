#source('sourcedir.R')
#sourceDir('./')

#loadmovielens()

user.data <- read.table('data/u.data')
movie.item <- read.delim('data/u.item',header=FALSE,sep="|")

#JACCARD Coeffiecent between Three Colors:Red and Three Colors:blue
item_id_1 = movie.item[movie.item[,2]=='Three Colors: Red (1994)',1]
item_id_2 = movie.item[movie.item[,2]=='Three Colors: Blue (1993)',1]

user_id_1 = user.data[user.data[,2]==item_id_1,1]
user_id_2 = user.data[user.data[,2]==item_id_2,1]

common = intersect(user_id_1,user_id_2)

jaccard_coeff = length(common)/(length(user_id_1)+length(user_id_2)-length(common))
print(jaccard_coeff)


#List of 5 movies having highest Jaccard Coeff to "Monty Python and Holy Grail"
item_id_holy = movie.item[movie.item[,2]=="Monty Python and the Holy Grail (1974)",1]

item_ids_list = unique(user.data[,2])
jaccard_list = c()

for( ele in item_ids_list)
{
  user_id_1 = user.data[user.data[,2]==ele,1]
  user_id_2 = user.data[user.data[,2]==item_id_holy,1]
  common = intersect(user_id_1,user_id_2)
  jaccard_coeff = length(common)/(length(user_id_1)+length(user_id_2)-length(common))
  jaccard_list = c(jaccard_list,jaccard_coeff)
  #print(jaccard_coeff)

}

jaccrd.frame = data.frame(item_ids_list,jaccard_list)
sorted_jaccard = jaccrd.frame[order(jaccrd.frame$jaccard_list,decreasing=TRUE),]
top_5_movie_id = sorted_jaccard$item_ids_list[1:5]
b = movie.item[,1] %in% top_5_movie_id
movie_list = movie.item[b,2]
print(movie_list)


#List of 5 movies having highest Jaccard Coeff to "Basic Instinct (1992)"
item_id_basic = movie.item[movie.item[,2]=="Basic Instinct (1992)",1]

item_ids_list = unique(user.data[,2])
jaccard_list = c()

for( ele in item_ids_list)
{
  user_id_1 = user.data[user.data[,2]==ele,1]
  user_id_2 = user.data[user.data[,2]==item_id_basic,1]
  common = intersect(user_id_1,user_id_2)
  jaccard_coeff = length(common)/(length(user_id_1)+length(user_id_2)-length(common))
  jaccard_list = c(jaccard_list,jaccard_coeff)
  #print(jaccard_coeff)

}

jaccrd.basic = data.frame(item_ids_list,jaccard_list)
sorted_jaccard_basic = jaccrd.basic[order(jaccrd.basic$jaccard_list,decreasing=TRUE),]
top_5_movie_id = sorted_jaccard_basic$item_ids_list[1:5]
b = movie.item[,1] %in% top_5_movie_id
movie_list_basic = movie.item[b,2]
print(movie_list_basic)


