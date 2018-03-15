#Read movieLens data
movies= read.table("movieLens.txt", header =FALSE, sep="|",quote = "\"")
#Structure of movieLens dataset
str(movies)
#Add col names to the data set
colnames(movies)= c("ID","Title","ReleaseDate", "VideoReleaseDate","IMDB","Unknown",
                    "Action", "Adventure","Animation","Childrens","Comedy", "Crime", "Documentary", "Drama",
                    "Fantasy", "FilmNoir","Horror", "Musical", "Mystery", "Romance", "SciFi",
                    "Thriller", "War", "Western")
# We wont be using the ID, Releasedate, VideoRelease, IMDB variables, so we will
#remove them
movies$ID=NULL
movies$ReleaseDate= NULL
movies$VideoReleaseDate= NULL
movies$IMDB= NULL
movies= unique(movies)
str(movies)
 
# Compute distances between all data points
distances= dist(movies[2:20], method = "euclidean")
#cluster the data points
clusterMovies=hclust(distances, method="ward.D2")
#plot the cluster dendogram
plot(clusterMovies)
#let us pick 10 clusters and assign the data points to the particular clusters
clusterGroups= cutree(clusterMovies, k=10)
clusterGroups
#Lets figure out what cluster Men in Black movie in in
subset(movies, Title=="Men in Black (1997)")
# Men in Black is in cluster 2
clusterGroups[257]
#Look at what other movies are in cluster2
cluster2= subset(movies, clusterGroups==2)
cluster2
#Look at titles of movies in cluster 2
cluster2$Title[1:10]