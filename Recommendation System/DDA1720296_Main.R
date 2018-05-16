library(recommenderlab)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(plyr)
#setwd("~/Documents/My R/Assignments/Recommendation System")

printNA <- function(df) {
  if(sum(is.na(df)) != 0) {
    v <- names(which(sapply(df, anyNA)))
    for(i in 1:length(v)) {
      print(paste(v[i], "has", sum(is.na(df[v[i]])), "NA"))
    }
    print(paste("Total", length(v),"columns has NAs"))
  } else {
    print("No NAs found")
  }
}

beer <- read.csv("beer_data.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#########################   EDA   ########################

printNA(beer)
#100 NA's
# Delete the NA's
beer <- na.omit(beer) 
# Check if there are duplicated reviews i.e. same person reviewed same beer twice
sum(duplicated(beer[,1:2]))
duplicate_review <- beer[duplicated(beer[,1:2]),]
beer <- beer[!duplicated(beer[,1:2]),]

beer$beer_beerid  <- factor(beer$beer_beerid)
length(levels(beer$beer_beerid))
# Unique Beers: 40304
beer$review_profilename  <- factor(beer$review_profilename)
length(levels(beer$review_profilename ))
# Unique Users: 22497
beer$review_overall <- as.numeric(beer$review_overall)
summary(beer$review_overall)

beer_counts <- ddply(beer, .(beer$beer_beerid), nrow)
names(beer_counts) <- c('beer_beerid', 'count')
summary(beer_counts$count)

# Mean Count is 11.77 with Max rating as 977 thus chosen N = 50 i.e. 
# Beers with atleast 50 ratings 
beer_count_final <- beer_counts[( which(beer_counts$count >= 50 )), ]
# With this cut-off we have 2064 Beers
beer_new <- subset(beer, beer$beer_beerid %in% beer_count_final$beer_beerid)
beer_new <- beer_new[,c(2,1,3)]
######################################################################
beer_rating <- as(beer_new, "realRatingMatrix")
beer_df <- as(beer_rating, "data.frame")
dimnames(beer_rating)
rowCounts(beer_rating)
colCounts(beer_rating)
rowMeans(beer_rating)
colMeans(beer_rating)
summary(getRatings(beer_rating))

######################################################################
#Similarity matrix by first 10 Users
beer_similar_users <- similarity(beer_rating[1:10, ],
                            method = "cosine",
                            which = "users")


as.matrix(beer_similar_users)

#Visualise similarity matrix
image(as.matrix(beer_similar_users), main = "User similarity")
# user 7 and 10 looks similar 
######################################################################
#Similarity matrix by first 10 Beers
beer_similar_items <- similarity(beer_rating[,1:10],
                            method = "cosine",
                            which = "items")

as.matrix(beer_similar_items)

image(as.matrix(beer_similar_items), main = "Item similarity")
# Item 3 and 5 looks similar
######################################################################
# Average Beer Rating
hist(getRatings(beer_rating), breaks="FD")

qplot(getRatings(beer_rating), binwidth = 1, 
      main = "Histogram of Beer Ratings", xlab = "Rating")

summary(getRatings(beer_rating))

qplot(getRatings(normalize(beer_rating, method = "Z-score")),  binwidth = 0.5,
      main = "Histogram of normalized ratings", xlab = "Rating" )

summary(getRatings(normalize(beer_rating, method = "Z-score")))
######################################################################
# Average Beer rating per User
summary(rowMeans(beer_rating))
# Average user rating : 3.95
######################################################################
# Average rating of Beer
summary(colMeans(beer_rating))
# Average Beer rating : 3.87
# Top 10 Beers as per ratings
head(sort(colMeans(beer_rating), decreasing = TRUE), n = 10)
 
######################################################################
#The average number of ratings given to the beers
summary(colCounts(beer_rating))


boxplot(colCounts(beer_rating),  
        boxfill = "gold",  
        outline = FALSE,
        main = "The average number of ratings given to the beers",
        ylab = "Rating Count")
points(mean(colCounts(beer_rating)),col="red",pch=18) 
text(mean(colCounts(beer_rating)), paste ('Mean' , round(mean(colCounts(beer_rating)),2) , sep = ' ' ), 
     labels = formatC(main1 , format = "f", 
                      digits = 1),
     pos = 2, cex = 0.9, col = "red") 
# 143.4
 
######################################################################
#The average number of ratings given by the users 
summary(rowCounts(beer_rating))

boxplot(rowCounts(beer_rating), boxfill = "skyblue", 
        outline = FALSE,
        main = "The average number of ratings by Users",
        ylab = "Rating Count") 
points(mean(rowCounts(beer_rating)),col="red",pch=18) 
text(mean(rowCounts(beer_rating)), paste ('Mean' , round(mean(rowCounts(beer_rating)),2) , sep = ' ' ), 
     labels = formatC(main1 , format = "f", 
                      digits = 1),
     pos = 2, cex = 0.9, col = "red")
# 15.19

######################################################################
######################################################################
#--------------------------Recommendation models ----------------#
######################################################################
#Divide data into test 
scheme <- evaluationScheme(beer_rating[1:1000], method = "split", train = .8,
                           k = 2, given = 1, goodRating = 4)
scheme

algorithms <- list(
  "user-based CF1" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=5,
                                                 minRating=3
                                                 )), 
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score" ))
)


# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

class(results)
#Plot ROC Curve
plot(results, annotate = 1:4, legend="topleft")
# UBCF is definitely is better
######################################################################
# Recommendation of Top5 Beers as per the UBCF Model
Rec.model<-Recommender(beer_rating, method = "UBCF",
                       param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=3))

recommended.items.cokes <- predict(Rec.model, beer_rating["cokes",], n=5)
as(recommended.items.cokes, "list")
# Beer ID : "571"  "4083" "7183" "7971" "2940"
recommended.items.genog <- predict(Rec.model, beer_rating["genog",], n=5)
as(recommended.items.genog, "list")
# Beer ID : "875"   "599"   "1891"  "35281" "355" 
recommended.items.giblet <- predict(Rec.model, beer_rating["giblet",], n=5)
as(recommended.items.giblet, "list")
# Beer ID : "61"    "1010"  "2872"  "22787" "3558"
