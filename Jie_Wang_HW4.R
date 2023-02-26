library(tm)
library(stringr)
library(wordcloud)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering
library(textstem)  ## Needed for lemmatize_strings
library(amap)  ## for Kmeans
library(networkD3)

## load the document
NovelsCorpus <- Corpus(DirSource("New_txt"))
ndocs<-length(NovelsCorpus)
NovelsCorpus <- tm_map(NovelsCorpus, content_transformer(tolower))
NovelsCorpus <- tm_map(NovelsCorpus, removePunctuation)
NovelsCorpus <- tm_map(NovelsCorpus, removeWords, stopwords("english"))

MyStopWords <- c("and","like", "very", "can", "I", "also", "lot")
NovelsCorpus <- tm_map(NovelsCorpus, removeWords, MyStopWords)

str(NovelsCorpus)

summary(NovelsCorpus)
meta(NovelsCorpus[[1]])
meta(NovelsCorpus[[1]],5)

## Change the COrpus into a DTM, a DF, and  Matrix
Novels_dtm <- DocumentTermMatrix(NovelsCorpus,
                                 control = list(
                                   stopwords = TRUE, ## remove normal stopwords
                                   wordLengths=c(4, 15), ## get rid of words of len 3 or smaller or larger than 15
                                   removePunctuation = TRUE,
                                   removeNumbers = TRUE,
                                   tolower=TRUE,
                                   remove_separators = TRUE))

str(Novels_dtm)

DTM_mat <- as.matrix(Novels_dtm)
str(DTM_mat)

inspect(Novels_dtm)
Novels_DF <- as.data.frame(DTM_mat)
str(Novels_DF)

Novels_DF[c(1:6),c(1:6)]
summary(Novels_DF)

str(Novels_DF)
Novels_df1 <- colSums(Novels_DF)/77

barplot(Novels_df1[1:20], names.arg = colnames(Novels_DF[,c(1:20)]), 
        ylab = 'frequency',
        las=2, cex.names=1)


# split the dataframe
dispt_df <- Novels_DF[grep('disp', row.names(Novels_DF)), ]
str(dispt_df)

disp_tot <- as.data.frame(colSums(dispt_df))
disp_tot <- t(disp_tot)

wordcloud(colnames(Novels_DF), disp_tot, max.words = 100) # high freq words in disp data


Hamilton_df <- Novels_DF[grep('Hamilton', row.names(Novels_DF)), ]
str(Hamilton_df)

Hamilton_tot <- as.data.frame(colSums(Hamilton_df))
Hamilton_tot <- t(Hamilton_tot)

wordcloud(colnames(Novels_DF), Hamilton_tot, max.words = 100) # high freq words in Hamilton_tot data

Madison_df <- Novels_DF[grep('Madison', row.names(Novels_DF)), ]
str(Madison_df)

Madison_tot <- as.data.frame(colSums(Madison_df))
Madison_tot <- t(Madison_tot)

wordcloud(colnames(Novels_DF), Madison_tot, max.words = 100) # high freq words in Hamilton_tot data


Madison_tot_df <- as.data.frame(Madison_tot)
Madison_tot_df <- t(Madison_tot_df)
Madison_tot_df <- as.data.frame(Madison_tot_df)

######
str(Madison_tot_df)
colnames(Madison_tot_df) <- 'counts'

newdata <- Madison_tot_df[order(Madison_tot_df$counts, decreasing = T), ]

nw <- cbind.data.frame(rownames(Madison_tot_df), Madison_tot_df$counts)
colnames(nw) <- c('word','count')

nw <- nw[order(nw$count, decreasing = T),]
nw1 <- nw[1:10,]
barplot(nw1$count/nrow(Madison_df), 
        names.arg = nw1$word, 
        xlab = 'words', 
        ylab = 'frequency') # most freq words in Madison


#####
Hamilton_tot_df <- as.data.frame(Hamilton_tot)
Hamilton_tot_df <- t(Hamilton_tot_df)
Hamilton_tot_df <- as.data.frame(Hamilton_tot_df)
colnames(Hamilton_tot_df) <- 'counts'
nw_a <- cbind.data.frame(rownames(Hamilton_tot_df), Hamilton_tot_df$counts)
colnames(nw_a) <- c('word','count')
nw_a <- nw_a[order(nw_a$count, decreasing = T),]
nw2 <- nw_a[1:10,]
barplot(nw2$count/nrow(Hamilton_df), names.arg = nw2$word, 
        xlab = 'words', 
        ylab = 'frequency', 
        title('Hamilton Papers'),
        horiz=TRUE
        ) # most freq words in Madison


############################################


#####
distance1 <- get_dist(Novels_DF,method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB",
                                     mid = "white", high = "#FC4E07"))+
  ggtitle("Manhattan Based Distance Map")

## Silhouette
fviz_nbclust(Novels_DF, method = "silhouette",
             FUN = hcut, k.max = 9)

#####
library(ClusterR)
library(cluster)
install.packages('ClusterR')


plot(Novels_DF[c('will','states')], col=kmeansFIT_1$cluster)
plot(Novels_DF[c('upon','powers')], col=kmeansFIT_1$cluster, "K-means with 2 clusters")

plot(Novels_DF[c('people','will')], col=kmeansFIT_1$cluster, "K-means with 2 clusters")

points(kmeansFIT_1$centers[, c("upon", "will")], 
       col = 1:3, pch = 8, cex = 3) 

points(kmeansFIT_1$centers[, c("will", "states")], 
       col = 1:3, pch = 8, cex = 3) 


y_kmeans <- kmeansFIT_1$cluster

library(tidyr)
extract_numeric(rownames(Novels_DF))

m <- Novels_DF
rownames(m) <- extract_numeric(rownames(m))

m

clusplot(m[, c("will", "states")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster Papers"),
         xlab = 'will',
         ylab = 'states')

cm_b <- cm_a[-c(1:11),]

pred_disp <- cm_a[grep('dispt', row.names(cm_a)), ]
View(pred_disp)

num_Ham <- nrow(cm_a[grep('Hamilton', row.names(cm_a)), ])
num_Mad <- nrow(cm_a[grep('Madison', row.names(cm_a)), ])

pred_Ham <- cm_a[grep('Hamilton', row.names(cm_a)), ]
pred_Mad <- cm_a[grep('Madison', row.names(cm_a)), ]

x <- nrow(pred_Ham[pred_Ham$kmeansFIT_1.cluster == 2,])
y <- nrow(pred_Mad[pred_Ham$kmeansFIT_1.cluster == 1,])

pred_Ham$Tar <- 2
pred_Mad$Tar <-1

new <- rbind(pred_Ham, pred_Mad)
ab <- table(new$Tar, new$kmeansFIT_1.cluster)
ab <- as.data.frame(ab)

ctable <- as.table(matrix(c(8, 7, 5, 46), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
##################################################
##################################################
##################################################


#######
distMatrix_C <- dist(Novels_DF, method="cosine")
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=.7, hang=-30,main = "Cosine Sim")
rect.hclust(groups_C, k=10) 
