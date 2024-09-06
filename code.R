library("ggplot2")
library("dplyr") #for data manipulation
library("fastDummies")
library("cluster")
library("factoextra")
library("plotly")
library("pheatmap")
library("igraph")
library("mclust")
library("dbscan")
library("tidyverse")
library("FeatureImpCluster")
library("flexclust")
library("outliers")
library("attempt")
library("infotheo")
library("MASS")
library(seriation)
library(cluster)
library(esquisse)

data <- read.csv("adult/adult.data", header = FALSE)
colnames(data) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", 
                    "race", "sex", "capital-gain", "capital-loss", "hours-per-week", 
                    "native-country", "income")
View(data)
write.csv(data, "data.csv", row.names = FALSE)
nrow(data)


#PREPROCESSING ----
data[data==""]<-NA
data[data==" ?"]<-NA

data$race[data$race == ' Other'] <- NA
data$race[data$relationship == ' Other-relative'] <- NA
data$race[data$occupation == ' Other-service'] <- NA

#before dropping
colSums(is.na(data))
nrow(data)

data<-data[complete.cases(data), ] #dropping columns with NA, can handle them separately later 

#after dropping
colSums(is.na(data))
nrow(data)
colnames(data)

#Visualizing data

#checking impact of various features on income


ggplot(data) +
  aes(x = `native-country`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title='Income Distribution by Native Country') +
  theme(plot.title = element_text(hjust = 0.5))

filtered_countries <- data %>%
  group_by(`native-country`) %>%
  summarise(count=n()) %>%
  filter(count>0.02*nrow(data))

filtered_countries<- filtered_countries$`native-country`

clean_data <- data %>% 
  filter(`native-country` %in% filtered_countries)


theme_bw()
ggplot(clean_data) +
  aes(x = `native-country`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title='Income Distribution by Native Country')+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(clean_data) +
  aes(x = `race`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title='Income Distribution by Race')+
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(clean_data %>%
         mutate(`race`= recode(`race`, " White" = " White", " Black"= " People of Color", 
                               " Asian-Pac-Islander"= " People of Color", 
                               " Amer-Indian-Eskimo" = " People of Color"))) +
  aes(x = `race`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title='Income Distribution by Race')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data) +
  aes(x = workclass, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title='Income Distribution by Work-Class')+
  theme(plot.title = element_text(hjust = 0.5))
  


ggplot(clean_data, aes(x = age, fill = income)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Income distribution by Age",
       x = "Age",
       y = "Count") +
  scale_x_continuous(breaks = seq(min(clean_data$age), max(clean_data$age), by = 5))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data, aes(x = fnlwgt, fill = income)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Income distribution by fnlwgt",
       x = "fnlwgt",
       y = "Count")+
  scale_x_continuous(breaks = seq(min(clean_data$fnlwgt), max(clean_data$fnlwgt), by = 5))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data) +
  aes(x = education, fill = income) +
  geom_bar() +
  labs(title = 'Income Distribution by Education Level') +
  scale_fill_hue(direction = 1)+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data) +
  aes(x = reorder(education, match(education, c(" Preschool", " 1st-4th", " 5th-6th", " 7th-8th", " 9th", " 10th", " 11th", " 12th", " HS-grad", " Some-college", " Assoc-acdm", " Assoc-voc", " Bachelors", " Masters", " Prof-school", " Doctorate"))), fill = income) +
  geom_bar() +
  labs(title = 'Income Distribution by Education Level', x="education") +
  scale_fill_hue(direction = 1) +
  theme(plot.title = element_text(hjust = 0.5))

  

ggplot(clean_data) +
  aes(x = `education-num`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1)+
  labs(title = 'Income Distribution by Education Level') +
  scale_x_continuous(breaks = unique(clean_data$`education-num`), labels = unique(clean_data$`education-num`)) +
  theme(plot.title = element_text(hjust = 0.5))
  

ggplot(clean_data)+
  aes(x = `marital-status`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = 'Income Distribution by Marital Status')+
  theme(plot.title = element_text(hjust = 0.5))



ggplot(clean_data %>% mutate(`marital-status`= recode(`marital-status`, " Married-civ-spouse" = " Married",
                                       " Married-spouse-absent" = " Married", " Married-AF-spouse"=' Married',
                                       " Never-married" = " Never-married", " Divorced"= ' Divorced',
                                       " Separated" = " Divorced", " Widowed" = " Widowed"))) +
  aes(x = `marital-status`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = 'Income Distribution by Marital Status')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data) +
  aes(x = `relationship`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = 'Income Distribution by Relationship')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data) +
  aes(x = `occupation`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1)+
  labs(title = 'Income Distribution by Occupation')+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(clean_data) +
  aes(x = `sex`, fill = income) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = 'Income Distribution by Sex')+
  theme(plot.title = element_text(hjust = 0.5))
  

ggplot(clean_data, aes(x = `capital-gain`, fill = income)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Income Distribution by Capital Gain")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(clean_data, aes(x = `capital-loss`, fill = income)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Income Distribution by Capital Loss")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(clean_data, aes(x = `hours-per-week`, fill = income)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Income Distribution by Work Hours Per Week")+
  scale_x_continuous(breaks = seq(min(data$`hours-per-week`), max(data$`hours-per-week`), 
                                  by = 5))+
  theme(plot.title = element_text(hjust = 0.5))



#dropping extreme values based on visualization

main_data<-data%>%
  filter(`native-country`== " United-States")

main_data <- main_data %>%
  mutate(`race`= recode(`race`, " White" = " White", " Black"= " People of Color", 
                        " Asian-Pac-Islander"= " People of Color", 
                        " Amer-Indian-Eskimo" = " People of Color"))

main_data <- main_data %>%
  filter(!(workclass %in% c(" Without-pay") ))

unique(main_data$workclass)

main_data <- main_data %>%
  filter(!(`education-num` %in% c(1,2,3,5,8)))



main_data <- main_data %>%
  mutate(`marital-status`= recode(`marital-status`, " Married-civ-spouse" = " Married",
                                  " Married-spouse-absent" = " Married", " Married-AF-spouse"=' Married',
                                  " Never-married" = " Never-married", " Divorced"= ' Divorced',
                                  " Separated" = " Divorced", " Widowed" = " Widowed"))

main_data <- main_data %>%
  filter(!(occupation %in% c(" Priv-house-serv"," Armed-Forces")))



#dropping education, native country, capital-gain, capital loss, and fnlwgt based on visualization
main_data <- main_data[,!(names(main_data) %in% c('education','native-country','capital-gain','capital-loss'
                                             ,'fnlwgt'))]

names(main_data)

#separating numeric, ordinal, and nominal data

numeric_data<-main_data %>% select_if(is.numeric)
numeric_data<- numeric_data[,!(names(numeric_data) %in% c('education-num')),drop=FALSE] # ordinal
View(numeric_data)
nominal_data<-main_data %>% select_if(is.character)

ordinal_data <- main_data[,c("education-num"),drop=FALSE]
View(ordinal_data)
one_hot_encoded <- dummy_cols(nominal_data,remove_most_frequent_dummy = TRUE) #can try the other parameter as well
View(one_hot_encoded)
one_hot_encoded <- one_hot_encoded[,-c(1:7)]

#binding data back
main_data <- cbind(numeric_data, ordinal_data, one_hot_encoded)

#Sampling ----
set.seed(257) # For reproducibility
sample_size <- floor(0.15* nrow(main_data))

#Scaling using z scores ----

# Compute Z-scores for each feature
scaled_data <- as.data.frame(scale(sampled_data))
View(scaled_data)

#remove constant data
constant_columns <- apply(scaled_data, 2, var) == 0
constant_columns
filtered_data <- scaled_data[, !constant_columns]

#feature selection ----
#random forest

# Using Random Forest for feature importance
library(randomForest)

# Train a random forest model
rf_model <- randomForest(filtered_data, importance = TRUE)

# Get feature importance
importance <- importance(rf_model)

# Sort features by importance
sorted_importance <- importance[order(-importance[,1]),]
sorted_importance
selected_features <- rownames(sorted_importance[c(2,3,4,7,10,12,14,27),])
selected_features
selected_data <- filtered_data[,selected_features]

# Remove data points with Z-scores above threshold (e.g., 3 or -3)
threshold <- 3
selected_data <- selected_data[apply(abs(selected_data) < threshold, 1, all), ]


#Clustering ----

#validating cluster tendency

#VAT
VAT(dist(selected_data))

#hopkins statistic
set.seed(2)
get_clust_tendency(selected_data, n = 100)

#VAT

VAT(dist(sample_data), col = bluered(100))

#pca
myPr <- prcomp(selected_data)
pca_scores <- myPr$x

zero_var_cols <- sapply(pca_scores, function(col) var(col, na.rm = TRUE) == 0)

zero_var_cols
data2 <- cbind(selected_data, myPr$x[,1:2])
head(data2)

ggplot(pca_scores, aes(PC1, PC2))+
  geom_point()


#kmeans
#determining optimal number of clusters

#within cluster sum of squares
fviz_nbclust(pca_scores,kmeans,"wss",k.max=25)
?fviz_nbclust
#average silhouette width
fviz_nbclust(pca_scores,kmeans,"silhouette",k.max=25)

#gap statistic
fviz_nbclust(myPr$x,kmeans,"gap_stat")

k<-list()
betweenss_totss <- list()
?kmeans



for(i in 1:20){
  set.seed(3)
  k[[i]]<-kmeans(pca_scores, i,iter.max=25, nstart=25)
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}


plot(1:20,betweenss_totss, type='b', ylab='between SS/total SS')

#dendrogram

set.seed(4)
sub_selected_data <- pca_scores[sample(1:nrow(pca_scores),100),]

fit_hc<-hclust(dist(pca_scores,method="euclidean"),
               "ward.D2")
plot(fit_hc)
rect.hclust(fit_hc,h=22,border=c("red","blue","green","purple"))

fviz_dend(fit_hc,k=19)


#implementing kmeans
set.seed(3)
fit_km<-kmeans(pca_scores,21, iter.max=25, nstart=25)
graph1<-fviz_cluster(fit_km, selected_data,ellipse.type = "norm")
ggplotly(graph1)
plot(graph1)


#evaluating cluster quality
#square method
square_method <- fit_km$betweenss/fit_km$totss
square_method

#silhouette method
distance_matrix <- dist(pca_scores)

sihouette<-silhouette(fit_km$cluster,distance_matrix)


fviz_silhouette(sihouette)

#Davies bouldin
dbi_km <- index.DB(pca_scores, fit_dbscan$cluster, centrotypes = "centroids")$DB
cat("Davies-Bouldin index for k-means clustering: ", dbi_km)

#pca centers to original centers

pca_centers <- fit_km$centers
pca_centers
original_centers <- pca_centers %*% solve(myPr$rotation)
original_centers <- as.data.frame(original_centers)
original_centers


#pam
set.seed(8098120)
d <- dist(pca_scores)

fit_pam<-pam(d,k=21)

graph2 <- fviz_cluster(c(fit_pam, list(data = pca_scores)), geom = "point", ellipse.type = "norm")
ggplotly(graph2)


#evaluating cluster quality
sihouette2<-silhouette(fit_pam$clustering,distance_matrix)


fviz_silhouette(sihouette2)


#hcluster
set.seed(4)
fit_hc<-hclust(dist(pca_scores,method="euclidean"),
               "ward.D2")
#creating clusters
hc_clusters<-cutree(fit_hc,k=21)

#visualizing clusters
#method1
ggplotly(fviz_cluster(list(data=pca_scores, clusters=hc_clusters)))

#evaluating quality
sihouette3<-silhouette(hc_clusters,dist(pca_scores))


fviz_silhouette(sihouette3)
mean(sihouette3[,3])
ncol(selected_data)

#dbscan

set.seed(7878)
kNNdistplot(pca_scores,k=15)
abline(h=1.78, col="red", lty=2)

#eps=h, minPts=2*num of cols
set.seed(12019213)
fit_dbscan<-dbscan(pca_scores,eps=1.78,minPts=16, borderPoints =
                     FALSE)
g<-fviz_cluster(fit_dbscan, data = selected_data, 
                geom = "point", main = "DBSCAN Clustering")
ggplotly(g)
selected_features

#evaluating cluster quality

sihouette4<-silhouette(fit_dbscan$cluster,dist(pca_scores))
View(sihouette4)
sil_df <- as.data.frame(sihouette4[, 1:3])
sil_df<-sil_df %>%
  filter(cluster!=0)
fviz_silhouette(sihouette4)

#computing average silhouette width of all points except noise
mean(sil_df$sil_width)

#Davies Bouldin Index
library(clusterSim)
dbi_db <- index.DB(pca_scores, fit_dbscan$cluster, centrotypes = "centroids")$DB
cat("Davies-Bouldin index for Db-Scan clustering: ", dbi_db)

#Analysing centers of DBSCAN
#looking at centers
selected_data_clusters<- selected_data 
selected_data_clusters$cluster<-fit_dbscan$cluster

mean_values_df <- data.frame()

for(cluster in unique(selected_data_clusters$cluster)) {
  selected_data_cluster <- selected_data_clusters %>%
    filter(cluster == !!cluster)
  mean_values_cluster <- colMeans(selected_data_cluster, na.rm = TRUE)
  mean_values_cluster_df <- as.data.frame(t(mean_values_cluster))
  mean_values_cluster_df$cluster <- cluster
  mean_values_df <- rbind(mean_values_df, mean_values_cluster_df)
}

#exporting as csv

write.csv(mean_values_df, "mean_values_clusters.csv", row.names = FALSE)

#Plotting patterns ----
graph<-ggplot(selected_data_clusters %>% filter(cluster!=0),
              (aes(`race_ People of Color`,`income_ >50K`)))+
  geom_jitter(aes(fill=cluster),
              shape=21,
              alpha=0.3,
              size=5)+
  labs(title = 'Cluster destribution: Race VS Income')
names(selected_data_clusters)

graph2<-ggplot(selected_data_clusters %>% filter(cluster!=0),
               (aes(`sex_ Female`,`marital-status_ Divorced`)))+
  geom_jitter(aes(fill=cluster),
              shape=21,
              alpha=0.3,
              size=5)+
  labs(title = 'Cluster destribution: Sex VS Divorced Rates')

ggplotly(graph2)

graph3<-ggplot(selected_data_clusters %>% filter(cluster!=0),
               (aes(`sex_ Female`,`occupation_ Craft-repair`)))+
  geom_jitter(aes(fill=cluster),
              shape=21,
              alpha=0.3,
              size=5)+
  labs(title = 'Cluster destribution: Sex VS Craft Repair Occupation')

ggplotly(graph3)

graph4<-ggplot(selected_data_clusters %>% filter(cluster!=0),
               (aes(`occupation_ Prof-specialty`,`hours-per-week`)))+
  geom_jitter(aes(fill=cluster),
              shape=21,
              alpha=0.3,
              size=5)+
  labs(title = 'Cluster destribution: Occupation Professor VS Work Hours per Week')

ggplotly(graph4)



#Fuzzy clustering
install.packages("e1071")
library(e1071)

# Set the number of clusters
set.seed(121212)
num_clusters <- 21  # Adjust as needed

# Perform fuzzy c-means clustering
fuzzy_result <- cmeans(pca_scores, num_clusters, m = 2)

# Get cluster membership probabilities
## membership probabilities for each data point. These probabilities indicate the degree to which each data point belongs to each cluster.
membership_probabilities <- fuzzy_result$membership
membership_probabilities

# Get the cluster centers
cluster_centers <- fuzzy_result$centers
cluster_centers

summary(fuzzy_result)

original_centers <- cluster_centers %*% solve(myPr$rotation)
original_centers <- as.data.frame(original_centers)
original_centers

# Assign each data point to the cluster with the highest membership probability
cluster_assignments <- apply(membership_probabilities, 1, which.max)
cluster_assignments

# Visualize the cluster centers
plot(pca_scores, col = cluster_assignments, pch = 19)
points(cluster_centers, col = 1:num_clusters, pch = 8, cex = 2)


fpc <- sum(membership_probabilities^2) / nrow(pca_scores)

# Average silhouette width
library(cluster)
silhouette_width <- silhouette(cluster_assignments, dist(pca_scores))
# Extract average silhouette width
avg_sil_width <- mean(silhouette_width[, "sil_width"])
fviz_silhouette(silhouette_width)
print(avg_sil_width)

names(selected_data_clusters)

