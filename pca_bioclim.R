df<-read.csv("bio_clim.csv")
df<-na.omit(df)

bio<-df[,2:20] #Extract only bioclimatic variables
bio_norm<-scale(bio) # Normalize data

corr_matrix <- cor(bio_norm) #Create correlation matrix

pca<-princomp(corr_matrix) #Using correlation matrix to conduct PCA
 
pca<-princomp(df[,2:20], cor = TRUE) #Conduct PCA using correlation matrix
summary(pca)

#Biplot plus contribution of each variable
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

