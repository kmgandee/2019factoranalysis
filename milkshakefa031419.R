#Imported dataset, using head function to confirm which variables are available
head(indepmilkvarmarch14)
#Remove rows with missing values and keep only complete cases
indepmilkvarmarch14=indepmilkvarmarch14[complete.cases(indepmilkvarmarch14),]
#Running principal components analysis using princomp function to determine the number of factors, then running a summary and making scree plot to assess
indepmilk0314.pca <- princomp(indepmilkvarmarch14)
summary(indepmilk0314.pca)
plot(indepmilk0314.pca)
#Looking at these results we really only seem to have one factor but for the sake of actually running an analysis I’m trying to go with having 2
indepmilk0314.fa1 <- factanal(indepmilkvarmarch14, factors=2, rotation="varimax")
#error message says "Error in solve.default(cv) : system is computationally singular: reciprocal condition number = 1.06371e-17" so I'm guessing that means 2 factors is too many for it