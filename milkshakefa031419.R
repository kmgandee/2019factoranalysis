#install.packages("nFactors")
library(nFactors)
data0 <- read.table("~/Documents/indepmilkvarmarch14v2.csv", sep=",", header = T)
names(data)
myvars <- c("subid","w1avgrmr", "w1basdrive","w1basfun","w1basreward","w1bis","w1eetotal","w1restotal","w1tfeqf1", "w1tfeqf2","w1tfeqf3", "w1bffq_totalcal", "w1bffq_.fat","w1bffq_.sug", "w1cravehifat","w1craveswt", "w1cravestrch","w1cravefast","w1likehifat","w1likeswt", "w1likestrch","w1likefast", "w1rscd","w1rswf", "w1hungrydiff","w1fulldiff","w1foodreinforcement", "w1pleasantdiff", "w1ediblediff","w1wantdiff", "w1familiardiff","w1intensitydiff")
data <- data0[myvars]
summary(data)
vhead(data)
data$w1pleasantdiff <- as.numeric(data$w1pleasantdiff)
data$w1ediblediff <- as.numeric(data$w1ediblediff)
data$w1wantdiff <- as.numeric(data$w1wantdiff)
data$w1familiardiff <- as.numeric(data$w1familiardiff)
data$w1intensitydiff <- as.numeric(data$w1intensitydiff)
#Imported dataset, using head function to confirm which variables are available
head(indepmilkvarmarch14)
#Remove rows with missing values and keep only complete cases
data=data[complete.cases(data),]
#Running principal components analysis using princomp function to determine the number of factors, then running a summary and making scree plot to assess
data.pca <- princomp(data)
summary(data.pca)
plot(data.pca)
#Looking at these results we really only seem to have one factor but for the sake of actually running an analysis I’m trying to go with having 2
data.fa1 <- factanal(data, factors=2, rotation="varimax")
#error message says "Error in solve.default(cv) : system is computationally singular: reciprocal condition number = 1.06371e-17" so I'm guessing that means 2 factors is too many for it
ev <- eigen(cor(data))
ap <- parallel(subject=nrow(data), var=ncol(data),
              rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel = ap$eigen$qevpa)
plotnScree(nS)
data.fa1 <- factanal(data, factors=11, rotation="varimax")
print(data.fa1,digits=2,cutoff=.3,sort=TRUE)
#if loading on more than one factor pick the one with the highest loading 