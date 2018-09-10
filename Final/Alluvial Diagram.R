final <- data2018
for (year in 2017:1993)
{
	temp <- NA
	for (i in 1:length(data2018[,1]))
	{
		j <- 1
		comparison <- get(paste("data", year, sep=""))
		while (as.vector(comparison[j,1]) != as.vector(data2018[i,1]) && j <= length(comparison[,1]))
		{
			j <- j+1
		}
		temp[i] <- comparison[j,2]
	}
	final <- data.frame(final, temp)
}


##b<-0:4;c<-0:5;d<-0:6
##temp <- expand.grid(b,d,d,c,b,b)
a <- c("Tactical", "Typical", "Atypical", "Thrilling")
b <- c("Atypical", "Explorative", "Thrilling", "Tactical", "Antiheroic", "Typical", "0")
c <- c("Isometric", "Simulation", "Mixed", "Thrilling", "Action", "Maniac", "0")
d <- c("Simulation", "RPG", "Thrilling", "TPS", "FPS", "0")
e <- c("Simulation", "Atypical", "Typical", "Thrilling", "0")
f <- c("Game", "Puzzle", "Horror", "Narrative", "0")
temp <- expand.grid(a,b,c,d,e,f)
temp[7] <- 0
exp <- read.csv("C:/Users/Jaewoo/Desktop/Project/ideal.csv")
for (i in 1:dim(exp)[1])
{
	set <- subset(temp, Var1==exp[i,2] & Var2==exp[i,3] & Var3==exp[i,4] & Var4==exp[i,5] & Var5==exp[i,6] & Var6 == exp[i,7])
	index <- as.numeric(rownames(set))
	temp[index,7] <- temp[index,7]+1
}

write.csv(temp, "experimental.csv")



library(MASS)
library(colorRamps)
library(alluvial)
experimental <- read.csv("C:/Users/Jaewoo/Desktop/Project/experimental.csv")
alluvial(experimental[,6:1], freq=experimental$Frequency,
         col = ifelse (experimental$Y2018=="Typical", "red", ifelse(experimental$Y2018=="Atypical", "blue", ifelse(experimental$Y2018=="Tactical", "yellow", "green"))),
         hide = experimental$Frequency == 0,
         cex = 0.7
)