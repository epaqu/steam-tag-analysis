##태그 별 게임 목록 매트릭스 생성
raw_data <- read.csv("C:/Users/Jaewoo/Desktop/raw.csv")
tagnum <- c(1)
Tags <- c("none")
for (i in 1:326)
{
	tagnum[i] <- strsplit(as.character(raw_data[i,1]), "\"")[[1]][2]
	Tags[i] <- as.character(raw_data[i,2])
}
dataframe <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[1], ".csv", sep=""))[2]
for (i in 2:326)
{
	temp <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[i], ".csv", sep=""))
	for (j in (dim(temp)[1]+1):11183)
	{
		temp[j,1] <- j
		temp[j,2] <- NA
	}
	dataframe <- data.frame(dataframe, temp)
}

##유니크 게임 목록 생성
install.packages("plyr")
library(plyr)
list <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[1], ".csv", sep=""))[2]
list[1,1] <- NA
list <- as.data.frame(list)
for (i in 2:326)
{
	temp_list <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[i], ".csv", sep=""))[2]
	temp_list[1,1] <- NA
	list <- rbind.fill(list,  temp_list)
}
games <- t(unique(list))
##original attempt below failed
list <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[1], ".csv", sep=""))[2]
list <- as.data.frame(list)
for (i in 2:326)
{
	list <- rbind.fill(list, read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[i], ".csv", sep=""))[2])
}
games <- as.character(list[!duplicated(list), ]) ##unique쓰면 type=data라서 이렇게 함.... 했었음
games <- t(games)

##games 와 dataframe 비교해서 games[i] 가 발생하면 interim 해당 좌표에 ++
interim <- matrix(0, length(games), length(Tags))
for (i in 1:length(games))
{
	for (j in 1:651)
	{
		if (games[i] %in% dataframe[,j])
		{
			interim[i,ceiling(j/2)] <- interim[i,ceiling(j/2)] + 1
		}
		j <- j + 1
	}
}

##interim 을 참조하여 tag-to-tag association list 를 생성하여 final에 저장
##games 게임 순서대로 출시연도를 정리한 releaseDate 을 생성
final01 <- NA
final02 <- NA
##final03 <- NA
index <- NA
for (i in 1:length(games))
{
	for (j in 1:length(Tags))
	{
		if (interim[i,j] > 0) ##전부 1 아니면 0임
		{
			index <- append(index, j)
		}
	}
	index <- index[-1]
	if (length(index) > 1)
	{
		for (k in 1:(length(index)-1))
		{
			for (n in (k+1):length(index))
			{
				final01 <- append(final01, Tags[index[k]])
				final02 <- append(final02, Tags[index[n]])
				##final03 <- append(final03, releaseDate[i])
			}
		}
	}
	index <- NA
}
final01 <- final01[-1]
final02 <- final02[-1]
##final03 <- final03[-1]
final <- data.frame(final01, final02)
write.csv(final, "Final_Link.csv")

#final을 읽어서 weighted edge list 로
freq <- NA
count <- 0
final <- read.csv("Final_Link.csv")
weighted <- unique(final)
for (i in 1:dim(weighted)[1])
{
	for (j in i:dim(final)[1])
	{
		if (weighted[i,1] == final[j,1] && weighted[i,2] == final[j,2])
		{
			count <- count +1
		}
	}
	freq[i] <- count
	count <- 0
}
##출력
weighted <- data.frame(weighted, freq)
write.csv(weighted, "weighted.csv")

##final 을 .csv로 저장하여 gephi로 network, clustering

##이후 서베이 진행
nominal var 1 = novice/expert
nominal var 2 = our_class/old_class
data = # of correct classifications
of course we ASSUME that the population distribution is approximately normal for all groups.
2 Way ANOVA to determine if the mean is any significantly different for different groups.
we wanna see:
mean(novice-our_class) > mean(novice-old_class)
mean(expert-our_class) >= mean(expert-old_class)
and, if possible,
mean(novice-our_class) := mean(expert-our_class)
 
 
