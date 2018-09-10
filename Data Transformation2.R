##파일 통합본 생성
dataframe <- NA
for (i in 1:length(tagnum))
{
	temp <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[i], ".csv", sep=""))
	temp <- temp[,-1]
	col_title
	dataframe.append(
}

##태그 별 게임 목록 매트릭스 생성
for (i in 2:length(tagnum))
{
	temp <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[i], ".csv", sep=""))
	for (j in (dim(temp)[1]+1):7677)
	{
		temp[j,1] <- j
		temp[j,2] <- NA
	}
	dataframe <- data.frame(dataframe, temp)
}

##레퍼런스용 유니크 게임 목록 생성
install.packages("plyr")
library(plyr)
list <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[1], ".csv", sep=""))
list[1] <- list[1,2]
list <- list[-1,]
for (i in 2:length(tagnum))
{
	temp_list <- read.csv(paste("C:/Users/Jaewoo/Documents/dat_", tagnum[i], ".csv", sep=""))
	temp_list[1] <- temp_list[1,2]
	temp_list <- temp_list[-1,]
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
	for (j in 1:dim(dataframe)[2])
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
			}
		}
	}
	index <- NA
}
final01 <- final01[-1]
final02 <- final02[-1]
final <- data.frame(final01, final02)
write.csv(final, "Final_Link.csv")

#broken gephi 파일을 읽어서 weighted edge list 로
broken <- read.csv("C:/Users/Jaewoo/Desktop/broken.csv")
broken <- broken[,-3]
for (i in 1:dim(broken)[1])
{
	if (broken[i,3] > 1)
	{
		broken <- broken[-((i+1):(i+broken[i,3])),]
	}
}

#혹은 sorted 파일을 읽어서 weighted edge list 로
sorted <- read.csv("C:/Users/Jaewoo/Desktop/sorted.csv")
for (i in dim(sorted)[1]:2)
{
	if (sorted[i,1] == sorted[i-1,1] && sorted[i,2] == sorted[i-1,2])
	{
		sorted[i-1,3] <- sorted[i-1,3] + sorted[i,3]
		sorted <- sorted[-i,]
	}
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
 
 
