##반복되는 각 게임의 스타팅 인덱스 알아내기
list <- read.csv("final.csv")
index <- 1
for (i in 1:dim(list[1])[1])
{
	j <- 1
	while (list[i,2] == list[i+j,2])
	{
		j <- j+1
	}
	index <- append(index, i+j)
	i <- i+j-1
}
index <- unique(index)
index <- as.data.frame(index)
index <- t(index)
write.csv(index, "Index.csv")

##알아낸 인덱스 사용
library(plyr)
set <- as.data.frame(t(combn(list[index[1]:(index[2]-1),1], 2)))
set[3] <- list[1,3]
set[4] <- 0
set[5] <- 0
for (count in 2:(length(index)-1))
{
	if (index[count] != (index[count+1]-1))
	{
		temp_set <- as.data.frame(t(combn(list[index[count]:(index[count+1]-1),1], 2)))
		temp_set[3] <- list[index[count],3]
		temp_set[4] <- list[index[count],4]
		temp_set[5] <- list[index[count],5]
		set <- rbind.fill(set, temp_set)
	}
}
temp_set <- as.data.frame(t(combn(list[79089:79095,1],2)))
temp_set[3] <- list[79089,3]
temp_set[4] <- list[79089,4]
temp_set[5] <- list[79089,5]
set <- rbind.fill(set, temp_set)
write.csv(set, "edges.csv")

##parsing the dates
data <- read.csv("C:/Users/Jaewoo/Desktop/edges.csv")
for (i in 1:dim(data)[1])
{
	check <- strsplit(as.character(data[i,3]), split=", ", fixed=TRUE)[[1]]
	if (length(check) > 1)
		data[i,3] <- check[2]
}
write.csv(data, "fixed.csv")

##tagnum to tags
data <- read.csv("C:/Users/Jaewoo/Desktop/fixed.csv")
formatted_tags <- read.csv("C:/Users/Jaewoo/Desktop/Tag_Formatted.csv")
formatted_tags <- t(formatted_tags)
for (i in 1:dim(data)[1])
{
	data[i,1] <- formatted_tags[which(tagnum==data[i,1])]
	data[i,2] <- formatted_tags[which(tagnum==data[i,2])]
}
