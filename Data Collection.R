install.package("rvest")
library(rvest)
Tags <- read.csv("태그 경로")
tagnum <- read.csv("태그넘 경로")
df <- data.frame(tagnum, Tags)
pagenum <- c(1)
list_in_tag <- NA
title <- NA
date <- NA
dates <- NA
date_in_tag <- NA
positive <- NA
negative <- NA
pos_in_tag <- NA
neg_in_tag <- NA

for (i in 1:317)
{
	list_in_tag[1] <- tagnum[i]
	web <- paste("http://store.steampowered.com/search/?sort_by=Name_ASC&category1=998&tags=", list_in_tag[1], "&page=1", sep="")
	htmlpage <- read_html(web)
	pagenum_html <- html_nodes(htmlpage, "#search_result_container > div.search_pagination > div.search_pagination_left");
	titles <- as.integer(strsplit(strsplit(html_text(pagenum_html), "of ")[[1]][2], "\t")[[1]][1])
	pagenum <- ceiling(titles/25)
	last <- titles %% 25
	if (pagenum > 1)
	{
		for (j in 1:pagenum-1)
		{
			web <- paste("http://store.steampowered.com/search/?sort_by=Name_ASC&category1=998&tags=", list_in_tag[1], "&page=", j, sep="")
			htmlpage <- read_html(web)
			for (k in 1:25)
			{
				add <- paste("#search_result_container > div:nth-child(2) > a:nth-child(", k+1, ") > div.responsive_search_name_combined > div.col.search_name.ellipsis > span", sep="")
				pagenum_html <- html_nodes(htmlpage, add)
				if (length(pagenum_html)==0)
					k <- k-1
				else
				{
					add <- paste("#search_result_container > div:nth-child(2) > a:nth-child(", k+1, ")", sep="")
					game_code <- html_nodes(htmlpage, add)
					if (length(game_code) != 0)
					{
						game_web <- paste("http://store.steampowered.com/app/", strsplit(gsub("^.*?app/", "", game_code), split="/", fixed=TRUE)[[1]][1], sep="")
						add <- "#game_highlights > div.rightcol > div > div.glance_ctn_responsive_left > div.release_date > span"
						date <- html_nodes(read_html(game_web), add)
						if (length(date)>0)
							dates[k] <- html_text(date)
						else
							dates[k] <- NA
						add <- paste("#search_result_container > div:nth-child(2) > a:nth-child(", k+1, ") > div.responsive_search_name_combined > div.col.search_reviewscore.responsive_secondrow > span", sep="")
						review <- html_nodes(htmlpage, add)
						if (length(review)>0)
						{
							score <- gsub("^.*?gt;", "", review)
							comment <- strsplit(score, split="% of the ", fixed=TRUE)[[1]]
							proportion <- as.integer(comment[1]) / 100
							n <- as.integer(strsplit(comment[2], split=" user", fixed=TRUE)[[1]][1])
							positive[k] <- round(proportion * n)
							negative[k] <- n - positive[k]
						}
						else
						{
							positive[k] <- 0
							negative[k] <- 0
						}
						title[k] <- html_text(pagenum_html)
					}
					else
						k <- k-1
				}
			}
			list_in_tag <- append(list_in_tag, title)
			date_in_tag <- append(date_in_tag, dates)
			pos_in_tag <- append(pos_in_tag, positive)
			neg_in_tag <- append(neg_in_tag, negative)
			title <- NA
			dates <- NA
			date <- NA
			positive <- NA
			negative <- NA
		}
	}
	if (last != 0)
	{
		web <- paste("http://store.steampowered.com/search/?sort_by=Name_ASC&category1=998&tags=", list_in_tag[1], "&page=", pagenum, sep="")
		htmlpage <- read_html(web)
		for (k in 1:last)
		{
			add <- paste("#search_result_container > div:nth-child(2) > a:nth-child(", k+1, ") > div.responsive_search_name_combined > div.col.search_name.ellipsis > span", sep="")
			pagenum_html <- html_nodes(htmlpage, add)
			if (length(pagenum_html)==0)
				k <- k-1
			else
			{
				add <- paste("#search_result_container > div:nth-child(2) > a:nth-child(", k+1, ")", sep="")
				game_code <- html_nodes(htmlpage, add)
				if (length(game_code) != 0)
				{
					game_web <- paste("http://store.steampowered.com/app/", strsplit(gsub("^.*?app/", "", game_code), split="/", fixed=TRUE)[[1]][1], sep="")
					add <- "#game_highlights > div.rightcol > div > div.glance_ctn_responsive_left > div.release_date > span"
					date <- html_nodes(read_html(game_web), add)
					if (length(date)>0)
						dates[k] <- html_text(date)
					else
						dates[k] <- NA
					add <- paste("#search_result_container > div:nth-child(2) > a:nth-child(", k+1, ") > div.responsive_search_name_combined > div.col.search_reviewscore.responsive_secondrow > span", sep="")
					review <- html_nodes(htmlpage, add)
					if (length(review)>0)
					{
						score <- gsub("^.*?gt;", "", review)
						comment <- strsplit(score, split="% of the ", fixed=TRUE)[[1]]
						proportion <- as.integer(comment[1]) / 100
						n <- as.integer(strsplit(comment[2], split=" user", fixed=TRUE)[[1]][1])
						positive[k] <- round(proportion * n)
						negative[k] <- n - positive[k]
					}
					else
					{
						positive[k] <- 0
						negative[k] <- 0
					}
					title[k] <- html_text(pagenum_html)
				}
				else
					k <- k-1
			}
		}
		list_in_tag <- append(list_in_tag, title)
		date_in_tag <- append(date_in_tag, dates)
		pos_in_tag <- append(pos_in_tag, positive)
		neg_in_tag <- append(neg_in_tag, negative)
		title <- NA
		dates <- NA
		date <- NA
		positive <- NA
		negative <- NA
	}
	write.csv(unique(data.frame(list_in_tag, date_in_tag, pos_in_tag, neg_in_tag)), paste("dat_", list_in_tag[1], ".csv", sep=""))
	list_in_tag <- NA
	date_in_tag <- NA
	positive_in_tag <- NA
	negative_in_tag <- NA
}

//저장할때 unique가 과연 일치할지??

문제0. html is deprecated
>> html(web) 대신 read_html(web) 으로 해결
문제1. 웹페이지를 새로 로드할때마다 순서가 바뀌어서 1페이지에서 나온 게임이 2페이지에서 나오는 등 중복/스킵 등이 발생함.
>> name순으로 정리한 페이지는 순서가 바뀔리가 없으므로 그 페이지로 바꿈.
문제2. 번들/DLC 등의 쓸데없는 데이터가 있으므로 data cleaning 필요
>> 가격을 받아야하나? no... F2P 는 어쩌게
문제3. 인터넷 커넥션 탓으로 가끔 정보가 누락되어 for loop이 깨짐... 어떻게 해야하나
// 에러가 나도 그냥 skip한 뒤 누락된 정보는 manually 기입하고 싶은데... 방법이 없나?
// 일단 에러는 pagenum_html의 length 가 0 일때 (정보가 누락되어) 발생하므로 그 경우만 if문으로 걸러내기로 함.
// print(c(list_in_tag[length(list_in_tag)], web))
>> 좀더 좋은 방법이 떠오름. length 가 0일때 k <- k-1 해버리면 재시도 하게 됨 ㅎㅎ.
문제4. 다 해결한 거 같은데 가끔 오류가 나서 NA로 스킵해버리는 페이지가 발생함
// 그냥 냅두고 manually 해결하지 뭐...
>> 데이터 크기의 문제였던지, 리스트를 여러개로 나눠 저장하니까 스킵하는 경우가 없어짐.
문제5. 데이터를 뽑긴 했는데 너무 커서 excel로 열 수도 없고 이건 뭐;
>> txt로 열리네. 그리고 R내부적으로도 access가능함
문제6. 왠진 몰라도 항상 데이터가 반복됨.
// 아 젤 큰 문제; 시발 뭐가 문젠질 모르겠음...
>> 그냥 unique(list)를 매 리스트마다 돌려버림.
문제7. Gothic 태그 전까지 밖에 데이터가 안 모임. 너무 커서
// 2번에 나눠서 구해야 할듯
>> 마찬가지로 리스트를 여러개로 나누는 것으로 문제가 해결됨.

>>>>> 새로 고안한 방법으로 전부 동시 해결 가능해보임.
>>>>> 해결됨.


EDIT 후 문제
P1. 504 Gateway timeout error