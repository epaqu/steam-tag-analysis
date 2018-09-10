library(rvest)
Tags <- read.csv("태그 경로")
tagnum <- read.csv("태그넘 경로")
tagnum <- t(tagnum)
Tags <- t(Tags)
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
codes_in_tag <- NA
codes <- NA

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
						codes[k] <- game_code
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
			codes_in_tag <- append(codes_in_tag, codes)
			title <- NA
			dates <- NA
			date <- NA
			positive <- NA
			negative <- NA
			codes <- NA
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
					codes[k] <- game_code
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
		codes_in_tag <- append(codes_in_tag, codes)
		title <- NA
		dates <- NA
		date <- NA
		positive <- NA
		negative <- NA
		codes <- NA
	}
	write.csv(unique(data.frame(list_in_tag, codes_in_tag, date_in_tag, pos_in_tag, neg_in_tag)), paste("dat_", list_in_tag[1], ".csv", sep=""))
	list_in_tag <- NA
	codes_in_tag <- NA
	date_in_tag <- NA
	pos_in_tag <- NA
	neg_in_tag <- NA
}