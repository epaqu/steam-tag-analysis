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

for (i in 1:length(Tags))
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
                    title[k] <- html_text(pagenum_html)
            }
            list_in_tag <- append(list_in_tag, title)
            title <- NA
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
            if (length(pagenum_html)==0 || length(game_code)==0)
                k <- k-1
            else
                title[k] <- html_text(pagenum_html)
        }
        list_in_tag <- append(list_in_tag, title)
        title <- NA
    }
    write.csv(unique(list_in_tag), paste("dat_", list_in_tag[1], ".csv", sep=""))
    list_in_tag <- NA
}