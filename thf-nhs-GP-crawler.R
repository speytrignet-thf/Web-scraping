#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,data.table,
               tibble,pbapply,pbmcapply,here,readxl,rvest)

pcode <- "E81LN"
  
url_first <- paste0("https://www.nhs.uk/service-search/find-a-gp/results/",pcode)

#Open first link
GP_read <- read_html(url_first)

xpath_month <- paste0("//a[contains(text(),'",month,"')]/@href")

names <- GP_read %>%
  html_nodes(xpath="//h2[starts-with(@id,'orgname')]") %>%
  html_text()

addresses <- GP_read %>%
  html_nodes(xpath="//p[starts-with(@id,'address')]") %>%
  html_text()

[starts-with(@id,'CompanyCalendar')]
