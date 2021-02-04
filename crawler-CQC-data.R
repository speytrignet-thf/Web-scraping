#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,data.table,
               tibble,pbapply,pbmcapply,here,readxl,rvest)

rm(list = ls())

#First ten results function

CQC_first_results_pcode <- function(pc){

  url_first <- paste0("https://www.cqc.org.uk/search/services/care-homes?location=",
                      pc,
                      "&latitude=&longitude=&sort=default&la=&distance=15&mode=html")
  
  #Open first link
  CQC_read <- read_html(url_first)
  
  #Get name and rating
  names <- CQC_read %>%
    html_nodes(xpath="//span[@class='facility-name']") %>% 
    html_text()
  
  names.names <- gsub("[\n]([^\n]+)[\n].*", "\\1", names) %>% trimws(.)
  names.rating <- gsub(".*[\n]([^\n]+)[\n].*", "\\1", names) %>% trimws(.)
  
  #Distance
  dist <- CQC_read %>%
    html_nodes(xpath="//span[@class='distance']") %>% 
    html_text() %>%
    readr::parse_number(.)
  
  #Get address, number and provider
  addresses <- CQC_read %>%
    html_nodes(xpath="//p[@class='details']") %>%
    html_text()
  
  addresses.add <- strsplit(addresses, "\n") %>%
    lapply(., `[[`, 3) %>%
    unlist(.) %>%
    trimws(.)
  
  addresses.number <- strsplit(addresses, "\n") %>%
    lapply(., `[[`, 4) %>%
    unlist(.) %>%
    trimws(.)
  
  addresses.provider <- strsplit(addresses, "\n") %>%
    lapply(., `[[`, 5) %>%
    unlist(.) %>%
    trimws(.)
  
  #Collate into table
  
  output <- data.frame(postcode=pc,
                       name=names.names,
                       distance.miles=dist,
                       rating=names.rating,
                       address=addresses.add,
                       number=addresses.number,
                       provider=addresses.provider)
  
  return(output)
}

#Try out

CQC_first_results_pcode("E84PH")