# Libraries

library(sf)
library(rgeos)
library(RANN)
library(data.table)
library(readxl)
library(pbapply)
library(pbmcapply)
library(stringr)
library(dplyr)
library(tidyr)
library(rvest)    
library(purrr)

rm(list = ls())

#UK projection

latlong="+init=epsg:4326"
ukgrid = "+init=epsg:27700"

#Postcode centroids

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Dropbox (Simetrica)/Simetrica Team Folder/Projects/Edinburgh City Centre/Data/")

PostCodeCentroids <- fread("Postcode centroids/ONS_Postcode_Directory_Latest_Centroids.csv", header = T, sep = ',', data.table = T)

PostCodeCentroids$pcode <- str_replace_all(PostCodeCentroids$pcds, fixed(" "), "")

PostCodeCentroidsCoords <- cbind(PostCodeCentroids$long,PostCodeCentroids$lat)

Pcode_SP <- SpatialPointsDataFrame(PostCodeCentroidsCoords,
                                   data = data.frame(PostCodeCentroids$pcd,
                                                     PostCodeCentroids$lsoa11),
                                   proj4string = CRS(latlong))

#GP-s

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Dropbox (Simetrica)/Simetrica Team Folder/Simetrica Data/GP surgeries/epraccur/epraccur")

gp_surgeries <- as_tibble(fread("epraccur.csv", header = T, sep = ',', data.table = T))

gp_surgeries$pcode <- str_replace_all(gp_surgeries$pcode, fixed(" "), "")

gp_surgeries <- left_join(gp_surgeries,PostCodeCentroids,by="pcode")

gp_surgeries_df <- dplyr::filter(gp_surgeries,is.na(gp_surgeries$lat)==F&is.na(gp_surgeries$long)==F&
                                   lat<99&Setting==4) %>%
  dplyr::select(.,GP_name,pcode,lat,long,Setting) %>%
  dplyr::rename(.,name=GP_name)

###### As a function ######

###### Function with a single surgery URL as an input

get_nhs_reviews_page <- function(page_url){
  
  #i <- 1
  #gplx <- practices_url
  #page_url <- practices_url[1]
  #page_url <- "https://www.nhs.uk/Services/GP/Overview/DefaultView.aspx?id=42009"
  
  #Set a random waiting time (between 1 and 4 seconds)
  
  Sys.sleep(round(runif(1,1,3)))
  
  #Read page
  
  surgery_read_page <- read_html(page_url)
  
  #Satisfaction %
  
 pct_data <- surgery_read_page %>% 
    # The '.' indicates the class
    html_nodes('.indicator-value') %>% 
    # Extract the raw text as a list
    html_text()

 satis.pct <- ifelse(sum(grepl("%", pct_data)),pct_data[grepl("%", pct_data)],NA)

 N_pct_data <- surgery_read_page %>% 
   # The '.' indicates the class
   html_nodes('.indicator-text') %>% 
   # Extract the raw text as a list
   html_text()
 
  satis.Ns <- ifelse(sum(grepl("responses", N_pct_data)),N_pct_data[grepl("responses", N_pct_data)],NA)
  
  satis.N <- as.integer(stringr::str_extract(satis.Ns, "\\d+"))
  
  #Number of doctors
  
  staff_data <- surgery_read_page %>% 
    # The '.' indicates the class
    html_nodes('.staff-list') %>% 
    # Extract the raw text as a list
    html_text()
  
  staff_number <- as.numeric(str_count(staff_data, "Dr"))
  
  reviews.out <- cbind(satis.pct,satis.N,staff_number)
  
  return(reviews.out)
    
}

###### Function with a single URL as input (incl. 10 reviews)

get_nhs_listings_page <- function(j,lx){
  
  #lx=list_of_pages
  #j=2
  
  page_url <- lx[j]
  
  #Set a random waiting time (between 1 and 4 seconds)
  
  Sys.sleep(round(runif(1,1,3)))
  
  #Read page
  
  GP_read_page <- read_html(page_url)
  
  #Names of surgries
  
  names_data <- GP_read_page %>% 
    # The '.' indicates the class
    html_nodes('.fctitle') %>% 
    # Extract the raw text as a list
    html_text() %>%
    str_replace_all(.,"\r\n","") %>%
    trimws(., "both")
  
  #Distances
  
  distances_data <- GP_read_page %>% 
    # The '.' indicates the class
    html_nodes('.fcdirections') %>% 
    # Extract the raw text as a list
    html_text() %>%
    str_replace_all(.," miles away\r\n","") %>%
    str_replace_all(.," ","") %>%
    str_replace_all(.,"\"","") %>%
    as.numeric()
  
  distances_km <- round(1.60934*distances_data,2)
  
  #Number of patients
  
  numbersp_data <- GP_read_page %>% 
    # The '.' indicates the class
    html_nodes('.fc-shaded') %>% 
    # Extract the raw text as a list
    html_text()
  
  nrpatients_data <- numbersp_data[seq(1, 30, by=3)] %>% str_replace_all(.,"\r","") %>%
    str_replace_all(.,"\n","") %>% str_replace_all(.," ","") %>% str_replace_all(.,"/","") %>%
    gsub("[^0-9.]", "", .)
  
  #Link to all practice links within this page
  
  reviewslinks <- GP_read_page %>% 
    # The '.' indicates the class
    html_nodes('.fctitle a') %>% 
    # Extract the raw text as a list
    html_attr('href')
  
  nhs.root <- "https://www.nhs.uk"
  
  practices_url <- str_c(nhs.root, reviewslinks)
  
  #Get all the patient review data using a function
  
  reviews.df=pblapply(practices_url,get_nhs_reviews_page)
  reviews.out <- rbindlist(lapply(reviews.df, as.data.table))
  
  #Output
  
  GP_output_page <- cbind.data.frame(names_data,distances_km,nrpatients_data,reviews.out)
  
  return(GP_output_page)
  
}

###### Function for all listings (final function to use: the input is the postcode and
###### the function runs through all the pages of results)

get_nhs_listings <- function(pcarg,maxresults){
  
  #pcarg <- "N43RY"
  #maxresults <- 20
  
  practice_long <- dplyr::filter(PostCodeCentroids,pcode==pcarg)$long
  practice_lat <- dplyr::filter(PostCodeCentroids,pcode==pcarg)$lat
  
  url_first <- paste0("https://www.nhs.uk/service-search/GP/London/Results/4/",practice_long,"/",practice_lat,"/4/13136?distance=25")
  
  #Open first link
  GP_read <- read_html(url_first)
  
  #Number of search results
  total_results <- GP_read %>% 
    # The '.' indicates the class
    html_nodes('.fcresultsinfo.clear') %>% 
    # Extract the raw text as a list
    html_text() %>%
    str_match(., " of (.*?) results")
  
  total_results <- as.numeric(total_results[1,2])
  
  #List of URLs to visit (max. 10 first pages for example)
  max_pages <- min(ceiling(maxresults/10),ceiling(total_results/10))
  
  url_page <- paste0(url_first,"&ResultsOnPageValue=10&isNational=0&totalItems=",total_results,"&currentPage=")
  
  list_of_pages <- str_c(url_page, 2:max_pages)
  list_of_pages <- c(url_first,list_of_pages)
  
  #Launch per-page function and return as a large data frame
  
  gs_support <- 1:max_pages
  gs_nhs=pblapply(gs_support,lx=list_of_pages,get_nhs_listings_page)
  nhs_out <- rbindlist(lapply(gs_nhs, as.data.table))
  
  return(nhs_out)

}

###### Test function

ShepherdsBush <- get_nhs_listings("W127FD",maxresults=100)

#Save

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Dropbox (Simetrica)/Simetrica Team Folder/Projects/Wellbeing Index/Data Sources/")

fwrite(ShepherdsBush, file = "NHS-gp-surgeries-ShepherdsBush.csv")
