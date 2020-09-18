###### Development ######

#Selenium section not complete yet

###### Libraries ######

if (!require("pacman")) install.packages("pacman")

pacman::p_load(RSelenium, seleniumPipes, tidyverse,dplyr,rvest,stringr,qdapRegex,tidyr,purrr,pbapply,data.table,readr)

rm(list = ls()) #Clear the working environment

#### Translating TripAdvisor HTML

map.code.stars <- data.frame(rating=c("Terrible - one star",
                                      "Poor - two stars",
                                      "Average - three stars",
                                      "Good - four stars",
                                      "Excellent - five stars"),
                             stars=1:5,
                             codes=c("1Nb7S6iu",
                                     "1LNKWH49",
                                     "17GUOtor",
                                     "7BtjwjjB",
                                     "dnQ8TT87"))

###### Search for all hotel listings within a city

###### Using Selenium to find the list of all hotels in a city

        ### Which location?

hotel.city <- "London, UK"

hotel.area <- "Euston"

        ### Run this first, in case a browser is open

driver$server$stop()

        ### Open the page

driver <- rsDriver(browser = c("firefox"))

remote_driver <- driver[["client"]]

remote_driver$navigate("https://www.tripadvisor.co.uk/")

        ### Find and click the 'search city' button

findcity_button_element <- remote_driver$findElement(using = 'class', value = "_3qLQ-U8m")

findcity_button_element$clickElement()

        ### Write the city name in the input

entercity_element <- remote_driver$findElement(using = 'class', value = '_3qLQ-U8m')

entercity_element$sendKeysToElement(list(hotel.city))

        ### Find and click the first result, maybe check it matches closely

firstresult_element <- remote_driver$findElement(using = "xpath", "//a[@class='bY-HBBDv'][2]")

firstresult_element$clickElement()

      ### Get on hotel page

hotelsbutton_element <- remote_driver$findElement(using = "xpath", "//a[@class='brand-quick-links-QuickLinkTileItem__link--1k5lE']")

hotelsbutton_element$clickElement()

      ### More areas

moreareasbutton_element <- remote_driver$findElement(using = "xpath", "//div[@data-param='distFrom']//div[@class='_3kI1z_wP v8kb8R34']")

moreareasbutton_element$clickElement()

      ### Give more detail on extra areas

entersubcity_element <- remote_driver$findElement(using = 'class', value = 'Smftgery')

entersubcity_element$clickElement()

entersubcity_element$sendKeysToElement(list(hotel.area))

      ### Click on first suggestion (not working)

subcity_firstresult_element <- remote_driver$findElement(using = "xpath", "//label[@class='_1dM16HHh mTd-BA8I']")

      ### Get link for hotels page

metalistings <- remote_driver$getCurrentUrl()[[1]]

        ### select hotels

#class="common-typeahead-results-BasicResult__result--1mjnd common-typeahead-results-BasicResult__active--1wjd4"
#<input type="text" class="Smftgery" placeholder="Where to?" autocomplete="on" value="" style="font-size: 24px; line-height: 1.7em;">

###### Needs to be done within Selenium

metalistings <- "https://www.tripadvisor.co.uk/Hotels-g186411-Leeds_West_Yorkshire_England-Hotels.html"

links.to.pages <- read_html(metalistings) %>%
  html_nodes('.property_title.prominent') %>%
  html_attr("href")

links.to.pages <- paste0("https://www.tripadvisor.co.uk",links.to.pages)

###### Start with TripAdvisor homepage of desired hotel ######

#### Homepage URL

#iamarriott_url <- "http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-JW_Marriott_Indianapolis-Indianapolis_Indiana.html"

#home_url <- "https://www.tripadvisor.co.uk/Hotel_Review-g187069-d191562-Reviews-Britannia_Manchester_Hotel-Manchester_Greater_Manchester_England.html"
#home_url <- "https://www.tripadvisor.co.uk/Hotel_Review-g186338-d195224-Reviews-London_Hilton_on_Park_Lane-London_England.html"

home_url <- "https://www.tripadvisor.co.uk/Hotel_Review-g187069-d191553-Reviews-Britannia_Sachas_Hotel-Manchester_Greater_Manchester_England.html"  

#### #### #### Function 1: only summary statistics in the first page

summary.hotel.function <- function(homepage){
  
  #### Wait between 1 and 3 seconds
  Sys.sleep(round(runif(1,1,3)))
  
  #### Read summary page
  
  read.first.page <- read_html(homepage)
  
  #### Location of hotel

  addresses <- read.first.page %>%
    html_nodes(xpath="//span[@class='public-business-listing-ContactInfo__ui_link--1_7Zp public-business-listing-ContactInfo__level_4--3JgmI']") %>%
    html_text()
  
  addresses <- addresses[1]
  
    #Split address into words/stubs
  addresses.split <- str_split(addresses, " ", simplify = TRUE)
  
    #Mix of letters and words
  addresses.split <- addresses.split[1,grepl("\\d", addresses.split)]
  addresses.split <- addresses.split[grepl("[A-z]", addresses.split)]
  
  if (length(addresses.split)==1){
    pcode <- addresses.split[1]
  } else {
    pcode <- paste0(addresses.split[1],addresses.split[2])
  }
  
  #### Overall number of reviews
  
  number.reviews.overall <- read.first.page %>%
    html_nodes('.hotels-hotel-review-atf-info-parts-Rating__reviewCount--1sk1X') %>%
    html_text() %>%
    str_replace_all(., " reviews", "") %>%
    str_replace_all(., ",", "") %>%
    as.numeric()
  
  #### Overall ranking in area
  
  area.ranking <- read.first.page %>%
    html_nodes('.hotels-hotel-review-atf-info-parts-PopIndex__popIndex--1Nei0') %>%
    html_text()
  
  number.ranking <- rm_between(area.ranking, "#", ' of', extract=TRUE,include.markers=FALSE) %>% unlist() %>% 
    str_replace_all(., ",", "") %>% as.numeric()
  
  number.ranking.total <- rm_between(area.ranking, "of ", 'Hotels', extract=TRUE,include.markers=FALSE) %>% unlist() %>%
    str_replace_all(., ",", "") %>% as.numeric()
  
  number.ranking.area <- sub('.*in ', '', area.ranking)
  
  #### Star reviews from summary in first page
  
  star.reviews.overall <- read.first.page %>%
    html_nodes('.hotels-review-list-parts-ReviewRatingFilter__row_num--gIW_f') %>%
    html_text() %>%
    str_replace_all(., ",", "") %>% 
    as.numeric()
  
  total.reviews <- data.frame(rating=paste0("star",5:1),
                              number=star.reviews.overall)
  
  total.reviews.wide <- tidyr::spread(total.reviews,rating,number)
  
  avg.rating <- sum((5:1)*total.reviews$number)/sum(total.reviews$number)
  
  #### Output
  
  page.output <- data.frame(number.reviews.overall,
                            area.ranking,
                            number.ranking,
                            number.ranking.total,
                            number.ranking.area,
                            total.reviews.wide,
                            avg.rating,
                            pcode)
  
  return(page.output)
}

summary.hotel.function(home_url)

#### #### #### Function 2: get as many individual reviews as desired

#### Create auxiliary function that generates URLs for subsequent pages

make.tripadvisor.url <- function(homepage,page_nr){
  
  stub1 <- paste0(strsplit(homepage,"-Reviews-")[[1]][1],"-Reviews-")
  
  if (page_nr==1){
    input1 <- ""
  } else {
    input1 <- paste0("or",(page_nr-1)*5,"-")
  }
  
  stub2 <- strsplit(homepage,"-Reviews-")[[1]][2]
  
  out.url <- paste0(stub1,input1,stub2)
  
  return(out.url)
  
}

#### Create auxiliary function for each review page

tripadvisor.hotels.perpage <- function(pagelist,k){
  
  #Wait between 1 and 3 seconds
  Sys.sleep(round(runif(1,1,3)))
  
  #Page to visit
  read.page.aux <- read_html(pagelist[k])

  #### Star review from individual posts
  
  star.reviews <- read.page.aux %>%
    html_nodes(xpath = "//div[@class='hotels-review-list-parts-RatingLine__bubbles--1oCI4']/span/@class") %>%
    html_text()
    
  star.reviews.clean <- strsplit(star.reviews,"tWzdVpH_ ") %>% unlist()
  
  star.reviews.clean <- star.reviews.clean[c(FALSE, TRUE)] %>% str_replace(.,"_","")
  
  star.reviews.clean <- data.frame(codes=star.reviews.clean)
  
  star.reviews.clean <- left_join(star.reviews.clean,map.code.stars,by="codes")
  
  #### Contributions and helpful votes from individual posts
  
  contributions.and.votes <- read.page.aux %>%
    html_nodes('.social-member-MemberHeaderStats__stat_item--34E1r') %>%
    html_text()
  
  contributions <- contributions.and.votes[which(grepl("contribution", contributions.and.votes)==TRUE)] %>%
   readr::parse_number()
  
  votes <- contributions.and.votes[which(grepl("helpful vote", contributions.and.votes)==TRUE)] %>%
    readr::parse_number()
  
  #### Review body from individual posts
  
  review.body <- read.page.aux %>%
    html_nodes('.hotels-review-list-parts-ExpandableReview__reviewText--3oMkH') %>%
    html_text()
  
  #### Date of review
  
  date.stay <- read.page.aux %>%
    html_nodes('.hotels-review-list-parts-EventDate__event_date--CRXs4') %>%
    html_text()
  
  date.stay.clean <- date.stay %>% str_replace_all(., "Date of stay: ", "")
  
  #### Name of reviewer
  
  reviewer.names <- read.page.aux %>%
    html_nodes('.ui_header_link.social-member-event-MemberEventOnObjectBlock__member--35-jC') %>%
    html_text()
  
  #### Put together data collected from 1 page
  
  page.output <- data.frame(reviewer.names,
                            contributions,
                            date.stay.clean,
                            star.reviews.clean,
                            review.body)
  
  return(page.output)
  
}

#### Then apply function 2 (inputs: home_url and number of reviews)

reviews.hotel.function <- function(homepage,chosen.nr.reviews){
  
  #### Run first function inside second one
  
  overall.stats <- summary.hotel.function(homepage)
  
  #### List of pages to visit
  
  max_pages <- ceiling(overall.stats$number.reviews.overall/5)
  
  chosen.nr.pages <- min(ceiling(chosen.nr.reviews/5),max_pages)
  
  list_of_pages <- purrr::map(1:chosen.nr.pages,make.tripadvisor.url,homepage=homepage) %>% unlist()
  
  #### Apply individual page function to all pages above
  
  gs_support <- 1:length(list_of_pages)
  
  gs_hotels=pblapply(gs_support,tripadvisor.hotels.perpage,pagelist=list_of_pages)
  
  hotels_out <- rbindlist(lapply(gs_hotels, as.data.table))
  
  return(hotels_out)
  
}

#### #### #### Example: Britannia Manchester - 25 reviews

hotel.example.one <- summary.hotel.function(home_url)

hotel.example.two <- reviews.hotel.function(home_url,50)

#Save

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Dropbox (Simetrica)/Simetrica Team Folder/Simetrica Data/Trip Advisor/")

fwrite(hotel.example.one, file = "Britannia-Manchester.csv")
fwrite(hotel.example.two, file = "Britannia-Manchester.csv")
