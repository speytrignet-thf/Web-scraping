#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,data.table,
               tibble,pbapply,pbmcapply,here,readxl,rvest,
               downloader,curl,lubridate,hrbrthemes,plotly,
               ggthemes,scales)

rm(list = ls())

#Today's date
Sys.Date()

#Directory to save files
rawdatadir <- "M:/Analytics/NHS England Vaccinations/Weekly/"
rawdataparentdir <- "M:/Analytics/NHS England Vaccinations/"

#Vaccination data website
nhse_link <- "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/"

#Scrape all download links
xlxs_links <- read_html(nhse_link) %>%
  html_nodes(xpath="//a[contains(text(),'weekly')]/@href") %>%
  html_text() %>%
  as.data.frame() %>%
  dplyr::rename(.,link=.) %>%
  mutate(.,is_xlsx=str_detect(link, "xlsx")) %>%
  filter(.,is_xlsx==TRUE) %>%
  select(.,link)

#Download files (that haven't already been downloaded!)
already.there <- list.files(rawdatadir)
to.download <- xlxs_links$link[which((basename(xlxs_links$link) %in% already.there)==FALSE)]

setwd(rawdatadir)
for (k in 1:length(to.download)){
  curl::curl_download(to.download[k], destfile=basename(to.download[k]))
}
rm(nhse_link,xlxs_links,to.download,already.there)

#Read in and append all files into one
setwd(rawdatadir)
files.to.read <- list.files(rawdatadir)
files.to.read <- files.to.read[!str_detect(files.to.read, "~")]

#Sort files by date
file.dates.fct <- function(s){
  day <- file.dates[[s]][which(file.dates[[s]] %in% c("2020","2021"))-2]
  month <- file.dates[[s]][which(file.dates[[s]] %in% c("2020","2021"))-1]
  year <- file.dates[[s]][which(file.dates[[s]] %in% c("2020","2021"))]
  dmy <- paste(day,month,year,sep=" ")
  return(as.data.frame(dmy))
}

file.dates <- files.to.read %>% str_replace_all(.,".xlsx","") %>% strsplit(.,split="-")
file.dates.clean <- lapply(1:length(file.dates), file.dates.fct) %>%
  rbindlist(.) %>%
  mutate(.,dmy=lubridate::dmy(dmy))
files.to.read <- data.frame(file=files.to.read,date=file.dates.clean$dmy)
rm(file.dates,file.dates.clean)

#Read in Number by age group and dose

#Sheet names to read in
sheet.names <- data.frame(date=lubridate::ymd(c("2020-12-31","2021-01-07")),
                          sheetname=c("Tab1 Vaccinations by age","Total Vaccinations"))
files.to.read <- left_join(files.to.read,sheet.names,by="date") %>%
  mutate(.,sheetname=ifelse(date>ymd("2021-01-07"),"Vaccinations by Region & Age",sheetname))
rm(sheet.names)

#Cell ranges for each sheet
cell.ranges <- data.frame(date=lubridate::ymd(c("2020-12-31","2021-01-07","2021-01-14",
                                                "2021-01-21","2021-01-28")),
                          cellrange=c("B14:J100","B13:J100","B12:J100",
                                      "B12:J100","B12:L100"))
files.to.read <- left_join(files.to.read,cell.ranges,by="date") %>%
  mutate(.,cellrange=ifelse(date>ymd("2021-01-28"),"B12:Z100",cellrange))
rm(cell.ranges)

#List to store files
datasets_list <- vector(mode = "list", length = nrow(files.to.read))
names(datasets_list) <- files.to.read$date

#Read in files
setwd(rawdatadir)
for (k in 1:nrow(files.to.read)){
  filename <- paste0("NHSE_vaccinations_",files.to.read$date[k]) %>% str_replace_all(.,"-","_")
  # assign(filename,read_excel(files.to.read$file[k], sheet = files.to.read$sheetname[k],
  #                            range=files.to.read$cellrange[k],col_names=FALSE))
  datasets_list[[k]] <- read_excel(files.to.read$file[k], sheet = files.to.read$sheetname[k],
                                   range=files.to.read$cellrange[k],col_names=FALSE)
}

#Cleaning function to apply to each list element
clean_weekly_data_dose_age <- function(j){
  data <- datasets_list[[j]]
  date_list <- names(datasets_list)[j]
  if(date_list<lubridate::dmy('01-01-2021')) {
    trdata <- data[3,c(2:6,8:9)]
    names(trdata) <- c("date",
                       "under80_first","under80_second",
                       "over80_first","over80_second",
                       "allages_first","allages_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list==lubridate::dmy('07-01-2021')){
    trdata <- data[3,c(2:6,8:9)]
    names(trdata) <- c("date",
                       "under80_first","under80_second",
                       "over80_first","over80_second",
                       "allages_first","allages_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list>lubridate::dmy('07-01-2021')&
             date_list<=lubridate::dmy('21-01-2021')){
    trdata <- data[3,c(2:4,6:7)]
    names(trdata) <- c("date",
                       "under80_first","over80_first",
                       "under80_second","over80_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list==lubridate::dmy('28-01-2021')){
    trdata <- data[3,c(2:4,7:8)]
    names(trdata) <- c("date",
                       "under80_first","over80_first",
                       "under80_second","over80_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list>lubridate::dmy('28-01-2021')){
    trdata <- data[3,c(2:6,11:14)]
    names(trdata) <- c("date",
                       "under70_first","70to74_first","75to79_first","over80_first",
                       "under70_second","70to74_second","75to79_second","over80_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  }
}

clean_data_dose_age <- lapply(1:nrow(files.to.read),clean_weekly_data_dose_age) %>% rbindlist(.)

rm(datasets_list,files.to.read)

#Aggregate variables

#Under 80 variable
detach(package:plyr)
clean_data_dose_age <- clean_data_dose_age %>%
  mutate(.,ageband_bis=ifelse(!(age %in% c("over80","allages")),"under80",age),
         cumnumber.people=as.numeric(cumnumber.people)) %>%
  group_by(date,dose,ageband_bis) %>%
  summarise(cumnumber.people=sum(cumnumber.people)) %>%
  ungroup(.)

#Save data
fwrite(clean_data_dose_age, file = paste0(rawdataparentdir,"Summary/age_dose_summary.csv"), sep = ",")