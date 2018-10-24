#!/usr/bin/env Rscript

oldw <- getOption("warn") #Warnings suppressed when running on the command line. Warnings unsupressed at end of script. Remove when developing/debugging
options(warn = -1)

library("optparse")
library('rvest')
library('stringr')
suppressPackageStartupMessages(library(tidyverse))
library("XML")
library("xml2")



##A helper function for getDraftData
##This function takes as input
#hasStats: One value for each row of the draft table. Does each row have a link
#statsLinks: vector of all stats links from the draft table
#This function produces a vector the length of the draft table with the link for players who have one, and NA for those who don't
matchLinks <- function(hasStats,statsLinks) {
  collegeVector <- rep(NA,length(hasStats))
  j=1
  for (i in 1:length(hasStats)) {
    if (hasStats[i]) {
      collegeVector[i] = statsLinks[j]
      j=j+1
    }
  }
  return (collegeVector)
}

#link: A link to a draft page
#This function outputs a table containing drafts stats for player and links to college stats, and the player page
getDraftData <- function(link) {
  doc <- read_html(link)
  
  #STEP 1: Get the table
  tbl <- doc %>% 
    html_table() %>% 
    as.data.frame() %>%
    filter(Rk !="Rk") %>%
    as.tibble() 
  
  #STEP 2: Get player links (also serves as ID for player)
  plrLinks <- doc %>% 
    html_nodes("tbody a[href^=\"/players/\"]") %>% 
    html_attr("href")
  #players <- tibble(plrLinks) %>%
    #filter(str_detect(plrLinks,"htm$"))
  #If the xpathSApply object has 4 elements, there is a link. If it only has 2, there is not
  hasNFLStats <- doc %>% htmlParse() %>%
    xpathSApply("//td[@data-stat='player']",xmlAttrs) %>%
    map(length) %>%
    map(~ifelse(.>2,TRUE,FALSE)) %>%
    unlist()
  
  players <- matchLinks(hasNFLStats,plrLinks) %>%
    as.tibble()
  
  #STEP 3: Get college stats links
  collegeStatsLinks <- doc %>%
    html_nodes("tbody [data-stat=college] a") %>%
    html_attr("href")
  
  #If the xpathSApply object has child elements, there is a link
  hasCollegeStats <- doc %>% htmlParse() %>%
    xpathSApply("//td[@data-stat='college']",xmlSize) %>%
    as.logical()

  collegeStats <- matchLinks(hasCollegeStats,collegeStatsLinks) %>%
    as.tibble()
  
  tbl2 <- bind_cols(tbl,players,collegeStats)
  names(tbl2)[17] = "plrLinks"
  names(tbl2)[18] = "clgLinks"
  tbl2 <- as.tibble(tbl2)
  
  return (tbl2)
  
}

##Helper function for the college and nfl scraping
#htmlRow: An html node that is a ROW (a tr) from a profootball reference table
#scrpStrings: A vector of css selectors that grab data from the row
#Returns a vector containing data collected based on each css selector
scrapeTableRow <- function(htmlRow,scrpStrings) {
  dat <- c()
  for (i in 1:length(scrpStrings)) {
    newDat <- htmlRow %>% 
      html_nodes(scrpStrings[i]) %>% 
      html_text()
    if (length(newDat)==0) {
      newDat = NA
    }
    dat <- c(dat,newDat)
  }
  return (dat)
}

#Helper function for college and NFL scraping
#Given a player page and a nodeCall (either asking for rookie or career info, grab the 
#respective rookie or career AV)
#doc: player page
#nodeCall: css selector (e.g [id*=\"rushing\"] tfoot tr:nth-child(1))
#Everything in this function is calling the table in the page (there is only one. All other
#tables are commented out!)
grabAV <- function(doc,nodeCall) {
  
  nodeCall <- nodeCall %>%
    str_extract("\\].*") %>%
    str_replace("]","table") %>%
    str_c(" td[data-stat=av]")
  
  av <- doc %>% 
    html_nodes(nodeCall) %>%
    html_text()
  
  if (length(av) == 0){
    av <- NA
  }
  
  return (av)
}

#Helper function for college and NFL scraping
#plrLinks: a vector of links to player pages (either college or NFL)
#collect: A vector containing the data elements that we want to collect from a row
#FUN: either getCollegeInfoPlayer or getNFLInfoPlayer
#tst: whether we are testing (TRUE or FALSE)
#g: number of testing iterations (to be used if testing)
scrapePlayers <- function(plrLinks,collect,FUN,tst,g) {
  iter <- length(plrLinks)
  if (tst) {
    iter <- g
  }
  
  result <- FUN(plrLinks[1],collect)
  for (i in 2:iter) {
    print(plrLinks[i])
    nxt <- FUN(plrLinks[i],collect)
    result <- rbind(result,nxt)
  }
  result = as.data.frame(result)
  return (result)
}


#e.g "https://www.sports-reference.com/cfb/players/jamaal-williams-1.html"
#Helper Function for getCollegeInfo
#For a particular player college link, return a vector with career college data
#collect: A vector of strings containing the data elements we want to collect from the row
getCollegeInfoPlayer <- function(clgLink,collect) {

  #For players who had No college link in the table
  if (is.na(clgLink)) {
    return (matrix(NA,nrow=1,ncol=length(collect)+2))
  }
  doc <- read_html(clgLink)
  pos <- "rushing"
  
  #First check if the rushing/receiving table has id: rushing
  if (length(doc %>% html_nodes(str_c("table","#",pos))) == 0) {
    #check if the rushing/receiving table has id: receiving
    if (length(doc %>% html_nodes(str_c("table","#","receiving"))) == 0) {
      #in this case there is no rushing/receiving table
      return (matrix(NA,nrow=1,ncol=length(collect)+2))
      #Note that the return for this function is a matrix. +2 for the links at the end
    }
    else {
      pos <- "receiving"
    }
  }
  
  cssPrefix <- str_c("table#",pos)

  
  scrpStrings <- str_c("td[data-stat=",collect,"]")
  
  lstYear <- doc %>%
    html_nodes(str_c(cssPrefix," tbody tr:nth-last-child(1)"))
  
  ##First we need to check that the lstYear row contains data
  #To do this we'll query the G entry
  gDat <- lstYear %>% 
    html_nodes(scrpStrings[1]) %>%
    html_text()
  
  #Indicates the last year contains No data. Then we jump to previous year
  #I believe this is just a problem with pro-football reference
  if (gDat == "") {
    lstYear <- doc %>%
      html_nodes(str_c(cssPrefix," tbody tr:nth-last-child(2)"))
  }
  
  clgDat <- scrapeTableRow(lstYear,scrpStrings)
  
  #Also grab the school and the conference
  schoolName <- doc %>%
    html_nodes(str_c(cssPrefix," tbody tr:nth-last-child(1) [data-stat=school_name] a")) %>%
    html_text()
  conf <- doc %>%
    html_nodes(str_c(cssPrefix," tbody tr:nth-last-child(1) [data-stat=conf_abbr] a")) %>%
    html_text()
  
  clgDat <- c(clgDat,c(schoolName,conf))
  
  clgDat <- clgDat %>%
    as.matrix() %>%
    t()
  
  return(clgDat)
}

#Try catch version of getCollegeInfoPlayer
getCollegeInfoPlayerTC <- function(clgLink,collect) {
  out <- tryCatch(
    {
      getCollegeInfoPlayer(clgLink,collect)
    },
    error = function(cond) {
      message("Here's the original error message:")
      message(cond)
      #2 extra links. Hence +2
      return (matrix(NA,nrow=1,ncol=length(collect)+2))
    }
  )
  return (out)
}


##This function loops through all college links, collects last year of college data and returns a data frame with this information
#plrLinks: vector of college links (coming from draft data table)
#v: The number iterations before save. ITERATED SAVE MAY BE BROKEN RIGHT NOW
#collect: A vector of strings containing the data elements we want to collect from the row
#tst: whether or not we are testing (boolean)
#g: Number of iterations in testing if we are testing
##e.g "~/Desktop/GradSchool/STAT6950/Project/Data/preClean.rda"
getCareerCollegeInfo <- function(plrLinks,v,collect,tst,g) {
  result <- scrapePlayers(plrLinks,collect,getCollegeInfoPlayerTC,tst,g)
  names(result) <- str_c("CLG.",c(collect,"college","conf"))
  #saveRDS(result,saveLoc)
  return (result)
}

#Helper function for getNFLInfoPlayer
#Gathers data from a specific row of the NFL table a player
#doc: an html data type that corresponds to a nfl player page
#collect: columns of the table to scrape
#nodeCall: which row to get (css selector)
nflInfoRow <- function(doc,collect,nodeCall) {
  scrpStrings <- str_c("td[data-stat=",collect,"]")
  
  ##GRAB THE AV [DONE SEPARATELY BECAUSE AV INFO IS ONLY IN THE UNCOMMENTED TABLE]
  ##NOTE THERE IS ONLY ONE UNCOMMENTED TABLE
  av <- grabAV(doc,nodeCall)
  
  
  #There is only one table in the document that is uncommented
  #We first check if the document contains an uncommented rushing table
  if (length(doc %>% html_nodes("table[id*=\"rushing\"]")) == 0) {
    #These documents DO NOT HAVE AN uncommented rushing table
    #So we check for a div containing rushing data
    if ((length(doc %>% html_nodes("div[id*=\"rushing\"]")) == 0)) {
      #These docs do not have such a div -there is no rushing table (commented or uncommented)
      #For these players, we can check if there is a games table. If there is, we can get
      #games dressed from this table
      if ((length(doc %>% html_nodes("table[id*=\"games\"]")) != 0)) {
        #Elijah Hood scenario
        nodeCall <- str_replace(nodeCall,"rushing","games")
      }
      else {
        #No rushing table (uncommented or commented), and no games table
        #Exists for two types of players. Those who never played RB/WR, but played in NFL
        #Those who never played in the NFL
        if ((length(doc %>% html_nodes("table"))) != 0) {
          #There is game data in the NFL link
          #1 for hasNFLdata
          return (c(rep(NA,length(collect)),av,1))
        }
        else {
          #There is no game data in the NFL link
          #0 for hasNFLData
          return (c(rep(NA,length(collect)),av,0))
        }
      }
    }
    else {
      #Grab the comment within the rushing div and turn it into html
      doc <- doc %>% 
        html_nodes("div[id*=\"rushing\"]") %>%
        html_nodes(xpath = "comment()") %>%
        html_text() %>%
        read_html()
    }
  }
  
  htmlRow <- doc %>%
    html_nodes(nodeCall)
  
  dat <- scrapeTableRow(htmlRow,scrpStrings)
  
  #The 1 is for hasNFLdata
  return (c(dat,av,1))
}

#Helper Function for getNFLInfo
#For a particular player link, return a vector with career NFL data
#collect: The row information in the NFl table to collect for each row
getNFLInfoPlayer <- function(plrLink,collect) {
  
  if (is.na(plrLink)) {
    #2 avs and 2 playedInNFLs, so +4
    #The 0 is referring to whether they have an NFL link
    return (cbind(matrix(NA,nrow=1,ncol=2*length(collect)+4),0))
  }
  
  fullLink <- str_c("https://www.pro-football-reference.com",plrLink)
  
  doc <- read_html(fullLink)
  
  nodeCallCareer <- "table[id*=\"rushing\"] tfoot tr:nth-child(1)"
  nodeCallRookie <- "table[id*=\"rushing\"] tbody tr:nth-child(1)"
  
  careerNFL <- nflInfoRow(doc,collect,nodeCallCareer)
  rookieNFL <- nflInfoRow(doc,collect,nodeCallRookie)
  
  nflDat <- c(careerNFL,rookieNFL)
  
  nflDat <- nflDat %>%
    as.matrix() %>%
    t()
  
  #The one is referring to that they have an NFL link
  nflDat <- cbind(nflDat,1)
  
  return(nflDat)
}

#Try-catch version of getNFLInfoPlayer
getNFLInfoPlayerTC <- function(plrLink,collect) {
  out <- tryCatch(
    {
      getNFLInfoPlayer(plrLink,collect)
    },
    error = function(cond) {
      message("Here's the original error message:")
      message(cond)
      #4 for two avs, and two has Game datas. One for has nfl link. Hence +5
      return (matrix(NA,nrow=1,ncol=2*length(collect)+5))
    }
  )
  return (out)
}

##This function loops through all player links, collects career NFL data and returns a data frame with this information
#plrLinks: vector of player links (coming from draft data table)
#collect: A vector containing the variables that should be collected from NFL row
#tst: Whether we are testing (TRUE OR FALSE)
#g: If we are testing, this is the number of iterations for testing
##e.g "~/Desktop/GradSchool/STAT6950/Project/Data/preClean.rda"
getNFLInfo <- function(plrLinks,collect,tst,g) {
  result <- scrapePlayers(plrLinks,collect,getNFLInfoPlayerTC,tst,g)
  names1 <- str_c("NFLCAR.",collect)
  names1 <- c(names1,"NFLCAR.av","NFLCAR.Data")
  names2 <- str_c("ROOKIE.",collect)
  names2 <- c(names2,"ROOKIE.av","ROOKIE.Data")
  names(result) <- c(names1,names2,"hasNFLLink")
  #saveRDS(result,saveLoc)
  
  return (result)
}

#Given a link to a draft table in pro-football focus, collect draft data and career NFL data
#example for saveLoc: "~/Desktop/GradSchool/STAT6950/Project/Data/preClean.rda"
collectData <- function(link,collectNFL,collectCollege,saveLocFinal) {
  draftData <- getDraftData(link)
  clgData <- getCareerCollegeInfo(draftData$clgLinks,10,collectCollege,FALSE,10)
  nflData <- getNFLInfo(draftData$plrLinks,collectNFL,FALSE,10)
  finalDat <- cbind(draftData,nflData)
  finalDat <- cbind(finalDat,clgData)
  print(saveLocFinal)
  saveRDS(finalDat,saveLocFinal)
  return (finalDat)
}

#Helper function for doScrape. Creates combine link, given pos, yearMin, yearMax
#pos: What position are we scraping data for (string: RB, FB, WR)
#year: The year we want to gather data for (string)
createLink <- function(pos,year) {
  link <- str_c("https://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1&year_min=",
                year,
                "&year_max=",
                year,
                "&pos%5B%5D=",pos,
                "&show=all&order_by=year_id")
  return (link)
}

#pos: What position are we scraping data for (string: RB, FB, WR)
#yearMin: The minimum year we want to gather data for (string)
#yearMax: The maximum year we want to gather data for (string)
#savePrefix: Location for saving (e.g "~/Desktop/GradSchool/STAT6950/Project/Data/)
doScrape <- function(pos,yearMin,yearMax,savePrefix) {
  
  if (!str_sub(savePrefix,-1)=="/") {
    savePrefix <- str_c(savePrefix,"/")
  }
  
  if(as.numeric(yearMin)<2000 | as.numeric(yearMin)>2018 | as.numeric(yearMax)<2000 | as.numeric(yearMax)>2018) {
    print("oops. The scraper can only collect data between 2000 and 2018. The reason data is not collected from before 2000 is because combine data is not available from before 2000. See the README for hints on how you might allow this functionality")
  }
  else if (pos!="RB" & pos!="WR") {
    print("oops. The scraper is only operating for running backs and wide recievers. See the README for hints on how you might allow this functionality")
  }
  else {
    #COLLECTION VARIABLES
    collectNFL <- c("g","gs","rush_att","rush_yds","rush_td","rush_long","rush_yds_per_att",
                    "rush_yds_per_g","rush_att_per_g","targets","rec_td","rec_per_g",
                    "rec_yds_per_g","yds_from_scrimmage","rush_receive_td","fumbles")
    collectCollege <- c("g","rush_att","rush_yds","rush_td","rush_yds_per_att",
                        "rec","rec_yds","rec_yds_per_rec","rec_td","scrim_att","scrim_yds",
                        "scrim_yds_per_att","scrim_td","class")
    
    #First get data for yearMin
    link <- createLink(pos,yearMin)
    
    if (!dir.exists(file.path(savePrefix,"partialData"))) {
      dir.create(file.path(savePrefix,"partialData"))
    }
    saveLoc <- str_c(savePrefix,"partialData/",yearMin,pos,"raw.rda")
    finalDat <- collectData(link,collectNFL,collectCollege,saveLoc)
    
    for (i in (as.integer(yearMin)+1):as.integer(yearMax)) {
      link <- createLink(pos,as.character(i))
      saveLoc <- str_c(savePrefix,"partialData/",as.character(i),pos,"raw.rda")
      finalDat <- finalDat %>%
        rbind(collectData(link,collectNFL,collectCollege,saveLoc))
    }
    
    source("clean.R")
    saveRDS(clean(finalDat),str_c(savePrefix,yearMin,"-",yearMax,pos,"cleanData.rda")) #clean the data
    
  }
  #return (finalDat)
}

option_list = list(
  make_option(c("-p", "--position"), type="character", default=NULL, 
              help="position is either WR or RB", metavar="character"),
  make_option(c("-s", "--startYear"), type="character", default=NULL,
              help="What year would like to start collecting data from? [2001 is minimum, 2018 is maximum]", metavar="character"),
  make_option(c("-e","--endYear"),type="character",
              help="What year would like to end collecting data from? [Should exceed startYear, and be greater than or equal to 2002; less than or equal to 2018"),
  make_option(c("-d","--dirForSave"),type="character",help="In what directory should the data be saved?")
); 

opt = parse_args(OptionParser(option_list=option_list))

doScrape(opt$p,opt$s,opt$e,opt$d)

options(warn = oldw)









