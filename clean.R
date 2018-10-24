###CLEANING FILE

##helper functions for cleaner

#Get the round,pick for ONE player
#player: the value for a specific player of the round/pick column in raw data
getRoundPick <- function(player) {
  if (length(player)<4) {
    return(c("UDFA","UDFA"))
  }
  else {
    round <- player[2]
    round <- str_replace(str_trim(round),"[^\\d].*","")
    ovrPick <- player[3]
    ovrPick <- str_replace(str_trim(ovrPick),"[^\\d].*","")
    return (as.matrix(t(c(as.numeric(round),as.numeric(ovrPick)))))
  }
}

#Get the round/pick for all players
allRoundPick <- function(finalDat) {
  rawRoundPick <- str_split(finalDat$Drafted..tm.rnd.yr.,"/")
  roundPick <- matrix(data=NA,nrow=0,ncol=2)
  for (i in 1:length(rawRoundPick)) {
    newDat <- getRoundPick(rawRoundPick[[i]])
    roundPick <- rbind(roundPick,newDat)
  }
  return(roundPick)
}

##For a column in a dataframe, make all blanks NAs
empty_to_na <- function(col) {
  col[col==""] = NA
  return (col)
}



############
clean <- function(data) {
  ##misc type changing and also getting height in inches
  data <- data %>%
    mutate_at(vars(Wt:Shuttle),as.numeric) %>%
    select(-c(AV,Rk,plrLinks,clgLinks)) %>%
    separate(Height,into=c("feet","inches"),convert=TRUE) %>%
    mutate(height = feet*12+inches) %>%
    select(-c(feet,inches)) %>%
    mutate(hasNFLdata = !(as.numeric(is.na(ROOKIE.Data) | ROOKIE.Data=="0"))) %>%
    select(-c(ROOKIE.Data,NFLCAR.Data,hasNFLLink))
  
  #What NFLCAR.noData and ROOKIE.noData really mean: 1 if the nfl link had DATA. 0 if the nfl link had no DATA. NA if there was no link. So if either NA or 0, no nfl data available. 
  #so hasNFLdata: 1=>There is NFL game data available (ANY DATA) 0=>NO NFL game data available.
  
  #get round pick info and add to data
  roundPick <- allRoundPick(data) %>%
    as.tibble()
  names(roundPick) = c("round","ovrPck")
  data <- cbind(data,roundPick) %>%
    as.tibble() %>%
    select(-c(College,Drafted..tm.rnd.yr.))
  
  #Cleaning in the CollegeData part
  data <- data %>%
    select(-CLG.college) %>%
    mutate_at(vars(NFLCAR.g:CLG.scrim_td),as.character) %>%
    mutate_at(vars(NFLCAR.g:CLG.scrim_td),as.numeric) %>%
    mutate_at(vars(Year,School,round),as.factor) %>%
    mutate_all(empty_to_na)
  
  #Make a column that checks if college,combine,av info is all available
  hasDraftCollegeAV <- data %>%
    select(NFLCAR.av,ROOKIE.av,Wt:Shuttle,height,starts_with("CLG")) %>%
    is.na() %>%
    rowSums() %>%
    map_lgl(~.==0)
  
  #Make a column that checks if the row has no NAs
  fullRow <- data %>%
    is.na() %>%
    rowSums() %>%
    map_lgl(~.==0)
  
  data <- data %>%
    mutate(hasDraftCollegeAV = hasDraftCollegeAV, hasAllData = fullRow)
  
  ##Adding the columns that were added by Daryl for EDA
  data <- data %>% mutate(
    Seasons = as.numeric(as.character(Year)),
    numYears = 2018-Seasons,
    ROOKIE.fant = ROOKIE.yds_from_scrimmage/10 + ROOKIE.rush_receive_td*6 - ROOKIE.fumbles*2,
    NFLCAR.fant = (NFLCAR.yds_from_scrimmage/10 + 
                     NFLCAR.rush_receive_td*6 - NFLCAR.fumbles*2)/NFLCAR.g,
    avgAV = NFLCAR.av/NFLCAR.g,
    Power5 = ifelse(CLG.conf == "SEC" | CLG.conf == "Big Ten" | CLG.conf == "Big 12" |
                      CLG.conf == "Pac-12" | CLG.conf == "ACC", TRUE, FALSE),
    underClass = ifelse(CLG.class == "JR" | CLG.class == "SO","UC","SR"),
    lgROOKIE.fant = log(ROOKIE.fant+abs(min(ROOKIE.fant,na.rm=TRUE))+.01),
    lgNFL.fant = log(NFLCAR.fant+abs(min(NFLCAR.fant,na.rm=TRUE))+.01),
    sqrtNFL.fant = sqrt(NFLCAR.fant+abs(min(NFLCAR.fant,na.rm=TRUE))))
      
  
  return (data)
}

##For running
#rawLoc <- "~/Desktop/GradSchool/STAT6950/Project/Data/2017-2018RBrawData.rda"
#finalDat <- clean(readRDS(rawLoc))
#saveRDS(finalDat,"~/Desktop/GradSchool/STAT6950/Project/Data/cleanData2018.rda")
