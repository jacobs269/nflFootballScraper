# Scraping NFL Player Data Using R

In this project, I use R to scrape data on the professional and college careers of NFL running backs and wide recievers. The professional data comes from [here](https://www.pro-football-reference.com/) and the college data comes from [here](https://www.sports-reference.com/cfb/). In addition to grabbing statistics from a player's college and nfl career, the scraper also grabs NFL combine statistics, NFL rookie statistics, and some NFL fantasy information. To learn more about what data is included in the data files that can be scraped using the code, see DataVariables.md

What follows below are instructions for using this scraper

### Prerequisites

1. [R programming language](https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/)
2. The following R libraries:
	* optparse
	* rvest
	* stringr
	* tidyverse
	* XML
	* xml2


### Running the Script

1. First download the repository.
2. From the command line, navigate to the directory where you downloaded the repository

3. If I want to collect data from NFL running backs between 2000 and 2002, and store this in the directory /foo/bar/:

```
Rscript --vanilla scrape.R -p "RB" -s "2000" -e "2002" -d "/foo/bar/"
```

* -p: The position for which you want to collect data. Should be a string. CAN BE "RB" or "WR"
* -s: The first year from which you would like to collect data. Should be a string. Earliest year is "2000". Latest year is "2018"
* -e: The last year from which you would like to collect data. Should be a string. Earliest year is "2000". Latest year is "2018"
* -d: the directory in which to save the data file scraped. Should be a string. "/foo/bar" will break. "/foo/bar/" will not.

4. The data will be stored in 2000-2002RBcleanData.rda. 
5. Load in R using the readRDS function, and happy analyzing!

### Ideas for the improvement of this script

There are a couple of things I haven't had time to do, that should be done sooner or later

1. You may want to collect data from before 2000. I haven't had time to put this functionality in yet. The only reason data isn't collected from before 2000 is because the sports reference sites don't have combine data for players from before that year. If you want to do this, then start by putting a condition inside the collectData function, which prevents the running of the getDraftData function when year is less than 2000. There are some other details you will have to work out, but this is a good start.

2. If you want to collect data from another position (NOT wide reciever or running back), then see the doScrape function. Inside, there is the lines 

```
collectNFL <- c("g","gs","rush_att","rush_yds","rush_td","rush_long","rush_yds_per_att",
                    "rush_yds_per_g","rush_att_per_g","targets","rec_td","rec_per_g",
                    "rec_yds_per_g","yds_from_scrimmage","rush_receive_td","fumbles")
    collectCollege <- c("g","rush_att","rush_yds","rush_td","rush_yds_per_att",
                        "rec","rec_yds","rec_yds_per_rec","rec_td","scrim_att","scrim_yds",
                        "scrim_yds_per_att","scrim_td","class")
```
These are the variable names collected from the player tables inside the sports reference sites. The variable names are different for quarterbacks than they are for wide recievers and running backs. To improve the script, make an condition inside this function based on position, and for each position make 2 variables like the ones above, with appropriate variable names

