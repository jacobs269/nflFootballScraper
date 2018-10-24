# What is included in the Data File?

Each row is a player

* Year: Year the player enetered the league
* Player: Name of player (first and last)
* Pos: Position of the player
* School: College of the player
* Wt: Weight of the player (lb)
* Height: Height of player in inches (inch)
* X40Yd: Combine 40 yd dash time (sec)
* Vertical: Combine vertical leap height (inch)
* BenchReps: Combine number of bench presses
* Broad.Jump: Combine broad.jump distance (inch)
* X3Cone: Combine 3 cone time
* Shuttle: Combine shuttle time
* NFLCAR.?: A collection of variables with specific NFL statistical information (the name of the variable in R clearly indicates what the statistic represents, except in the following case):
	* NFLCAR.av: [approximate value](https://www.pro-football-reference.com/blog/index37a8.html)
* ROOKIE.?: All of the same variables as in NFLCAR, except this time, the information JUST for the rookie season
* CLG.?: Most of the same variables as in NFLCAR and ROOKIE, except this time, for the players college career
* hasNFLData: Some players do not have a page on the NFL sports reference site. For these players, this variable is false. Otherwise true.
* round: The round the player was drafted in. UDFA if undrafted
* numYears: Ignore; irrelevant
* Power5: Is the player in a power 5 conference?
* underClass: Did the player come out of college before their senior year to enter the draft?
* ROOKIE.fant: Derived variables. Take careerYdsFromScrimmage/10 + careerTouchdownsX6− careerFumblesX2 and divide by number of games played in rookie season
* NFLCAR.fant: Same as ROOKIE.fant, but dividing by total number of games played in career


