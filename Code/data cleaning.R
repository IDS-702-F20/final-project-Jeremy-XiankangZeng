library(dplyr)

main <- read.csv("main.csv")

RoleGoldDiff <- function(role){{
  result<- numeric(length = 7620)
  red <- vector("list",7620)
  blue<- vector("list",7620)
  if(role == "top"){
    for(i in 1:length(main$goldredTop)){
      y <- gsub("\\[|\\]", "", main$goldredTop[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    for(i in 1:length(main$goldblueTop)){
      y <- gsub("\\[|\\]", "", main$goldblueTop[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
  }
  else if(role == "jun"){
    for(i in 1:length(main$goldredJungle)){
      y <- gsub("\\[|\\]", "", main$goldredJungle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    for(i in 1:length(main$goldblueJungle)){
      y <- gsub("\\[|\\]", "", main$goldblueJungle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
  }
  else if(role == "mid"){
    for(i in 1:length(main$goldredMiddle)){
      y <- gsub("\\[|\\]", "", main$goldredMiddle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    for(i in 1:length(main$goldblueMiddle)){
      y <- gsub("\\[|\\]", "", main$goldblueMiddle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
  }
  else if(role == "adc"){
    for(i in 1:length(main$goldredADC)){
      y <- gsub("\\[|\\]", "", main$goldredADC[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    for(i in 1:length(main$goldblueADC)){
      y <- gsub("\\[|\\]", "", main$goldblueADC[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
  }
  else{
    for(i in 1:length(main$goldredSupport)){
      y <- gsub("\\[|\\]", "", main$goldredSupport[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
  }
  for(i in 1:length(main$goldblueSupport)){
    y <- gsub("\\[|\\]", "", main$goldblueSupport[i])
    y <- gsub(" ","",y)
    numbers <- strsplit(y,',')
    numbers <- unlist(numbers)
    blue[[i]] <- as.numeric(numbers)
  }
}

for(i in 1:length(result)){
  result[i] <- round(mean(blue[[i]] - red[[i]]),0)
}  
result
}

main$TopAvg <- RoleGoldDiff(role = "top")
main$JungleAvg <- RoleGoldDiff(role = "jun")
main$MidAvg <- RoleGoldDiff(role = "mid")
main$ADCAvg <- RoleGoldDiff(role = "adc")
main$SupportAvg <- RoleGoldDiff(role= "support")

games <- data.frame(winner=ifelse(main$bResult==1,"Blue","Red"),
                    TopAvg = main$TopAvg,
                    JungleAvg= main$JungleAvg,
                    MidAvg = main$MidAvg,
                    ADCAvg = main$ADCAvg,
                    SupportAvg= main$SupportAvg,
                    GameLength= main$gamelength,
                    KillDiff = main$KillDiff,
                    Season = main$Year)

games$TopWinningTeam <- ifelse(games$TopAvg > 0,"Blue","Red")
games$JungWinningTeam <- ifelse(games$JungleAvg > 0,"Blue","Red")
games$MidWinningTeam <- ifelse(games$MidAvg > 0,"Blue","Red")
games$ADCWinningTeam <- ifelse(games$ADCAvg > 0,"Blue","Red")
games$SupWinningTeam <- ifelse(games$SupportAvg > 0,"Blue","Red")

# games$TopAvg <- abs(games$TopAvg)
# games$JungleAvg <- abs(games$JungleAvg)
# games$MidAvg <- abs(games$MidAvg)
# games$ADCAvg <- abs(games$ADCAvg)
# games$SupportAvg <- abs(games$SupportAvg)


Carries <- vector("character",length = nrow(games))
CarryTeams <- vector("character",length = nrow(games))

for(i in 1:nrow(games))
{
  TopValue <- abs(games$TopAvg[i])
  JungValue <- abs(games$JungleAvg[i])
  MidValue <- abs(games$MidAvg[i])
  ADCValue <- abs(games$ADCAvg[i])
  SupValue <- abs(games$SupportAvg[i])
  
  RoleValues <- c(TopValue,JungValue,MidValue,ADCValue,SupValue)
  
  CarryGold <- max(RoleValues)
  
  #Cases in which each role was the carry. Get the team which was the winner aswell.
  if(CarryGold == TopValue )
  {
    Carries[i] <- "Top"
    CarryTeams[i] <- games$TopWinningTeam[i]
  }
  else if(CarryGold == JungValue){
    Carries[i] <- "Jungle"
    CarryTeams[i] <- games$JungWinningTeam[i]
  }
  
  else if(CarryGold == MidValue){
    Carries[i] <- "Mid"
    CarryTeams[i] <- games$MidWinningTeam[i]
  }
  else if(CarryGold == ADCValue){
    Carries[i] <- "ADC"
    CarryTeams[i] <- games$ADCWinningTeam[i]
  }
  else{
    #Case for support (yeah, like that's gonna happen :^) )
    Carries[i] <- "Support"
    CarryTeams[i] <- games$SupWinningTeam[i]
  }
}
games$Carry <- Carries
games$TeamOfCarry <- CarryTeams

games <- data.frame(games)
write.csv(games,file = "final data")




