#####Import Data
#prepare
library(tidyverse)
#This work directory is where the delimited bridge data files is downloaded.
setwd('/Users/Sushi/Desktop/WISC/Course/479/BridgeData')
#read all the file names of each year (such as '1992del') into a list
fileName<-dir(getwd())
#build an empty dataframe
bridge <- data.frame()
#start a loop through all the files (years)
for(file in fileName){
  file <- fileName[1]
  path <- paste(getwd(),"/",file,sep="")
  #read all the txt file names of each state into a list
  TxtFileName <- dir(path)
  #start a loop through all the files (states)
  for(TxtFile in TxtFileName){
    TxtFile <- TxtFileName[1]
    if((length(grep('xls\\>',TxtFile))==1)|(length(grep('xlsx\\>',TxtFile))==1)){
      next
    }
    FilePath <- paste(path,"/",TxtFile,sep="")
    tmpdata <- read_csv(FilePath)
    #skip empty files
    if(length(tmpdata)==0){
      next
    }
    #select the variables of interest
    tmpdata <- select(tmpdata,
                      STATE_CODE_001,
                      STRUCTURE_NUMBER_008,
                      PLACE_CODE_004,
                      YEAR_BUILT_027,
                      YEAR_RECONSTRUCTED_106,
                      DESIGN_LOAD_031,
                      ADT_029,
                      YEAR_ADT_030,
                      DECK_COND_058,
                      SUPERSTRUCTURE_COND_059,
                      SUBSTRUCTURE_COND_060,
                      CHANNEL_COND_061,
                      CULVERT_COND_062)
    tmpdata$year <- parse_number(file)
    #update the bridge dataframe
    bridge <- rbind(bridge,tmpdata)
  }
}
#rearrange the variable order
bridge <- select(bridge,STATE_CODE_001,STRUCTURE_NUMBER_008,year,everything())

#save the dataframe to a RDS.
write_rds(bridge, "bridge.rds")

#summerize the data
year_summary <- bridge %>%
  group_by(year) %>%
  summarise(count = n(),
            BUILT = mean(as.numeric(YEAR_BUILT_027), na.rm = TRUE),
            DECK_COND = mean(as.numeric(DECK_COND_058),na.rm = T),
            SUPERSTRUCTURE_COND = mean(as.numeric(SUPERSTRUCTURE_COND_059),na.rm = T),
            SUBSTRUCTURE_COND = mean(as.numeric(SUBSTRUCTURE_COND_060),na.rm = T),
            CHANNEL_COND = mean(as.numeric(CHANNEL_COND_061),na.rm = T),
            CULVERT_COND = mean(as.numeric(CULVERT_COND_062),na.rm = T)
  )
YearBuilt2014 <- bridge %>%
  filter(year==2014) %>%
  group_by(YEAR_BUILT_027) %>%
  summarise(count = n(),
            DECK_COND = mean(as.numeric(DECK_COND_058),na.rm = T),
            SUPERSTRUCTURE_COND = mean(as.numeric(SUPERSTRUCTURE_COND_059),na.rm = T),
            SUBSTRUCTURE_COND = mean(as.numeric(SUBSTRUCTURE_COND_060),na.rm = T),
            CHANNEL_COND = mean(as.numeric(CHANNEL_COND_061),na.rm = T),
            CULVERT_COND = mean(as.numeric(CULVERT_COND_062),na.rm = T)
  )


