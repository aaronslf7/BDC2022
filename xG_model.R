install.packages('tidyverse')
install.packages('tidymodels')
library(tidyverse)
library(tidymodels)

# Function for determining distance from shooter to centre of the goal
function(X.Coordinate,Y.Coordinate) {
  x_dis = 189-X.Coordinate
  y_dis = 42.5-Y.Coordinate
  return(distance = sqrt((x_dis)^2+(y_dis)^2))
}

# Merging two data sets provided by Stathletes
pxpdata <- rbind(NWHLdata,data2018)

# Filtering for conditions I want to include to determine xG values
# Also made two new variables: distance to goal and goal_value
pxpdata <- pxpdata %>%
  filter((Home.Team.Skaters == 5)&(Away.Team.Skaters == 5)) %>%
  mutate(distance = distance(X.Coordinate,Y.Coordinate)) %>%
  mutate(goal_value = ifelse(Event == 'Goal',1,0))

# Filtering so new data set "shots" only contains shots
shots <- pxpdata %>%
  filter((Event == 'Shot')|(Event == 'Goal'))

# glm regression with 4 variables
xGmodel <- glm(goal_value ~ Detail.1+distance+Detail.3+Detail.4,data=shots,family="binomial")
summary(xGmodel)

# A function that will have the xG value for a given shot as the output
xGvalue <- function(Detail.1s,distances,Detail.3s,Detail.4s){
  x <- data.frame(Detail.1 = Detail.1s,distance = distances,Detail.3=Detail.3s,Detail.4=Detail.4s)
  p <- predict(xGmodel,x)
  exp(p)
}

# These apply the xG function to the whole "pxpdata" sets. This way the play by play data will also contain xG values for shots
shots <- shots %>%
  mutate(xG = xGvalue(Detail.1,distance,Detail.3,Detail.4))

for (i in 1:nrow(pxpdata)) {
  if ((pxpdata[i,"Event"] == 'Goal')|(pxpdata[i,'Event'] == 'Shot')) {
    pxpdata[i,'xG'] = xGvalue(pxpdata[i,'Detail.1'],pxpdata[i,'distance'],pxpdata[i,'Detail.3'],pxpdata[i,'Detail.4'])
  } else {
    pxpdata[i,'xG'] = NA }
}
