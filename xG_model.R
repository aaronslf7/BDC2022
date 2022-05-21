install.packages('tidyverse')
install.packages('tidymodels')
library(tidyverse)
library(tidymodels)

pxpdata <- rbind(NWHLdata,data2018)
pxpdata <- pxpdata %>%
  filter((Home.Team.Skaters == 5)&(Away.Team.Skaters == 5)) %>%
  mutate(distance = distance(X.Coordinate,Y.Coordinate)) %>%
  mutate(goal_value = ifelse(Event == 'Goal',1,0))

shots <- pxpdata %>%
  filter((Event == 'Shot')|(Event == 'Goal'))

xGmodel <- glm(goal_value ~ Detail.1+distance+Detail.3+Detail.4,data=shots,family="binomial")
summary(xGmodel)

xGvalue <- function(Detail.1s,distances,Detail.3s,Detail.4s){
  x <- data.frame(Detail.1 = Detail.1s,distance = distances,Detail.3=Detail.3s,Detail.4=Detail.4s)
  p <- predict(xGmodel,x)
  exp(p)
}

shots <- shots %>%
  mutate(xG = xGvalue(Detail.1,distance,Detail.3,Detail.4))

for (i in 1:nrow(pxpdata)) {
  if ((pxpdata[i,"Event"] == 'Goal')|(pxpdata[i,'Event'] == 'Shot')) {
    pxpdata[i,'xG'] = xGvalue(pxpdata[i,'Detail.1'],pxpdata[i,'distance'],pxpdata[i,'Detail.3'],pxpdata[i,'Detail.4'])
  } else {
    pxpdata[i,'xG'] = NA }
}
