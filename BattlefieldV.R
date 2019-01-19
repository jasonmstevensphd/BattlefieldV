library(tidyverse)
library(readxl)

BFV <- read_xlsx("D:/Data_Science/BattlefieldV/Battlefield5.xlsx", sheet = "Sheet1") %>%
  filter(!is.na(Date))

KD_Plot <- ggplot(BFV, aes(x = `K/D`))+
  geom_density(aes(color = GameMode))

KD_Plot


KD_Plot_Map <- ggplot(BFV, aes(x = `K/D`))+
  geom_density(aes(color = Map))

KD_Plot_Map

KD_Plot_Weapon <- ggplot(BFV, aes(x = `K/D`))+
  geom_density(aes(color = Weapon))+
  facet_wrap(~Class)

KD_Plot_Weapon

ROWS <- nrow(BFV)

BFV$Entry <- seq(1:ROWS)

KD_Time <- ggplot(BFV, aes(x = Entry, y = `K/D`))+
  geom_point(aes(color = GameMode, shape = Weapon), size = 2)+ 
  geom_smooth(method='lm',formula=y~x)

KD_Time
