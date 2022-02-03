# Install / Load relevant packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(shiny,tidyverse, tidytext, stringr, Matrix,DT,data.table, dplyr)
#pacman::p_load(DT)
