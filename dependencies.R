# Install / Load relevant packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(shiny,tidyverse, tidytext, stringr, Matrix,DT,data.table)
#pacman::p_load(DT)
