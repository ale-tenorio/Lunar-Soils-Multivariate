library(tibble)
library(dplyr)
library(ggplot2)

df <- read.csv("zarzycki.csv", sep = ";", header = TRUE) %>% as_tibble() 

df1 <- df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))  %>% 
       subset(select = -c(CR2O3,FE2O3,S,LOI))

df1.pca <- prcomp(df1[,c(4:13)], center = TRUE, scale. = TRUE)

library(factoextra)

library(FactoMineR)
