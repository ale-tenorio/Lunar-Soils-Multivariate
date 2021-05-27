library(tibble)
library(dplyr)
library(ggplot2)
library(ggrepel)

df <- read.csv("zarzycki.csv", sep = ";", header = TRUE) %>% as_tibble() %>% 
      mutate(Tipo = ifelse(is.na(Mission),"Simulant","Moon"))

df1 <- df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))  %>% 
       subset(select = -c(CR2O3,FE2O3,S,LOI))

df1.pca <- prcomp(df1[,c(4:13)], center = TRUE, scale. = TRUE)

library(factoextra)

library(FactoMineR)

df1.pca2 <- PCA(X = df1[,c(4:13)], scale.unit = TRUE, graph = TRUE )

perc.PCA <- df1.pca2$eig[,2]

plot.df <- df %>% bind_cols(df1.pca2$svd$U[,1:2] %>% as_tibble())

ggplot() + 
  geom_point(plot.df, mapping = aes(x = V1,y = V2), size = 3.5, colour = "black") + 
  theme_bw() + 
  ggrepel::geom_text_repel(plot.df, 
                         mapping = aes(V1,V2, label = ID),
                         size = 4,
                         box.padding = 0.5,
                         segment.color = "black") +
  labs(x = paste("Dim 1 (",round(perc.PCA[1], 2),"%)", sep = ""),
       y = paste("Dim 2 (",round(perc.PCA[2], 2),"%)", sep = "")) + 
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) + 
  theme(axis.title.x = element_text(face = "bold")) +
  theme(axis.title.y = element_text(face = "bold")) + 
  scale_x_continuous(position = "right")






https://rpubs.com/Cristina_Gil/PCA

