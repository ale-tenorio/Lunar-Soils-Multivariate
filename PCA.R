library(tibble)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggforce)

df <- read.csv("zarzycki.csv", sep = ";", header = TRUE) %>% as_tibble() %>% 
      mutate(Tipo = ifelse(Mission == "","Simulant","Moon"))

df1 <- df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))  %>% 
       subset(select = -c(CR2O3,FE2O3,S,LOI))

df1.pca <- prcomp(df1[,c(4:13)], center = TRUE, scale. = TRUE)

library(factoextra)

library(FactoMineR)

df1.pca2 <- PCA(X = df1[,c(4:13)], scale.unit = TRUE, graph = TRUE )

perc.PCA <- df1.pca2$eig[,2]

plot.df <- df %>% bind_cols(df1.pca2$svd$U[,1:2] %>% as_tibble())

ggplot() + 
  ggrepel::geom_text_repel(plot.df, 
                           mapping = aes(V1,V2, label = ID),
                           size = 4.5,
                           box.padding = 0.5,
                           segment.color = "black") +
  geom_point(plot.df, mapping = aes(x = V1,y = V2), size = 5,
             pch = 21, colour = "white", fill = "black") + 
  geom_point(plot.df %>% filter(Tipo == "Moon"), mapping = aes(x = V1,y = V2), size = 5, 
             pch = 21, colour = "black", fill = "white") +
  theme_bw() + 
  labs(x = paste("Dim 1 (",round(perc.PCA[1], 2),"%)", sep = ""),
       y = paste("Dim 2 (",round(perc.PCA[2], 2),"%)", sep = "")) + 
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) + 
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 14)) +
  scale_x_continuous(position = "right")


plot.df2 <- df1.pca2$var$coord %>% as.data.frame() %>% add_rownames()

ggplot() + 
  ggrepel::geom_text_repel(plot.df2, 
                           mapping = aes(Dim.1,Dim.2, label = rowname),
                           size = 4.5,
                           box.padding = 0,
                           segment.color = "black") + 
  geom_segment(plot.df2, mapping = aes(xend = Dim.1, yend = Dim.2), x = 0, y = 0, col = "gray") +
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  ggforce::geom_circle(mapping = aes (x0 = 0, y0 = 0, r = 1)) + 
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 14)) +
  labs(x = paste("Dim 1 (",round(perc.PCA[1], 2),"%)", sep = ""),
       y = paste("Dim 2 (",round(perc.PCA[2], 2),"%)", sep = "")) + 
  coord_equal()

https://rpubs.com/Cristina_Gil/PCA

