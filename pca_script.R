library(raster)
library(ggplot2)

#### IMPORTANDO IMAGEM E COLOCANDO NO FORMATO DESEJADO ####


red <- raster('LC08_L1TP_117033_20210913_20210924_02_T1_B4.TIF')
green <- raster('LC08_L1TP_117033_20210913_20210924_02_T1_B3.TIF')
blue <- raster('LC08_L1TP_117033_20210913_20210924_02_T1_B2.TIF')

rgb <- stack(red,green,blue)

plotRGB(rgb, scale = 65535)


# Cortando a imagem

de <- drawExtent() # Selecionar com o mouse

rgb <- crop(rgb,de)

plotRGB(rgb, scale = 65535)

red <- rgb$LC08_L1TP_117033_20210913_20210924_02_T1_B4
green <- rgb$LC08_L1TP_117033_20210913_20210924_02_T1_B3
blue <- rgb$LC08_L1TP_117033_20210913_20210924_02_T1_B2

#### Analisando o gráfico de dispersão de cada par de cores. #####


# O conjunto de dados é muito grande, então vamos
# selecionar um subconjunto aleatório de 10000 pixels

sr <- sampleRandom(rgb,10000)

sr_df <- as.data.frame(sr)

names(sr_df) <- c('red','green','blue')

# Red-Green

ggplot(sr_df,aes(red,green))+
  geom_point()+
  labs(title = 'Red x Green scatterplot',
       x = 'Red band',
       y = 'Green band')


cor(sr_df$red,sr_df$green) # correlação de 0.95

# Red-Blue

ggplot(sr_df,aes(red,blue))+
  geom_point()+
  labs(title = 'Red x Blue scatterplot',
       x = 'Red band',
       y = 'Blue band')


cor(sr_df$red,sr_df$blue) # correlação de 0.9

# Green-Blue

ggplot(sr_df,aes(green,blue))+
  geom_point()+
  labs(title = 'Green x Blue scatterplot',
       x = 'Green band',
       y = 'Blue band')

cor(sr_df$green,sr_df$blue) # correlação de 0.91


##### Principal Component Analysis #####

# Redimention data


### PCA

red_m2 <- matrix(red, nrow = nrow(red)*ncol(red),ncol = 1, byrow = FALSE)
green_m2 <- matrix(green, nrow = nrow(red)*ncol(red),ncol = 1, byrow = FALSE)
blue_m2 <- matrix(blue, nrow = nrow(red)*ncol(red),ncol = 1, byrow = FALSE)

matriz <- cbind(red_m2,green_m2,blue_m2)

prcomp(matriz, center = FALSE)

red_m <- matrix(red, nrow = nrow(red), ncol = ncol(red), byrow = TRUE)
green_m <- matrix(green, nrow = nrow(green), ncol = ncol(green), byrow = TRUE)
blue_m <- matrix(blue, nrow = nrow(blue), ncol = ncol(blue), byrow = TRUE)

red_prcomp <- prcomp(red_m, center = FALSE)
green_prcomp <- prcomp(green_m, center = FALSE)
blue_prcomp <- prcomp(blue_m, center = FALSE)

# Reconstruindo imagem

nComp_img <- 40

red_hat <- red_prcomp$x[,1:nComp_img] %*% t(red_prcomp$rotation[,1:nComp_img])

green_hat <- green_prcomp$x[,1:nComp_img] %*% t(green_prcomp$rotation[,1:nComp_img])

blue_hat <- blue_prcomp$x[,1:nComp_img] %*% t(blue_prcomp$rotation[,1:nComp_img])

image(red_hat) # Reconstrução banda vermelha

image(green_hat) # Reconstrução banda verde

image(blue_hat) # Reconstrução banda azul


r_raster <- raster(red_hat)
g_raster <- raster(green_hat)
b_raster <- raster(blue_hat)

rgb_hat <- stack(r_raster,g_raster,b_raster)

plotRGB(rgb_hat,scale = 65535) # imagem reconstruída com 40 componentes principais








