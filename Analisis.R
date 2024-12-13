library(ggplot2)
library(tidyverse)
library(ggbeeswarm)
library(forcats)
library(xtable)
library(nortest)

df <- readRDS("resultados.RDS")
df$Modelo <- factor(df$Modelo)

resumen <- df %>% 
  group_by(Modelo) %>%
  summarise(Media = mean(ECM),dev = sd(ECM),
            min = min(ECM), max = max(ECM)) %>%
  arrange(Media)

xtable(resumen)

df2 <- df %>%
  filter(Modelo != "RANDOM")
df2$Modelo <- fct_reorder(df2$Modelo, df2$ECM, .fun = mean, .desc = FALSE)

ggplot(df2, aes(x = Modelo, y = ECM, fill = Modelo)) +
  geom_violin(alpha = 0.5) +
  geom_beeswarm(color = "#091057") +
  theme(legend.position = "none") + 
  theme_bw() + 
  theme(plot.title = element_text(color = "black",
                                  size = 20,hjust=0.5),
        axis.title.x = element_text(size = 15, color = "black"),  
        axis.title.y = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_blank(),  
        panel.spacing = unit(0, "lines"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "none",
        #panel.grid.major = element_blank(),  # Eliminar rejillas principales
        panel.background = element_rect(fill = "white"),  # Fondo del panel
        plot.background = element_rect(fill = "white"),   # Fondo del gráfico
        panel.grid.major = element_line(color = "#DBD3D3"), # Líneas de cuadrícula mayores
        panel.grid.minor = element_line(color = "#DBD3D3")
  ) + 
  labs(title = "ECM por Método", x = "Método")



an <- aov(ECM ~ Modelo, data = df2)
xtable(anova(an))

tukey <- TukeyHSD(an)
plot(tukey)
dif <- as.data.frame(tukey$Modelo)
dif <- dif %>%
  arrange(`p adj`)
xtable(dif)

ad.test(an$residuals)
bartlett.test(ECM ~ Modelo, data = df2)


