library(dplyr)
library(ggplot2)
library(RColorBrewer)

#Dataset
library(gapminder)
data("gapminder")

gapminder %>% head()


#group by country

lifeExpByCountry <- gapminder %>% group_by(country) %>% summarise(
  meanLifeExp = mean(lifeExp),
  meanGpdPercap = mean(gdpPercap)
)

lifeExpByCountry <- lifeExpByCountry %>% arrange(desc(meanLifeExp))

library(tmap)
data(World, metro, rivers)

World <- left_join(World, lifeExpByCountry, by =c('name' = 'country'))

ggplot(data = World) + 
  geom_sf(aes(fill = meanLifeExp), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90",
                       name = "Life Expentancy") +
  theme_minimal() + 
  labs(
    title = "Average life expentancy (in years)",
    subtitle = "Dataset: gapminder",
	x = "", y = ""
  )


# lets add letters


ggplot(data = World) + 
  geom_sf(aes(fill = meanLifeExp), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90",
                       name = "Life Expentancy") +
  geom_sf_text(aes(label = iso_a3, fontface = "plain"),
                   size = 1.5,
                   color = "black") +
  theme_minimal() + 
  labs(
    title = "Average life expentancy (in years)",
    subtitle = "Dataset: gapminder",
	x = "", y = ""
  )

## Mapping categorical variable income_grp

ggplot(World) +
  geom_sf(aes(fill = income_grp)) + 
  scale_fill_brewer(palette = "YlOrRd") +
  scale_color_identity() +  # Mantener los colores especificados
  labs(title = "", x = "", y = "",
       fill = "Gross National Income (GNI) per capita") +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.key = element_blank()
  )
  
  

ggplot(World) +
  geom_sf(aes(fill = income_grp)) + 
  scale_fill_brewer(palette = "Oranges") +
  scale_color_identity() + 
  labs(title = "", x = "", y = "",
       fill = "Gross National Income (GNI) per capita") + 
  geom_sf_text(aes(label = iso_a3, 
                   fontface = "plain"),
               size = 1.5, color = "black") +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.key = element_blank()
  )

#export to png high resolution

png("Gross National Income (GNI) per capita.png", 
    width = 18, height = 11, units  =  'in', res = 300)

ggplot(World) +
  geom_sf(aes(fill = income_grp)) + 
  scale_fill_brewer(palette = "Oranges") +
  scale_color_identity() + 
  labs(title = "", x = "", y = "",
       fill = "Gross National Income (GNI) per capita") + 
  geom_sf_text(aes(label = iso_a3, 
                   fontface = "plain"),
               size = 1.5, color = "black") +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.key = element_blank()
  )

dev.off()
