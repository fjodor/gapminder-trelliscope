# Trelliscope-Beispiel
# Inspiration: https://ryanhafen.com/blog/trelliscopejs-plotly/
# Help: help(package = "trelliscopejs")
# Es gibt eine Einführungs-Vignette

library(gapminder)
library(tidyverse)

# Gapminder: Datensatz kennen lernen

# Daten laden

data(gapminder)
str(gapminder)

# ggplot-Diagramme

# Entwicklung der Lebenserwartung im Zeitverlauf: Beispiel Deutschland

gapminder %>% 
  filter(country == "Germany") %>% 
  ggplot(aes(x = year, y = lifeExp)) +
    geom_point() +
    labs(title = "Germany", subtitle = "Life Expectancy by Year",
         caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan")

# Europa

gapminder %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  labs(title = "Europe", subtitle = "Life Expectancy by Year",
       caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
  scale_color_discrete(name = "")

# Mehr Infos: Facets

gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
    geom_line() +
    facet_wrap(~ continent) +
    labs(title = "Life Expectancy by Year", subtitle = "Facets by continent",
         caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
    scale_color_discrete(guide = NULL)

# Mouse-Over

library(plotly)

p <- gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
    geom_line() +
    facet_wrap(~ continent) +
    labs(title = "Life Expectancy by Year", subtitle = "Facets by continent",
         caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
    theme_bw() +
    theme(legend.position = "none")

ggplotly(p)
# ggplotly(p, tooltip = c("country", "x", "y"))


# Visualisierung mit Trelliscope und Plotly

library(trelliscopejs)

gapminder %>% 
  mutate(population = scales::number(pop)) %>% 
  ggplot(aes(x = year, y = lifeExp, size = pop, label = population)) +
    geom_point() +
    facet_trelliscope(~ country + continent,
                      nrow = 2, ncol = 5,
                      as_plotly = TRUE,
                      plotly_args = list(tooltip = c("x", "y", "label")),
                      thumb = FALSE) +
    theme_bw() +
    theme(legend.position = "none")

# Mit zusätzlichen Informationen

# Compute cognostics:
# delta life exp

gapminder_cog <- gapminder %>% 
  group_by(country) %>% 
  mutate(delta_lifeExp = max(lifeExp) - min(lifeExp)) %>% 
  ungroup()

# R²

getrsq <- function(data) {
  model <- lm(lifeExp ~ year, data = data)
  summary(model)$r.squared
}

gapminder_cog <- gapminder_cog %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(rsq = round(map_dbl(data, getrsq), 3)) %>% 
  unnest(cols = data) %>% 
  ungroup() %>% 
  mutate(rsq = cog(val = rsq, desc = "R-Quadrat", default_label = TRUE),
         delta_lifeExp = cog(val = delta_lifeExp, desc = "delta lifeExp", default_label = TRUE))

gapminder_cog %>% 
  mutate(population = scales::number(pop)) %>% 
  ggplot(aes(x = year, y = lifeExp, size = pop, label = population)) +
  geom_point() +
  facet_trelliscope(~ country + continent,
                    nrow = 2, ncol = 5,
                    as_plotly = TRUE,
                    plotly_args = list(tooltip = c("x", "y", "label")),
                    thumb = FALSE) +
  theme_bw() +
  theme(legend.position = "none")
