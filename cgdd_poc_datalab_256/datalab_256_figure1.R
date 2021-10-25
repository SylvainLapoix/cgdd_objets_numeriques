## setup -------

library(tidyverse)
library(readxl)
library(here)
library(lemon)
library(ggtext)
library(ggrepel)


biodiv <- readxl::read_excel(here("cgdd_poc_datalab_256", "data", "biodiv_evolution_depenses.xlsx"),
                             sheet = 1) %>% 
  filter(Nature == "Total") %>% 
  pivot_longer(-Nature, names_to = "annee", values_to = "depenses",
               names_transform = list(annee = as.integer)
  ) %>% 
  mutate(depenses_text = gsub("\\.", ",", as.character(round(depenses, digits = 2))),
         annee = as.integer(annee))

# palette datalab 256

datalab_pal <- c(
  'vert' =  "#a1bd0b",
  'orange' = "#ed6f0f",
  'bleu' = "#6c8da9"
  )

# event

annee_event <- c(2006, 2009, 2016, 2018)

filter(biodiv, annee %in% annee_event)$depenses

jalons <- data.frame(annee_event,
                     y = filter(biodiv, annee %in% annee_event)$depenses,
                     texte = c("**14 avril 2006**<br>Loi relative aux parcs nationaux, aux parcs naturels marins<br>et aux parcs naturels régionaux",
                               "**3 août 2009 et 12 juillet 2010**<br>Lois Grenelle 1 et 2",
                               "**8 août 2016**<br>Loi pour la reconquête de la biodiversité,<br>de la nature et des paysages",
                               "**4 juillet 2018**<br>Plan biodiversité"))

## base chart ----

figure1 <-
  biodiv %>% ggplot(aes(x = annee, y = depenses)) +
  geom_point(color = datalab_pal["vert"]) +
  geom_line(aes(group = Nature), color = datalab_pal["vert"]) +
  geom_text(aes(x = annee, y = depenses + .08, label = depenses_text),
            color = datalab_pal["vert"], size = 3) +
  geom_segment( aes(xend = annee, y= 1, yend = depenses),
                color= datalab_pal["vert"], size = .2, linetype = 2) +
  theme_minimal() +
  ylim(c(1,3)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(size = .2),
    axis.ticks.x = element_line(size = .2),
    plot.title = element_markdown(size = 12),
    plot.subtitle = element_markdown(size = 8),
    plot.caption = element_markdown(hjust = 0),
    axis.text.x = element_text(size = 8)
  ) +
  geom_segment(aes(x = 2004, xend = 2010, y = 1, yend = 1),
               size = 7, color = datalab_pal["orange"]) +
  annotate(geom = "text", x = (2004+2010)/2, y = 1,
           label = "Stratégie nationale de la biodiversité 1",
           hjust = .5,
           size = 2.5,
           color = "white") +
  geom_segment(aes(x = 2011, xend = 2020, y = 1, yend = 1),
               size = 7, color = datalab_pal["orange"]) +
  annotate(geom = "text", x = (2011+2020)/2, y = 1,
           label = "Stratégie nationale de la biodiversité 2",
           hjust = .5,
           size = 2.5,
           color = "white") +
  coord_capped_cart(bottom='right') + # https://cran.r-project.org/web/packages/lemon/
  labs(title = "<b>Figure 1 : évolution de la dépense de protection de la biodiversité et des paysages en France</br>",
       subtitle = "En milliards d'euros courants",
       caption = "<em>Champ : France Entière.<br><b>Source</b> : SDES, Compte satellite de l'environnement 2020</em>") +
  scale_x_continuous(breaks = c(2000:2020)) +
  # geom_text_repel(
  #   data = jalons,
  #   aes(x = annee_event, y = y, label = texte),
  #   size = 2,
  #   min.segment.length = 0,
  #   nudge_x = -3,
  #   nudge_y = .3
  # )
geom_richtext(
  data = jalons,
  aes(x = annee_event, y = y, label = texte),
  size = 2,
  nudge_x = -3,
  nudge_y = .3
  
)
