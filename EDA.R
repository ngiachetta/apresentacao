library(tidyverse)
library(plotly)
library(tidyr)
library(readr)
library(magrittr)
library(ggthemes)
library(stringr)
library(zoo)

dataset1 <- read_csv("dataset1.csv")

# Transform 

dataset1 %<>% filter(Category %in% c("Beef & Pork",
                                    "Beverages",
                                    "Chicken & Fish",
                                    "Desserts",
                                    "Smoothies & Shakes",
                                    "Snacks & Sides")) %>% 
  mutate(Category_2 = case_when(Category == "Beef & Pork"~"Meats",
                                Category == "Chicken & Fish"~"Meats",
                                Category == "Smoothies & Shakes"~"Desserts",
                                TRUE ~ Category))
# Plot 1: Média Calórica por Produtos
p1 <- dataset1 %>% 
  group_by(Category_2) %>% 
  summarise(Mean_Calories = mean(Calories, na.rm = T)) %>%
  arrange(Mean_Calories) %>% 
  mutate(Category_2 = factor(Category_2, c("Beverages", "Snacks & Sides", "Desserts", "Meats"))) %>% 
  ggplot(aes(Category_2, Mean_Calories))+geom_col(fill =c("#ffd700", "#db1020", "#27742d", "purple"), width = 0.75)+
  coord_flip()+
  labs(title = "Média calórica por categoria de produtos", x = "Categoria",y= "Média de Calorias")+
  scale_y_continuous(limits = c(0,550), breaks = c(0,200,300,400, 500))+
  theme_bw()
p1

# Receita das franquias em São Paulo

receita <- tibble(mes = seq(1,12,1) %>% rep(3), ano = NA, receita_milhoes = runif(36, 15,35))
receita$ano[1:12] <- 2014
receita$ano[13:24] <- 2015
receita$ano[25:36] <- 2016
receita %<>% mutate(mes_ano = str_c(as.character(mes), as.character(ano),sep = "/"),
                    mes_ano = as.yearmon(mes_ano, "%m/%Y") %>% as.Date)

p2 <- ggplot(receita, aes(mes, y = receita_milhoes)) + geom_line(aes(color = factor(ano)))+
  facet_grid(ano~.)+
  scale_x_continuous(breaks = c(1:12))+theme_bw()+
  scale_color_manual(values = c("#ffd700", "#db1020", "#27742d"))+
  theme(legend.position = 'none')+
  labs(title = "Total de receita em São Paulo/Ano", x = "Mês", y = "Total de receita (em milhões)")


p3 <- ggplot(receita, aes(mes_ano, y = receita_milhoes)) + geom_line(color = "#db1020")+
  theme_bw()+
  labs(title = "Total de receita em São Paulo/Ano", x = "Ano", y = "Total de receita (em milhões)")
ggplotly(p3, tooltip = "y")
