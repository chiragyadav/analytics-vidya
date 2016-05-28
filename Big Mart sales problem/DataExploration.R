rm(list = ls())
library(dplyr)
library(ggplot2)
setwd('C:/Users/chirag/Downloads/Analytics Vidya Problems Datasets/Big Mart sales problem')
x <- read.csv('Train.csv',stringsAsFactors = FALSE)
numerical_col <- names(x)[sapply(x,is.numeric)]
x %>% select(-one_of(numerical_col),-Item_Identifier)%>% sapply(unique)

x %>%
  group_by(Item_Type,Item_Identifier) %>% 
  summarise(count=n(), avgsales=mean(Item_Outlet_Sales))%>% arrange(desc(avgsales))


x.tier1 <- x %>% filter(Outlet_Location_Type=='Tier 1')
summary(x.tier1)
str(x.tier1)


x.tier1 %>% select(-one_of(numerical_col),-Item_Identifier)%>% sapply(unique)

# Data Cleaning for Item_Fat_content Column
x.tier1$Item_Fat_Content[x.tier1$Item_Fat_Content %in% c('low fat','LF')]= 'Low Fat'
x.tier1$Item_Fat_Content[x.tier1$Item_Fat_Content %in% c('reg')]= 'Regular'

x.tier1 %>% group_by(Outlet_Type,Item_Type) %>%
  summarise(avgsales=mean(Item_Outlet_Sales)) %>%
  arrange(desc(avgsales)) %>% as.data.frame()

x.tier1 %>% group_by(Outlet_Type) %>%
  summarise(totalsales=sum(Item_Outlet_Sales), count=n())%>% as.data.frame()

x.tier1 %>% select(one_of(numerical_col)) %>% cor()



x.tier1 %>% group_by(Item_Type,Outlet_Size) %>% 
  summarise(avgsales=mean(Item_Outlet_Sales)) %>% as.data.frame()

x.tier1 %>% ggplot(aes(x=Item_Type)) + geom_bar() + facet_grid(Outlet_Size~.)

# this is to check the relationship of visibility and sales for each in different type of
#outlet types
x.tier1 %>% ggplot(aes(y=Item_Outlet_Sales,x=Item_Visibility)) + geom_point()+
  geom_smooth() + facet_grid(Outlet_Type~.) 