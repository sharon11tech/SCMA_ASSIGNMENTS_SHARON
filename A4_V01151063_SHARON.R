#too see existing dataset
data()
mtcars
a=mtcars
?mtcars
??mtcars
##plotting in base R####
head(mtcars)
mtcars$mpg
mtcars$cyl
#scatter plot-relation btw 2 variables
plot(y=mtcars$mpg,x=mtcars$cyl)

#line plot
plot(y=mtcars$mpg,x=mtcars$cyl,type="l")
names(mtcars)

#outliers
boxplot(mtcars$mpg,mtcars$qsec,col="red")
attach(mtcars)

#histogram
hist(cyl,col="green",main="histogram")
?hist
#barplot


require(ggplot2)
library(ggplot2)


ggplot(data=mtcars, mapping=aes(x=mpg,y=qsec))+
  geom_point(col="green",size=5)+
  geom_line(col="red",lwd=1)+
  theme_minimal()+
  labs(
    title="relation",
    subtitle="rajagiri",
    x="mileage",
    y="prformance",
    caption="hehe"
    
  )+
  facet_wrap(-am)

install.packages("tidyverse")
require(tidyverse)  
install.packages("dplyr")  
require(dplyr)
require(ggplot2)
mtcars %>%
  select(gear, mpg, wt, vs)%>%
  mutate(efficiency=mpg/wt)%>% 
  ggplot(aes(efficiency))+
  geom_histogram(bins= 4,fill="red")
  
  
  
#plottinggplot2#plotting in ggplot 2####