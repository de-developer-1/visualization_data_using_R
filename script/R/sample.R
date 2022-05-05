#######################################
# library
#######################################
library(gapminder)
library(ggplot2)
library(ggrepel)

#######################################
# plot
#######################################
p <- ggplot2::ggplot(data = gapminder,
                     mapping = aes(x = gdpPercap,y = lifeExp))
p + geom_point()

#######################################
# plot1
#######################################
p <- ggplot2::ggplot(data = gapminder,
                     mapping = aes(x = gdpPercap,y = lifeExp))
p + geom_smooth()

#######################################
# plot2
#######################################
p <- ggplot2::ggplot(data = gapminder,
                     mapping = aes(x = gdpPercap,y = lifeExp))
#p + geom_smooth(method = "lm")
#p + geom_point() + geom_smooth(method = "lm")
p + geom_point() + geom_smooth(method = "gam") + scale_x_log10()

#######################################
# plot3
#######################################
p <- ggplot2::ggplot(data = gapminder,
                     mapping = aes(x = year,y = gdpPercap))
p + geom_line(color = "gray70",
              mapping = aes(group = country)) +
  geom_smooth(size = 1.1,
              method = "loess",
              se = FALSE) +
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~ continent,ncol = 5) +
  labs(x = "year",
       y = "log GDP per capita",
       title = "log GDP per capita on Five Continents")

#######################################
# plot4
#######################################
p <- ggplot2::ggplot(data = gapminder,
                     mapping = aes(x = gdpPercap,y = lifeExp,color = continent,
                                   fill = continent))
p + geom_point()+
  geom_smooth(method = "loess")+
  scale_x_log10()
