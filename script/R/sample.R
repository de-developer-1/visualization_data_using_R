#######################################
# library
#######################################
library(gapminder)
library(ggplot2)
library(ggrepel)
library(socviz)

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

#######################################
# plot5
#######################################
p <- ggplot2::ggplot(data = gss_sm,
                     mapping = aes(x = bigregion))
p + geom_bar()

#######################################
# plot6
#######################################
p <- ggplot2::ggplot(data = gss_sm,
                     mapping = aes(x = bigregion))
p + geom_bar(mapping = aes(y = after_stat(prop),group = 1))

#######################################
# plot7
#######################################
p <- ggplot2::ggplot(data = gss_sm,
                     mapping = aes(x = religion,fill = religion))
p + geom_bar() + guides(fill = "none")

#######################################
# plot8
#######################################
p <- ggplot2::ggplot(data = gss_sm,
                     mapping = aes(x = bigregion,fill = religion))
p + geom_bar(position = "fill") 

#######################################
# plot9
#######################################
p <- ggplot2::ggplot(data = gss_sm,
                     mapping = aes(x = bigregion,fill = religion))

p + geom_bar(position = "dodge",
             mapping = aes(y = after_stat(prop),
                           group = religion))

#######################################
# plot10
#######################################
p <- ggplot2::ggplot(data = gss_sm,
                     mapping = aes(x = religion))
p + geom_bar(position = "dodge",
             mapping = aes(y = after_stat(prop),
                           group = bigregion)) +
  facet_wrap( ~ bigregion,ncol = 2) 

#######################################
# plot11
#######################################
p <- ggplot2::ggplot(data = midwest,
                     mapping = aes(x = area))
p + geom_histogram(bins = 10)

#######################################
# plot12
#######################################
oh_wi <- c("OH","WI")
p <- ggplot2::ggplot(data = subset(midwest,subset = state %in% oh_wi),
                     mapping = aes(x = percollege,fill = state))
p + geom_histogram(alpha = 0.4,bins = 20)

#######################################
# plot13
#######################################
p <- ggplot2::ggplot(data = midwest,
                     mapping = aes(x = area,fill = state, color = state))
p + geom_density(alpha = 0.3)

#######################################
# plot14
#######################################
oh_wi <- c("OH","WI")
p <- ggplot2::ggplot(data = subset(midwest,subset = state %in% oh_wi),
                     mapping = aes(x = area,fill = state,color = state))
p + geom_density(alpha = 0.3,
                 mapping = (aes (y=after_stat(scaled))))

#######################################
# plot15
#######################################
p <- ggplot2::ggplot(data = titanic,
                     mapping = aes(x = fate,y = percent, fill = sex))
p + geom_bar(position = "dodge",stat = "identity") + theme(legend.position = "top")

#######################################
# plot16
#######################################
