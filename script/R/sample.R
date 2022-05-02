#######################################
# library
#######################################
library(gapminder)
library(ggplot2)

#######################################
# plot
#######################################
p <- ggplot2::ggplot(data = gapminder,
                     mapping = aes(x = gdpPercap,y = lifeExp))
p + geom_point()
