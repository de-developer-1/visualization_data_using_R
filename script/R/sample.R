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

