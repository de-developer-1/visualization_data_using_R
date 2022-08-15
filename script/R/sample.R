#######################################
# library
#######################################
library(gapminder)
library(ggplot2)
library(ggrepel)
library(socviz)
library(dplyr)


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
p <- ggplot2::ggplot(data = oecd_sum,
                     mapping = aes(x = year, y = diff ,fill = hi_lo))
p + geom_col() + guides(fill = "none") +
  labs(x = NULL , y = "Difference in Years",
       title = "アメリカの平均寿命格差",
       substitle = "1960-2015年におけるアメリカとOECD諸国の平均寿命の差",
       caption = "データ:チャート")

#######################################
# plot17
#######################################

rel_by_region <- gss_sm %>% 
  dplyr::group_by(bigregion,religion) %>% 
  dplyr::summarise(N = n()) %>% 
  dplyr::mutate(freq = N/sum(N),
         pct = round((freq * 100),0))

# 各地域における信徒数の割合
p<- ggplot(rel_by_region,aes(x =  bigregion, y = pct ,fill = religion))
p + geom_col(position ="dodge2") +
  labs(x = "Region", y = "percent", fill = "Religion") +
  theme(legend.position = "top")

# facetを用いた各地域における信徒数の割合
p <- ggplot(rel_by_region,aes(x = religion,y = pct, fill = religion))
p + geom_col(position ="dodge2") +
  labs(x = NULL, y = "percent", fill = "Religion") +
  guides(fill = "none") +
  coord_flip()+
  facet_grid(~bigregion)

#######################################
# plot18
#######################################
organdata %>% 
  select(1:6) %>% 
  slice_sample(n = 10)
organdata %>% names() %>% dput()
data <- organdata
# 散布図
p <- ggplot(data,mapping = aes(x = year,y = donors))
p + geom_point()

# facetで国別の時系列の出力
p <- ggplot(data,mapping = aes(x = year,y = donors))
p + geom_line(aes(group = country)) + facet_wrap(~ country)

# 国別の箱ひげ図
p <- ggplot(data,mapping = aes(x = country,y = donors))
p + geom_boxplot() + coord_flip()

# 臓器提供の平均値をもとにソート
p <- ggplot(data,mapping = aes(x = reorder(country,donors,na.rm = TRUE),y = donors))
p + geom_boxplot() + labs(x = NULL) +coord_flip()

# 臓器提供の平均値をもとにソート(着色)
p <- ggplot(data,mapping = 
              aes(x = reorder(country,donors,na.rm = TRUE),y = donors,
                  fill = world))
p + geom_boxplot() + 
  labs(x = NULL) + 
  coord_flip() +
  theme(legend.position = "top")

# 観測値
p <- ggplot(data,mapping = 
              aes(x = reorder(country,donors,na.rm = TRUE),y = donors,
                  color = world))
p + geom_point() + 
  labs(x = NULL) + 
  coord_flip() +
  theme(legend.position = "top")

# 観測値に揺らぎを与える
p <- ggplot(data,mapping = 
              aes(x = reorder(country,donors,na.rm = TRUE),y = donors,
                  color = world))

p + geom_jitter() + 
  labs(x = NULL) + 
  coord_flip() +
  theme(legend.position = "top")

# 揺らぎの大きさ調整
p + geom_jitter(position = position_jitter(width = 0.15)) + 
  labs(x = NULL) + 
  coord_flip() +
  theme(legend.position = "top")

# df作成
by_country <- data %>% 
  group_by(consent_law,country) %>% 
  dplyr::summarize(donors_mean = mean(donors,na.rm = TRUE),
           donors_sd = sd(donors,na.rm = TRUE),
           gdp_mean = mean(gdp,na.rm = TRUE),
           health_mean = mean(health,na.rm = TRUE),
           roads_mean = mean(roads,na.rm = TRUE),
           cerebvas_mean = mean(cerebvas,na.rm = TRUE))

# きれいな書き方
by_country <- data %>% 
  group_by(consent_law,country) %>% 
  dplyr::summarise_if(is.numeric,funs(mean,sd),na.rm = TRUE) %>% 
  ungroup()

# クリーブランドドットプロット
# 代入
data2 <- by_country 

p <- ggplot(data = data2,
            mapping = aes(x = donors_mean,y = reorder(country,donors_mean),
                          color = consent_law))
p + geom_point(size = 3) +
  labs(x = "Donor Procument Rate",
       y = "",
       color = "Consent Law") +
  theme(legend.position = "top")

# facetで分類
p <- ggplot(data = data2,
            mapping = aes(x = donors_mean,y = reorder(country,donors_mean),
                          color = consent_law))
p + geom_point(size = 3) +
  facet_wrap(~ consent_law,scales = "free_y",ncol = 1) +
  labs(x = "Donor Procument Rate",
       y = "") 

# 標準偏差を追記したドットプロット
p <- ggplot(data = data2,
            mapping = aes(x = reorder(country,donors_mean),y = donors_mean,
                          color = consent_law))
p + geom_pointrange(mapping = aes(ymin = donors_mean - donors_sd,
                                  ymax = donors_mean + donors_sd)) +
  labs(x = "Donor Procument Rate",
       y = "") 

# ラベル・テキストの追加
p <- ggplot(data = data2,mapping = aes(x = roads_mean,y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country))

# ラベルをずらす
p <- ggplot(data = data2,mapping = aes(x = roads_mean,y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country),hjust = 0)

# ggrepelを使ったテキストラベリング
elections_historic %>% select(2:7)
data3 <- elections_historic
p_title <- "タイトル"
p_subtitle <- "サブタイトル"
p_caption <- "キャプション"
x_label <- "x"
y_label <- "y"

p<- ggplot(data = data3,
           aes(x = popular_pct,y = ec_pct,label = winner_label))
p + 
  geom_hline(yintercept = 0.5,size = 1.4,color = "gray80") +
  geom_vline(xintercept = 0.5,size = 1.4,color = "gray80") +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = x_label,y = y_label,title = p_title,subtitle = p_subtitle,caption = p_caption)

# 単一条件のラベル・テキストの追加
p <- ggplot(data = data2,mapping = aes(x = gdp_mean,y = health_mean))
p + geom_point() +
  geom_text_repel(data = subset(data2,gdp_mean > 25000),mapping = aes(label = country))

# 複数条件のラベル・テキストの追加
p <- ggplot(data = data2,mapping = aes(x = gdp_mean,y = health_mean))
p + geom_point() +
  geom_text_repel(data = subset(data2,gdp_mean > 25000 | 
                                  health_mean < 1500 |
                                  country %in% "Belgium"),
                  mapping = aes(label = country))

# ダミー変数を利用したラベル付け
organdata$ind <- organdata$ccode %in% c("Ita","Spa") &
  organdata$year > 1998
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = ind))
p + geom_point() +
  geom_text_repel(data = subset(organdata,ind),
                  mapping = aes(label = ccode)) +
  guides(label = "none",color = "none")

# 任意のテキスト挿入
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors))
p + geom_point() + annotate(geom = "text",
                            x = 91,
                            y = 33,
                            label = "label",
                            hjust = 0)
# 2つの異なるgeomを利用
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors))
p + geom_point() + 
  annotate(geom = "rect",xmin = 125,xmax = 155,ymin = 30,ymax = 35,fill = "red",alpha = 0.2) +
  annotate(geom = "text",x = 157,y = 33,label = "label",hjust = 0)

# mapping
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point()

# scale
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
  scale_x_log10()+
  scale_y_continuous(breaks = c(5,15,25),
                     labels = c("Five","Fofteen","Twenty Five"))
# 再調整
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
  scale_color_discrete(labels = c("Corporatist","Liberal","Social Democratic","Unclassified"))+
  labs(x = "Road Deaths",
       y = "Donor Procurement",
       color = "Welfare State")

# guides
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
  labs(x = "Road Deaths",
       y = "Donor Procurement",
       color = "Welfare State") +
  guides(color = "none")
