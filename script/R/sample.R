#######################################
# library
#######################################
library(gapminder)
library(ggplot2)
library(ggrepel)
library(socviz)
library(dplyr)
library(broom)
library(survival)
library(margins)
library(survey)
library(srvyr)
library(coefplot)
library(GGally)
library(maps)
library(ggthemes)
library(statebins)
library(viridis)
library(tidyverse)

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

#######################################
# plot18
#######################################
# data
p <- ggplot(data = gapminder,
            mapping = aes(x = log(gdpPercap), y = lifeExp))

# ロバスト線形単回帰モデル
p + geom_point(alpha = 0.1) +
  geom_smooth(color = "tomato",fill = "tomato",method = MASS::rlm) +
  geom_smooth(color = "steelblue",fill = "steelblue",method = "lm")

# 多項式回帰モデル
p + geom_point(alpha = 0.1) +
  geom_smooth(color = "tomato",method =　"lm",size = 1.2,
              formula = y ~ splines::bs(x,df = 3),se = FALSE) 

# 分位点回帰モデル
# https://stackoverflow.com/questions/59184868/geom-quantile-full-range-in-ggplot2
# https://qiita.com/kenmatsu4/items/03739db76aa010b7c6fe
p + geom_point(alpha = 0.1) +
  geom_quantile(color = "tomato",size = 1.2,method =　"rqss",
              lambda = 1,quantiles = c(0.20,0.5,0.85)) 

# カラーパレット
model_colors <- RColorBrewer::brewer.pal(3,"Set1")

# モデルへの凡例追加
p0 <- ggplot(data = gapminder,
            mapping = aes(x = log(gdpPercap), y = lifeExp))

p1 <- p0 + geom_point(alpha=0.2) +
  geom_smooth(method = "lm",aes(color = "OLS",fill = "OLS")) +
  geom_smooth(method = "lm",formula = y ~ splines::bs(x,df = 3),
              aes(color = "Cubic Spline",fill = "Cubic Spline")) +
  geom_smooth(method = "loess",
              aes(color = "LOESS",fill = "LOESS"))
p1 + scale_color_manual(name = "Models",values = model_colors) +
  scale_fill_manual(name = "Models",values = model_colors) +
  theme(legend.position = "top")

# 線形モデルによる予測の可視化
# data加工
# https://qiita.com/maech/items/83d1b3a85b976ffcdaf6
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,data = gapminder)
summary(out)

min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)
pred_df <- expand.grid(gdpPercap = (seq(from = min_gdp,
                                        to = max_gdp,
                                        length.out = 100)),
                       pop = med_pop,
                       continent = c("Africa","Americas",
                                     "Asia","Europe","Oceania"))
dim(pred_df)

pred_out <- predict(object = out,
                    newdata = pred_df,
                    interval = "predict")
head(pred_out,5)

pred_df <- cbind(pred_df,pred_out)
head(pred_df,5)

p <- ggplot(data = subset(pred_df,continent %in% c("Africa","Europe")),
            aes(x = gdpPercap,
                y = fit,ymin = lwr,ymax = upr,
                color = continent,
                fill = continent,
                group = continent)
            )
p + geom_point(data = subset(gapminder,
                             continent %in% c("Africa","Europe")),
               aes(x = gdpPercap,y = lifeExp,
                   color = continent),
               alpha = 0.5,
               inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.2,color = NA) +
  scale_x_log10(labels  = scales::dollar)

# 線形回帰モデルの推定量を用いた図
# data作成
out_comp <- tidy(out)
p <- ggplot(out_comp,mapping = aes(x = term,
                                   y = estimate))
p + geom_point() +coord_flip()

# ラベルの編集・信頼区間の追加を行った線形回帰モデル
out_conf <- tidy(out,conf.int = TRUE) %>% 
  subset(.,term %nin% "(Intercept)")
out_conf$nicelabs <- prefix_strip(out_conf$term,"continent")

p <- ggplot(out_conf,mapping = aes(x = reorder(nicelabs,estimate),
                                   y = estimate,ymin = conf.low,ymax = conf.high))
p + geom_pointrange()+ coord_flip() +labs(x = NULL,y = "OLS estimate")

# 予測値・残差プロット
out_aug <- augment(out,data = gapminder)
p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted,y = .resid))
p + geom_point()

# カプランマイヤー生存曲線
out_cph <- coxph(Surv(time,status)~age + sex,data = lung)
out_surv <- survfit(out_cph)
out_tidy <- tidy(out_surv)

p <- ggplot(data = out_tidy,mapping = aes(time,estimate))
p + geom_line()+
  geom_ribbon(mapping = aes(ymin = conf.low,ymax = conf.high),alpha = 0.2)

# 大陸ごとに層別したGDPと平均寿命に関する推定値
fit_ols <- function(df){
  lm(lifeExp ~ log(gdpPercap),data = df)
}

eu77 <- gapminder %>% 
  filter(continent == "Europe",year == 1977)

fit <- lm(lifeExp ~ log(gdpPercap),data = eu77)
summary(fit)

out_tidy <- gapminder %>% 
  group_by(continent,year) %>% 
  nest() %>% 
  mutate(model = map(data,fit_ols),
         tidied = map(model,tidy)) %>% 
  unnest(tidied) %>% 
  select(!c(data,model)) %>% 
  filter(term %nin% "(Intercept)" &
           continent %nin% "Oceania")

p <- ggplot(data = out_tidy,
            mapping = aes(x = year,y = estimate,
                          ymin = estimate - 2 * std.error,
                          ymax = estimate + 2 * std.error,
                          group = continent,color = continent))
p + geom_pointrange(position = position_dodge(width = 1)) +
  scale_x_continuous(breaks = unique(gapminder$year)) +
  theme(legend.position = "top")+
  labs(x = "Year", y = "Estimate",color = "Continent")

#平均限界効果
gss_sm$polviews_m <- relevel(gss_sm$polviews,ref = "Moderate")
out_bo <- glm(obama ~ polviews_m + sex*race,
              family = "binomial",data = gss_sm)
summary(out_bo)

bo_m <- margins(out_bo)
summary(bo_m)

bo_gg <- as_tibble(summary(bo_m))
prefixes <- c("polvies_m","sex")
bo_gg$factor<-prefix_strip(bo_gg$factor,prefixes)
bo_gg$factor<-prefix_replace(bo_gg$factor,"race","Race: ")

p<- ggplot(data = bo_gg,
           aes(x = reorder(factor,AME),
               y = AME,ymin = lower,ymax = upper
               ))
p + geom_hline(yintercept = 0,color = "gray80")+
  geom_pointrange()+coord_flip()+
  labs(x = NULL,y = "Average Marginal Effect")

# 条件付き効果の可視化
pv_cv <- cplot(out_bo,x = "sex",draw = FALSE)
p <- ggplot(data = pv_cv,aes(x = reorder(xvals,yvals),
                             y = yvals,ymin = lower,ymax = upper))
p + geom_hline(yintercept = 0,color = "gray80")+
  geom_pointrange()+coord_flip()+
  labs(x = NULL,y = "Conditional Effect")

# 複雑な調査データの可視化
# データ加工
options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")

gss_wt <- subset(gss_lon,year>1974) %>% 
  mutate(stratvar = interaction(year,vstrat)) %>% 
  as_survey_design(ids = vpsu,
                   strata = stratvar,
                   weight = wtssall,
                   nest = TRUE)

out_grp <- gss_wt %>% 
  filter(year %in% seq(1976,2016,by = 4)) %>% 
  group_by(year,race,degree) %>% 
  summarize(prop = survey_mean(na.rm = TRUE))

out_mrg <- gss_wt %>% 
  filter(year %in% seq(1976,2016,by = 4)) %>% 
  mutate(racedeg = interaction(race,degree)) %>% 
  group_by(year,race,degree) %>% 
  summarize(prop = survey_mean(na.rm = TRUE)) %>% 
  separate(racedeg,sep = "\\.",into = c("race","degree"))

# 教育歴の加重推定値
p <- ggplot(data = subset(out_grp,race %nin% "Other"),
            mapping = aes(x = degree,
                          y = prop,
                          ymin = prop - 2*prop_se,
                          ymax = prop + 2*prop_se,
                          fill = race,
                          color = race,
                          group = race))
dodge <- position_dodge(width = 0.9)
p + geom_col(position = dodge,alpha = 0.2)+
  geom_errorbar(position = dodge,width = 0.2)+
  scale_x_discrete(labels = scales::wrap_format(10))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_brewer(type = "qual", palette = "Dark2")+
  scale_fill_brewer(type = "qual", palette = "Dark2")+
  labs(title = "title",
       subtitle = "subtitle",
       fill = "Race",
       color = "Race",
       x = NULL,y = "Percent")+
  facet_wrap(~ year,ncol = 2) +
  theme(legend.position = "top")

#　教育歴でfacet
p <- ggplot(data = subset(out_grp,race %nin% "Other"),
            mapping = aes(x = year,
                          y = prop,
                          ymin = prop - 2*prop_se,
                          ymax = prop + 2*prop_se,
                          fill = race,
                          color = race,
                          group = race))

p + geom_ribbon(alpha = 0.2,aes(color = NULL))+
  geom_line()+
  facet_wrap(~ degree,ncol = 1)+
  scale_y_continuous(labels = scales::percent)+
  scale_color_brewer(type = "qual", palette = "Dark2")+
  scale_fill_brewer(type = "qual", palette = "Dark2")+
  labs(title = "title",
       subtitle = "subtitle",
       fill = "Race",
       color = "Race",
       x = NULL,y = "Percent")+
  theme(legend.position = "top")

# coefplotを使った可視化
out <- lm(formula = lifeExp ~ log(gdpPercap)+log(pop)+continent,
          data = gapminder)
coefplot(out,sort = "magnitude",intercept = FALSE)

# 散布図行列
organdata_sm <- organdata %>% 
  select(donors,pop_dens,pubhealth,roads,consent_law)
ggpairs(data = organdata_sm,mapping = aes(color = consent_law),
        upper = list(continuous = wrap("density"),combo = "box_no_facet"),
        lower = list(continuous = wrap("points"),combo = "dot_no_facet"))

#######################################
# plot19
#######################################
# 大統領選挙の結果
party_colors <- c("#2E74C0","#CB454A")
p0 <- ggplot(data = subset(election,st %nin% "DC"),
             mapping = aes(x = r_points,
                           y = reorder(state,r_points),
                           color = party))
p1 <- p0 + geom_vline(xintercept = 0,color = "gray30") +
  geom_point(size = 2)
p2 <- p1 + scale_color_manual(values = party_colors)
p3 <- p2 + scale_x_continuous(breaks = c(-30,-20,-10,0,10,20,30,40),
                              labels = c("30\n (Clinton)","20","10","0",
                                         "10","20","30","40\n(Trump)"))
p3 + facet_wrap(~ census,ncol = 1,scales = "free_y") + 
  guides(color = "none") + labs(x = "point Margin",y = "") +
  theme(axis.text = element_text(size = 8))

# アメリカの白地図
us_states <- map_data("state")
p <- ggplot(data = us_states,
            mapping = aes(x = long,y = lat,
                          group = group))
p + geom_polygon(fill = "white",color = "black")

# 州ごとの塗分け
p <- ggplot(data = us_states,
            mapping = aes(x = long,y = lat,
                          group = group,fill = region))
p + geom_polygon(color = "gray90",size = 0.1) +
  guides(fill = "none")

# 投影方法の変更
p <- ggplot(data = us_states,
            mapping = aes(x = long,y = lat,
                          group = group,fill = region))
p + geom_polygon(color = "gray90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45) +
  guides(fill = "none")

# 選挙結果
election$region <- tolower(election$state)
dat <- left_join(us_states,election)

p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          group = group,fill = party))
p + geom_polygon(color = "gray90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45) 

# 州ごとの結果
p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          group = group,fill = party))
p1 <- p + geom_polygon(color = "gray90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45) 
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Title",fill = NULL)
p2 + theme_map()


# 得票率
p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          group = group,fill = pct_trump))
p1 <- p + geom_polygon(color = "gray90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45) 
p1 + labs(title = "Vote")+ theme_map()+labs(fill = "percent")

p2 <- p1 + scale_fill_gradient(low = "white",high = "#CB454A") + 
  labs(title = "Title")
p2 + theme_map() + labs("Percent")

# 対立
p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          group = group,fill = d_points))
p1 <- p + geom_polygon(color = "gray90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45) 
p2 <- p1 + scale_fill_gradient2() + labs(title = "Title")
p2 + theme_map() + labs("Percent")

p3 <-  p1 + scale_fill_gradient2(low = "red",
                                 mid = scales::muted("purple"),
                                 high = "blue",
                                 breaks = c(-25,0,25,50,75)) + labs(title = "Title")
p3 + theme_map() + labs(fill = "Percent")

# 一部データ除外
p <- ggplot(data = subset(dat,region %nin% "district of columbia" ),
            mapping = aes(x = long,y = lat,
                          group = group,fill = d_points))
p1 <- p + geom_polygon(color = "gray90",size = 0.1) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45) 
p3 <-  p1 + scale_fill_gradient2(low = "red",
                                 mid = scales::muted("purple"),
                                 high = "blue",
                                 breaks = c(-25,0,25,50,75)) + labs(title = "Title")
p3 + theme_map() + labs(fill = "Percent")

#群単位での人口密度
dat <- county_map %>% 
  left_join(.,county_data,by = "id")

p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          fill = pop_dens,
                          group = group))
p1 <- p +  geom_polygon(color = "gray90",size = 0.05) + coord_equal() 
p2 <- p1 + scale_fill_brewer(palette = "Blues",
                             labels = c("0-10","10-50","50-100","100-500",
                                        "500-1,000","1,000-5,000",">5,000"))
p2 + labs(fill = "Population per\nsquare mile") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom")

# アフリカ系アメリカ人の人口割合
p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          fill = pct_black,
                          group = group))
p1 <- p +  geom_polygon(color = "gray90",size = 0.05) + coord_equal() 
p2 <- p1 + scale_fill_brewer(palette = "Greens")
p2 + labs(fill = "US Population,Pecent Black") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_map() +
  theme(legend.position = "bottom")

# カラーパレットの反転
orange_pal <- RColorBrewer::brewer.pal(n = 6,name = "Oranges")
orange_rev <- rev(orange_pal)

gun_p <- ggplot(data = dat,
            mapping = aes(x = long,y = lat,
                          fill = su_gun6,
                          group = group))
gun_p1 <- gun_p +  geom_polygon(color = "gray90",size = 0.05) + coord_equal() 
gun_p2 <- gun_p1 + scale_fill_manual(values = orange_pal)
gun_p2 + labs(title = "Title",fill = "Rate per 100,000 pop.") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_map() +
  theme(legend.position = "bottom")

pop_p <- ggplot(data = dat,
                mapping = aes(x = long,y = lat,
                              fill = pop_dens6,
                              group = group))
pop_p1 <- pop_p +  geom_polygon(color = "gray90",size = 0.05) + coord_equal() 
pop_p2 <- pop_p1 + scale_fill_manual(values = orange_rev)
pop_p2 + labs(title = "Title",fill = "People per square mile") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_map() +
  theme(legend.position = "bottom")

# state bins
  statebins(
       state_data = election,
       state_col = "state",
       value_col = "pct_trump",
       dark_label = "white",
       name = "Percent Trump",
       palette = "Reds",
       direction = 1,
       font_size = 3
     ) + theme_statebins(legend_position = "top")
  
statebins(
    state_data = subset(election,st %nin% "DC"),
     state_col = "state",
     light_label = "black",
     value_col = "pct_clinton",
     name = "Percent Clinton",
   palette = "Blues",
     font_size = 3
   ) + theme_statebins(legend_position = "top")

statebins(
  state_data = election,
  state_col = "st",
  value_col = "party",
  light_label = "white",
  name = "Winner",
  font_size = 3,
  ggplot2_scale_function = scale_fill_manual,
  values = c(Republican = "darkred",
             Democratic = "royalblue"),
  labels = c("Clinton","Trump")
) + theme_statebins(legend_position = "right")

election %>% 
  mutate(pct_trump = cut(pct_trump,breaks = 4,
                         labels = c("4-21","21-37","37-53","53-70"))) %>% 
  statebins(
    value_col = "pct_trump",
    dark_label = "white",
    name = "Percent Trump",
    palette = "Reds",
    ggplot2_scale_function = scale_fill_brewer,
    font_size = 3
  ) + theme_statebins(legend_position = "top")

# 複数の地図を１枚にまとめる
opiates$region <- tolower(opiates$state)
dat <- left_join(us_states,opiates)

p0 <- ggplot(data = subset(dat,year > 1999),
             mapping = aes(x = long, y = lat,
                           group = group,
                           fill = adjusted))

p1 <- p0 + geom_polygon(color = "gray90",size = 0.05) +
  coord_map(projection = "albers",lat0 = 39,lat1 = 45)

p2 <- p1 + scale_fill_viridis_c(option = "plasma")

p2 + theme_map() + facet_wrap(~ year,ncol = 3)+
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "fill",title = "title")

# 全データ
p <- ggplot(data = opiates,
            mapping = aes(x = year,y = adjusted,
                          group = state))
p + geom_line(color = "gray70")

#時系列データの区画化
p0 <- ggplot(data = drop_na(opiates,division_name),
             mapping = aes(x = year,y = adjusted))
p1 <- p0 + geom_line(color = "gray70",
                     mapping = aes(group=state))
p2 <- p1 + geom_smooth(mapping = aes(group = division_name),
                       se = FALSE)
p3 <- p2 + geom_text_repel(data = subset(opiates,
                                         year == max(year) & abbr != "DC"),
                           mapping = aes(x = year,y = adjusted,label = abbr),
                           size = 1.8,segment.color = NA,nudge_x = 30) +
  coord_cartesian(c(min(opiates$year),
                    max(opiates$year)))

p3 + labs(x = "",y = "Rate per 100,000 population",
          title = "Title") +
  facet_wrap(~ reorder(division_name,-adjusted,na.rm = TRUE),nrow = 3)

#######################################
# plot19
#######################################
# data
p <- ggplot(data = subset(asasec, Year == 2014),
            mapping = aes(x = Members,
                          y = Revenues,
                          label = Sname))
p + geom_point() + geom_smooth()

# data
p <- ggplot(data = subset(asasec, Year == 2014),
            mapping = aes(x = Members,
                          y = Revenues,
                          label = Sname))
p + geom_point(mapping = aes(color = Journal)) + geom_smooth(mathod = "lm")

# data
p <- ggplot(data = subset(asasec, Year == 2014),
            mapping = aes(x = Members,
                          y = Revenues,
                          label = Sname))
p1 <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "gray80") +
  geom_point(mapping = aes(color = Journal))

p2 <- p1 +
  geom_text_repel(data = subset(asasec, Year == 2014 & Revenues > 7000),size = 2)

p3 <- p2 +
  labs(x = "Membership",y = "Revenues",
       color = "Section",title = "ASA",
       subtitle = "subtitle",
       caption = "Source: ASA")

p4 <- p3 +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "bottom")

p4
