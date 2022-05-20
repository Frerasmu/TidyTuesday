library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(patchwork)
library(here)
library(showtext)


solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
price <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')
cap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')

#story
#solar projected price and wind projected price (mwh). Calculated % decrease relative to first data point.
#plot geom_point for each + a trend line going through
# take away:
# prices for wind and solar has decreased, but more so for solar

path <- here('r','scripts','2022','#tidytuesday','2022-05-03')

##

fonts <- list(h <- "Koulen",
              t <- "Raleway"
)

font_add_google(fonts[[1]])
font_add_google(fonts[[2]])
showtext_auto()

#colors
cols <- MetBrewer::met.brewer('OKeeffe2',13,dir=-1)
cols[5] <- MetBrewer::met.brewer('OKeeffe2',13,dir=-1)[3] 
cols[4] <- MetBrewer::met.brewer('OKeeffe2',13,dir=-1)[3]

###

sun <- price |> 
  ggplot(aes(1,as.factor(-year),fill=solar_mwh))+
  geom_col()+
  guides(fill=guide_colorbar(direction = "horizontal",title = "Solar $/MWH",title.position = "top",title.hjust = .5))+
  scale_fill_gradientn(colors=MetBrewer::met.brewer('OKeeffe2',dir=-1))+
  theme_minimal()+
  coord_polar()+
  annotate('richtext',x=.5,y=as.factor(-price$year),label = as.factor(price$year),size=4,
           color=cols,
           family = 'Raleway',fontface='bold',label.color=NA,fill=NA)+
  scale_y_discrete(breaks=as.factor(-price$year),labels=as.factor(price$year))+
  theme(text=element_text(family=fonts[[2]]),
        axis.title = element_blank(),
        axis.text=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        panel.grid = element_blank(),
        plot.title = element_markdown(hjust=.5,
                                      size=30,
                                      face='bold',
                                      family=fonts[1],
                                      margin=margin(t=7.5)),
        plot.margin = margin(15,15,15,15),
        plot.caption = element_markdown(hjust=.5,
                                        margin = margin(t=30)))+
  labs(title=glue::glue("The Avereage Price of Solar Power <br>has Been Going Down Every Year Since 2009"),
       caption = glue::glue('Visualization: Frederik Rasmussen (@FrederikRasmus9) | **#TidyTuesday**, 2022-05-03 | Data source: Berkely Lab'))



ggsave(here(path,'plot','tidytuesday_2022_05_03_sunpower.png'),
       sun,
       width=8,
       height=9,
       bg='grey85',
       dpi='retina')

