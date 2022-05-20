library(pacman)
p_load('ggplot2','dplyr','ggtext','showtext','here')


eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

path <- here('r','scripts','2022','#tidytuesday','2022-05-17')

eurovision_unique <- eurovision |> 
  filter(section %in% c("final","grand-final")) |> 
  group_by(year) |> 
  arrange(year,rank) |>  
  mutate(n = cumsum(!duplicated(artist_country)))



eurovision_unique |> 
  filter(year == 2020) |> 
View()
#title: "Permanent Marker" ,"Bungee Inline" "Bungee" "Fredoka One"
#text: Montserrat
#annotation: "Kalam" "Gloria Hallelujah"

fonts <- list(h <- "Fredoka One",
              t <- "Montserrat"
)


font_add_google(fonts[[1]],bold.wt=700)
font_add_google(fonts[[2]],regular.wt = 500)
font_add_google(fonts[[2]],bold.wt = 400)
showtext_auto()

test <- MetBrewer::met.brewer('Tam',n=13,dir=1)

#point
#1970 - 5 countries boycut the eurovision due to 5 winners in 1969
# 2020 - covid
europlot <- eurovision_unique |> 
  filter(year != 2020) |> 
  ggplot(aes())+
  geom_point(aes(year,n,color=n,group=artist_country),shape="square",size=4.8,show.legend = F)+
  scale_color_gradientn(colours = MetBrewer::met.brewer('Tam',dir=-1))+
  scale_size(range = c(4,2))+
  scale_y_continuous(limits=c(0,30))+
  scale_x_continuous(limits=c(NA,2023),breaks =seq(1960,2020,by=10))+
  annotate('richtext',
           x=1961,
           y=23,
           label = glue::glue('Five countries boycotted <br> the 1970 edition in protest <br> of the four-way tie result <br>that had occurred in 1969'),
           label.color=NA,
           fill=NA,
           color='white',
           family=fonts[[2]],
           fontface='bold',
           size=3.3)+
  annotate('richtext',
           x=2023,
           y=30,
           label = glue::glue('Cancelled due<br>to Covid-19 <br>Pandemic'),
           label.color=NA,
           fill=NA,
           color='white',
           family=fonts[[2]],
           fontface='bold',
           size=3.3)+
  annotate('curve',
           x=1966.8,
           xend=1969.9,
           y=23,
           yend=17,
           size=.33,
           curvature = -.2,
           arrow = arrow(length = unit(1.5, "mm")),
           color='white')+
  annotate('curve',
           x=2020.3,
           xend=2019.9,
           y=30,
           yend=27,
           size=.33,
           curvature = .2,
           arrow = arrow(length = unit(1.5, "mm")),
           color='white')+
  #guides(color=guide_colorbar(direction = "horizontal",title = "Nof Countries",title.position = "top",title.hjust = .5))+
  theme_minimal()+
  theme(text = element_text(family =fonts[[2]]),
        #plot.background = element_rect(fill='black'),
        panel.grid = element_blank(),
        plot.title.position = 'plot',
        plot.title = element_markdown(family = fonts[[1]],
                                      color='white',
                                      size = 30,
                                      face='bold',
                                      lineheight = 1.2),
        plot.subtitle = element_markdown(family = fonts[[2]],
                                         color='white',
                                         size =15,
                                         margin = margin(b=20,t=2.5),
                                         lineheight = 1),
        axis.text = element_markdown(color='white'),
        axis.text.x = element_markdown(angle =45,
                                       size=11),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_markdown(hjust=.5,
                                        color='white',
                                        margin = margin(t=20),
                                        size=11),
        plot.margin = margin(25,20,25,20))+
  labs(title = glue::glue("THE <span style ='color:{test[1]}'>**E**</span><span style ='color:{test[2]}'>**U**</span><span style ='color:{test[3]}'>**R**</span><span style ='color:{test[4]}'>**O**</span><span style ='color:{test[5]}'>**V**</span><span style ='color:{test[6]}'>**I**</span><span style ='color:{test[7]}'>**S**</span><span style ='color:{test[8]}'>**I**</span><span style ='color:{test[9]}'>**O**</span><span style ='color:{test[10]}'>**N**</span> SONG CONTEST: <br>FROM EXCLUSIVE CLUB TO INCLUSIVE COMMUNITY"),
       subtitle = glue::glue("Since the 1960 an increasing number of countries have been participating in the Eurovision finals"),
       caption = glue::glue('Visualization: Frederik Rasmussen (@FrederikRasmus9) | **#TidyTuesday**, 2022-05-17 | Data source: Eurovision'),
       color=NA)


ggsave(here(path,'plots','tidytuesday_2022_05_17_eurovison.png'),
       europlot,
       dpi='retina',
       width=11.5,
       height=9.5,
       bg='black')
