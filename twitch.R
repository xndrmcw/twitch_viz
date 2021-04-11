setwd("C:/Users/Work/Desktop/RStudio_viz/twitch")
library(tidyverse)
library(extrafont)
library(showtext)
library(shadowtext)
library(scales)
font_add("Twitch", "C:/Users/Work/Downloads/Twitchy.ttf")
font_add("MOD20", "C:/Users/Work/Downloads/MOD20.ttf")
showtext.auto()

# download and manipulate data. filtered based on what i thought was nice lol

tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

# filtered for games where the peak viewership is 50 times greater than average
# also made sure to remove games where there are at least ten viewers on average
# avg_peak_perc is weird but basically games with 0% shouldn't be considered

popularity_boom <- games %>% 
  filter(avg*50 < peak & avg_peak_perc != "0%" & avg >= 10) %>% 
  arrange(desc(peak))

# added spacing to game titles for graph clarity

popularity_boom[1,]$gamename = "LIFE IS\nSTRANGE 2"
popularity_boom[2,]$gamename = "THE TINY\nBANG STORY"
popularity_boom[3,]$gamename = "DISTRAINT"
popularity_boom[5,]$gamename = "LAYERS OF\nFEAR"
popularity_boom[6,]$gamename = "GUNS OF\nICARUS ONLINE"
popularity_boom <- popularity_boom[1:6,]

# initialize the blobs
s1 <- png::readPNG('icarus.png')
s1 <-  grid::rasterGrob(s1, interpolate=TRUE, width = .35)

s2 <- png::readPNG('fear.png')
s2 <-  grid::rasterGrob(s2, interpolate=TRUE, width = .35)

s3 <- png::readPNG('orwell.png')
s3 <-  grid::rasterGrob(s3, interpolate=TRUE, width = .35)

s4 <- png::readPNG('distraint.png')
s4 <-  grid::rasterGrob(s4, interpolate=TRUE, width = .35)

s5 <- png::readPNG('bang.png')
s5 <-  grid::rasterGrob(s5, interpolate=TRUE, width = .35)

s6 <- png::readPNG('strange.png')
s6 <-  grid::rasterGrob(s6, interpolate=TRUE, width = .35)

logo <- png::readPNG('logo.png')
logo <- grid::rasterGrob(logo, interpolate=TRUE, width = .5)

# SETTING THEME

theme_set(theme_dark())
theme_update(legend.position = "none",
             panel.background = element_rect((fill = "#6441A5")),
             plot.background = element_rect((fill = "#6441A5")),
             # library shadowtext to add black outline to title
             plot.title = element_shadowtext(family = "Twitch", color = "white",
                          size = rel(13), vjust = 1.3, hjust = .5),
             # got rid of the title, self explanatory
             axis.title.x.bottom = element_blank(),
             # basic aesthetic updating
             axis.title.y.left = element_text(size = rel(9), family = "Twitch", colour = "white"),
             axis.text.x.bottom = element_text(size = rel(5), lineheight = .3, family = "Twitch", colour = "white"),
             axis.text.y.left = element_text(size = rel(4), family = "Twitch", colour = "white"),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(colour = "white"),
             axis.line = element_line(size = 1, colour = "white"),
             axis.ticks = element_blank(),
             axis.text = element_text(colour = "white"))

p1 <- ggplot(data = popularity_boom) +
  geom_point(aes(x=reorder(gamename, peak), y=peak, colour = "black", size = 10)) +
  geom_point(aes(x=reorder(gamename, peak), y=avg, colour = "white", size = 10)) +
  labs(title = "TWITCH.TV BREAKOUT GAMES", x = "Game Name", y = "Viewer Count") + 
  scale_y_continuous(limits = c(0, 500000), labels = comma) +
  
  
  # adding dashed lines above points
  geom_segment(x = 1, y = 25000, yend = 102258, xend = 1, color = "white", size = 2, linetype = 'dashed') +
  geom_segment(x = 2, y = 25000, yend = 143872, xend = 2, color = "white", size = 2, linetype = 'dashed') +
  geom_segment(x = 3, y = 25000, yend = 150000, xend = 3, color = "white", size = 2, linetype = 'dashed') +
  geom_segment(x = 4, y = 25000, yend = 155000, xend = 4, color = "white", size = 2, linetype = 'dashed') +
  geom_segment(x = 5, y = 25000, yend = 162795, xend = 5, color = "white", size = 2, linetype = 'dashed') +
  geom_segment(x = 6, y = 25000, yend = 468000, xend = 6, color = "white", size = 2, linetype = 'dashed') +
  
  #adding logos
  annotation_custom(grob = logo, xmin = .45,
                    xmax = 1.45, ymax = 1050000) +
  annotation_custom(grob = s1, xmin = .5,
                     xmax = 1.5, ymax = 235000) +
  annotation_custom(grob = s2, xmin = 1.5,
                    xmax = 2.5, ymax = 325000) +
  annotation_custom(grob = s3, xmin = 2.5,
                    xmax = 3.5, ymax = 335000) +
  annotation_custom(grob = s4, xmin = 3.5,
                    xmax = 4.5, ymax = 360000) +
  annotation_custom(grob = s5, xmin = 4.5,
                    xmax = 5.5, ymax = 365000) +
  annotation_custom(grob = s6, xmin = 5.5,
                    xmax = 6.5, ymax = 1000000) +
  annotate(geom = "label", x = 2.3, y = 440000, size = 14, ## label
           label = "Are there 'one-hit-wonder' video games?
           Blue points represent average viewership,
           icons represent peak viewership!", 
           family = "Twitch",
           lineheight = 0.3,
           color = "black",
           #label.size = NA,
           fill = "#FFFFFF") +
  annotate(geom = "label", x = 1, y = 170000, size = 12, ## label
           label = "Guns of Icarus Online:\n Average Viewers: 2,029\n Peak Viewers: 102,258\n 50.4x Multiplier!", 
           family = "MOD20",
           lineheight = 0.3,
           color = "#6441A5",
           #label.size = NA,
           fill = "#FFFFFF") +
  annotate(geom = "label", x = 2, y = 215000, size = 12, ## label
           label = "Layers of Fear:\nAverage Viewers: 2,283\nPeak Viewers: 143,872\n63x Multiplier!", 
           family = "MOD20",
           lineheight = 0.3,
           color = "#6441A5",
           #label.size = NA,
           fill = "#FFFFFF") +
  annotate(geom = "label", x = 3, y = 220000, size = 12, ## label
           label = "Orwell:\nAverage Viewers: 2,098\nPeak Viewers: 150,575\n71.7x Multiplier!", 
           family = "MOD20",
           lineheight = 0.3,
           color = "#6441A5",
           #label.size = NA,
           fill = "#FFFFFF") +
  annotate(geom = "label", x = 4, y = 233000, size = 12, ## label
           label = "Distraint:\nAverage Viewers: 2,925\nPeak Viewers: 162,795\n55.6x Multiplier!", 
           family = "MOD20",
           lineheight = 0.3,
           color = "#6441a5",
           #label.size = NA,
           fill = "#FFFFFF") +
  annotate(geom = "label", x = 5, y = 235000, size = 12, ## label
           label = "The Tiny Bang Story:\nAverage Viewers: 3,029\nPeak Viewers: 163,735\n54x Multiplier!", 
           family = "MOD20",
           lineheight = 0.3,
           color = "#6441A5",
           #label.size = NA,
           fill = "#FFFFFF") +
  annotate(geom = "label", x = 6, y = 425000, size = 12, ## label
           label = "Life is Strange 2:\nAverage Viewers: 2,729\nPeak Viewers: 468,634\n171.7x Multiplier!!!", 
           family = "MOD20",
           lineheight = 0.3,
           color = "#6441A5",
           #label.size = NA,
           fill = "#FFFFFF") 
  

p1

ggsave(here::here("twitch.png"), device = "png",
       type = "cairo", width = 17, height = 8, dpi = 300)
 
