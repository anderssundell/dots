rm(list=ls(all=TRUE))
library(tidyverse)
library(ggpubr)
library(readxl)
library(stargazer)
library(RColorBrewer)
library(gridExtra)

colnr <- 20

titletext <- "IMAGINE"

text <- "Imagine there's no countries
It isn't hard to do
Nothing to kill or die for
And no religion, too
Imagine all the people
Living life in peace
You, you may say I'm a dreamer
But I'm not the only one
I hope someday you will join us
And the world will be as one
Imagine no possessions
I wonder if you can
No need for greed or hunger
A brotherhood of man
Imagine all the people
Sharing all the world
You, you may say I'm a dreamer
But I'm not the only one
I hope someday you will join us
And the world will live as one"

# Clean text
cleaned <- str_replace_all(text, "[\t\r,.—-]", "") %>%
  str_replace_all(c("[0123456789]"), "") %>%
  str_replace_all(c("[,;:!=.'`?’]"), "") %>%
  str_replace_all(c("\n"), " ") %>%
  tolower();

# Dimensions
chars <- nchar(cleaned)
rownr <- ceiling(chars/colnr)
allchars <- rownr*colnr

# Split string
letter <- str_split_fixed(cleaned, "", n = nchar(cleaned))
letter <- tolower(letter)
letter[(chars+1):allchars] <- NA

# Assing positions
id <- 1:allchars
xpos <- as.double((id-1) %% colnr)
ypos <- rep(rownr:1, each = colnr)

df <- as.data.frame(cbind(xpos, ypos))
df <- cbind(df, letter)

# Make palette
letterlist <- levels(as.factor(letter))
letterlist <- letterlist[2:length(letterlist)] # Remove first element of the list
nrcolors <- length(letterlist)

mycolors <- colorRampPalette(brewer.pal(12, "Accent"))(nrcolors)

colorlist <- as.data.frame(cbind(letterlist, mycolors)) %>%
  rename("ltr" = 1)

# Make main graph
maingraph <- df %>%
  filter(is.na(letter)==FALSE & letter!= " ") %>%
  ggplot(aes(x=xpos, y=ypos, col=as.factor(letter))) +
  geom_point() +
  theme_void() +
  scale_color_manual(values=mycolors) +
  theme(legend.position = "none")

# Make title
letter_title <- as.vector(str_split_fixed(titletext, "", n = nchar(titletext)))
xpos_title <- 1:length(letter_title)
df_title <- as.data.frame(xpos_title)
df_title <- cbind(df_title, letter_title)

letterlist_title <- as.data.frame(tolower(levels(as.factor(letter_title)))) %>%
  rename("ltr" = 1) %>%
  filter(ltr!=" ")
  
letterlist_title <- left_join(letterlist_title, colorlist, by = "ltr")
mycolors_title <- as.vector(letterlist_title[,2])

# Title graph
titlegraph <- df_title %>%
  filter(letter_title!=" ") %>%
  ggplot(aes(x=xpos_title, y=1, col=as.factor(letter_title), label=letter_title)) +
  geom_point() +
  geom_text(nudge_y = 1, col = "black") +
  theme_void() +
  scale_color_manual(values=mycolors_title) +
  theme(legend.position = "none") +
  ylim(0, 3.5)

# Make Legend
xpos_legend <- 1:nrcolors # X position
df_legend <- as.data.frame(xpos_legend) # Create data frame
df_legend <- cbind(df_legend, letterlist) # Combine with the lett

# Legend graph
legendgraph <- df_legend %>%
  ggplot(aes(x=xpos_legend, y=1, col=as.factor(letterlist), label=letterlist)) +
  geom_point() +
  geom_text(nudge_y = -1, col = "#555555") +
  theme_void() +
  scale_color_manual(values=mycolors[1:nrcolors]) +
  theme(legend.position = "none") +
  ylim(-1, 2.5)

# Put it together
ggarrange(titlegraph, maingraph, legendgraph, nrow = 3, ncol = 1, heights = c(1, 14, 1))