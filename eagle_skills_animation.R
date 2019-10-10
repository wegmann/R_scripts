
library(RCurl)

df <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vS0wAfZt28-tqhdxEX4EartljewLmOKpa0XCvDFgqKqibaNYuHFABsiN3NGgOIWj06tfyynEdg3q6bO/pub?gid=1579905509&single=true&output=csv")))
summary(df)

df1 <- data.frame(df[,c(2:6,10,14)], semester=1, courses=11)
df2 <- data.frame(df[,c(2:5,7,11,15)], semester=2, courses=15)
df3 <- data.frame(df[,c(2:5,8,12,16)], semester=3, courses=2)
df4 <- data.frame(df[,c(2:5,9,13,17)], semester=4, courses=3)



df.names <- c("eyecol","haircol","glasses","sex","eoExp","progExp","praesExp","semester", "courses")

names(df1) <- df.names
names(df2) <- df.names
names(df3) <- df.names
names(df4) <- df.names


df <- rbind(df1,df2,df3,df4)

summary(df)

library(ggplot2)
library(gganimate)

# scatterplot EO exp vs. coding exp
p <- ggplot(data=df, aes(y=eoExp, x=progExp, color=eyecol, size=sex))+
  geom_point(alpha=.8)

p

p + transition_time(semester) +
  ease_aes('linear')+
#  labs(title = "semester: {closest_state}")+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  enter_fade() +
  exit_fade()
  # enter_grow()+
  # exit_disappear()


anim_save("eagle_EO_vs_prog_experiences.gif")



# barplot by semester

p <- ggplot(df, aes(semester, eoExp, fill=courses)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
p

p + transition_states(semester, transition_length = 1, state_length = 1, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()

anim_save("eagle_barplot_EO_vs_prog_experiences.gif")

