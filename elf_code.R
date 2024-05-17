#SMargalef
# What it takes to win in the ELF

# Data from SportsMetrics

# Load the data
elf = read.csv("elf_dataset.csv")

# calculate win percentage
elf$w.perc = pmin(elf$w / elf$g, 1)

# calculate points and touchdowns difference
elf$pts.diff = elf$o.pts - elf$d.pts
elf$td.diff = elf$o.tot.td - elf$d.tot.td

# create some plots
library(ggplot2)

# plot win percentage vs. points difference
ggplot(elf, aes(x = pts.diff, y = w.perc)) +
  geom_point() +
  labs(x = "Points Difference", y = "Win Percentage") +
  theme_bw()

# plot win percentage vs. touchdowns difference
ggplot(elf, aes(x = td.diff, y = w.perc)) +
  geom_point() +
  labs(x = "Touchdowns Difference", y = "Win Percentage") +
  theme_bw()

# correlation between win percentage and points difference
cor(elf$w.perc, elf$pts.diff)
# 0.9560689

# correlation between win percentage and touchdowns difference
cor(elf$w.perc, elf$td.diff)
# 0.9453056

# add playoffs to the plots
ggplot(elf, aes(x = pts.diff, y = w.perc, color = playoffs)) +
  geom_point() +
  labs(x = "Points Difference", y = "Win Percentage") +
  theme_bw() + 
  guides(color = FALSE)

ggplot(elf, aes(x = td.diff, y = w.perc, color = playoffs)) +
  geom_point() +
  labs(x = "Touchdowns Difference", y = "Win Percentage") +
  theme_bw() + 
  guides(color = FALSE)

# linear regression model with win percentage and points difference
library(stargazer)

model_pts = lm(w.perc ~ pts.diff, data = elf)
summary(model_pts)

stargazer(model_pts, type = "text",
          title = "Regression Win Percentage vs. Points Difference",
          dep.var.labels = "Win Percentage",
          covariate.labels = c("Points Difference", "Intercept"),
          out = "modelptstable.htm")

# linear regression model with win percentage and touchdowns difference
model_td = lm(w.perc ~ td.diff, data = elf)
summary(model_td)

stargazer(model_td, type = "text",
          title = "Regression Win Percentage vs. Touchdowns Difference",
          dep.var.labels = "Win Percentage",
          covariate.labels = c("Touchdowns Difference", "Intercept"),
          out = "modeltdtable.htm")

# linear regression model of offense points
model_off_pts = lm(o.pts ~ o.rush.yds + o.pass.yds + o.rush.td + o.pass.td , data = elf)
summary(model_off_pts)

stargazer(model_off_pts, type = "text",
          title = "Regression Offense Points",
          dep.var.labels = "Offense Points",
          covariate.labels = c("Rushing Yards", "Passing Yards", "Rushing Touchdowns", "Passing Touchdowns", "Intercept"),
          out = "modeloffptstable.htm")

# linear regression model of defense points
model_def_pts = lm(d.pts ~ d.rush.yds + d.pass.yds + d.rush.td + d.pass.td , data = elf)
summary(model_def_pts)

stargazer(model_def_pts, type = "text",
          title = "Regression Defense Points",
          dep.var.labels = "Defense Points",
          covariate.labels = c("Rushing Yards", "Passing Yards", "Rushing Touchdowns", "Passing Touchdowns", "Intercept"),
          out = "modeldefptstable.htm")
