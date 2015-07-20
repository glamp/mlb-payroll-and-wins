library(plyr)
library(ggplot2)
library(ggthemes)

df <- read.csv("./mlb-standings-and-payroll.csv", stringsAsFactors=FALSE)
df <- df[,c("tm", "year", "w", "wins_losses", "est_payroll")]
df <- df[df$year >= 1985,]
head(df)

# Lookup metadata for each team
team.lookups <- read.csv("./team-lookups.csv")
df <- merge(df, team.lookups, by.x="tm", by.y="historic_team", stringsAsFactors=FALSE)
head(df)

# Lookup colors
team.colors <- read.csv("./team-colors.csv", stringsAsFactors=FALSE)
df <- merge(df, team.colors, by.x="modern_team", by.y="tm")
head(df)

yearly_payroll <- ddply(df, .(year), function(x) {
  data.frame(
    mean_payroll=mean(x$est_payroll, na.rm=TRUE),
    std_payroll=sd(x$est_payroll, na.rm=TRUE)
  )
})

df <- merge(df, yearly_payroll, by="year")

# calculate number of standard deviations from mean
df$standardized_payroll <- (df$est_payroll - df$mean_payroll) / df$std_payroll


ggplot(subset(df, year==2014), aes(x=standardized_payroll, y=wins_losses)) +
  geom_point() +
  scale_x_continuous(name="Standardized Salary\n(# of standard deviations from yearly mean)",
                     breaks=c(-2, 0, 2), limit=c(-2.5, 2.5), labels=c("-2", "0", "+2")) +
  scale_y_continuous(name="Win/Loss %", breaks=seq(0.3, 0.7, 0.1), limit=c(0.2, 0.8)) +
  annotate("text", x = -2, y = 0.45, label = "Houston Astros") +
  annotate("text", x = 2, y = 0.6, label = "NY Yankees") +
  ggtitle("2014 Win % vs. Standardized Payroll")

p <- ggplot(subset(df, tm=="NYM"), aes(x=standardized_payroll, y=wins_losses, color=team_color)) + 
  geom_point(alpha=0.75, size=3) + 
  #   stat_smooth(data=within(df, modern_team <- NULL), color="grey", size=.5,
  #               method="lm", formula = y ~ poly(x, 2), se=FALSE) +
  stat_smooth(size=1.5, method="lm", formula = y ~ poly(x, 2), se=FALSE) +
  scale_color_identity() +
  scale_x_continuous(name="Standardized Salary\n(# of standard deviations from yearly mean)",
                     breaks=c(-2, 0, 2), limit=c(-2.5, 2.5), labels=c("-2", "0", "+2")) +
  scale_y_continuous(name="Win/Loss %", breaks=seq(0.3, 0.7, 0.1), limit=c(0.2, 0.8)) +
  #   theme_fivethirtyeight() +
  ggtitle("New York Mets\nWin Percentage vs. stgandard deviations from average salary")
p
ggsave(filename="nym.png", plot=p, width=8.67, height=6.17)


p <- p + stat_smooth(data=df, color="grey", size=.5, method="lm", formula = y ~ poly(x, 2), se=FALSE)
p
ggsave(filename="nym-with-league.png", plot=p, width=8.67, height=6.17)

divisions <- c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")

for (div in divisions) {
  df.division <- subset(df, division==div)
  div.title <- sub("AL", "American League", div)
  div.title <- sub("NL", "National League", div.title)
  p <- ggplot(df.division, aes(x=standardized_payroll, y=wins_losses, color=team_color)) + 
    geom_point(alpha=0.75, size=3) + 
    stat_smooth(data=within(df, modern_team <- NULL), color="grey", size=.5,
                method="lm", formula = y ~ poly(x, 2), se=FALSE) +
    stat_smooth(size=1.5, method="lm", formula = y ~ poly(x, 2), se=FALSE) +
    scale_color_identity() +
    scale_x_continuous(name="Standardized Salary\n(# of standard deviations from yearly mean)",
                       breaks=c(-2, 0, 2), limit=c(-2.5, 2.5), labels=c("-2", "0", "+2")) +
    scale_y_continuous(name="Win/Loss %", breaks=seq(0.3, 0.7, 0.1), limit=c(0.25, 0.75)) +
    facet_wrap(~modern_team, ncol=5, scales="free_x") +
    theme_fivethirtyeight() +
    ggtitle(div.title)
  ggsave(filename=paste0(div, ".png"), plot=p, width=13.65, height=3.59)
}
