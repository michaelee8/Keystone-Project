#Keystone Coding Project
## Michael Lee
### Aggregating Hockey Data


# Work Space -------------------------------------------------------------------
library(dplyr)

AdvancedStats2019 <- read.csv("~/Michael's Stuff/Michael's Stuff/School/Keystone/Advanced Stats.csv")
NHL_2019_RegStats <- read.csv("~/Michael's Stuff/Michael's Stuff/School/Keystone/Regular Stats.csv")
TeamStats2019 <- read.csv("~/Michael's Stuff/Michael's Stuff/School/Keystone/TeamStats2019.csv")

McDavidWork <- AdvancedStats2019 %>% 
   filter(Player=="Connor McDavid")

MillerWork <- AdvancedStats2019 %>% 
   filter(Player=="J.T. Miller")

NiskanenWork <- AdvancedStats2019 %>% 
   filter(Player=="Matt Niskanen")

AvgPlayers <- NHL_2019_RegStats %>%
   filter(Total.Points %in% 20:24)

AvgPlayerAdvStats <- AdvancedStats2019 %>%
   filter(Player=AvgPlayers)

mean(NHL_2019_RegStats$Total.Points, na.rm = TRUE)
mean(AdvancedStats2019$CF.)
mean(AdvancedStats2019$FF.)
mean(AdvancedStats2019$PDO)
mean(AdvancedStats2019$xGF.)

PtsHist <- hist(NHL_2019_RegStats$Total.Points,
     breaks = seq(0, 130, by=1), 
     xaxt = "n",
     ylim = range(pretty(c(0, 200))),
     xlab = "Points",
     ylab = "Number of Players",
     main = "Distribution of Points in the 2019 NHL Season")
axis(1, at=seq(0, 130, by=10))



CFHist <- hist(AdvancedStats2019$CF.,
     breaks = seq(0, 100, by =1),
     main = "CF% Distribution",
     xlab = "CF%",
     xaxt = "n",
     ylab = "Number of Players",
     ylim = range(pretty(c(0, 120))))
axis(1, at=seq(0, 100, by =10))

PDOHist <- hist(AdvancedStats2019$PDO,
     breaks = seq(0, 150, by =1),
     main = "PDO Distribution",
     xlab = "PDO Score",
     ylim = range(pretty(c(0,150))))

xGFhist <- hist(AdvancedStats2019$xGF.,
                breaks = seq(0, 100, by = 1),
                main = "xGF% Distribution",
                xlab = "xGF%",
                xaxt = "n",
                ylab = "Number of Players",
                ylim = range(pretty(c(0,100))))
axis(1, at=seq(0,100, by =10))

FFHist <- hist(AdvancedStats2019$FF.,
               breaks = seq(0,100,by=1),
               main = "FF% Distribution",
               xlab = "FF%",
               xaxt = "n",
               ylim = range(pretty(c(0,120))))
axis(1, at=seq(0,100,by=10))

EDMStats <- TeamStats2019 %>%
   filter(Team=="Edmonton Oilers")

TBLStats <- TeamStats2019 %>%
   filter(Team=="Tampa Bay Lightning")

KucherovWork <- AdvancedStats2019 %>% 
   filter(Player=="Nikita Kucherov")

StamkosWork <- AdvancedStats2019 %>%
   filter(Player=="Steven Stamkos")

HintzWork <- AdvancedStats2019 %>%
   filter(Player=="Roope Hintz")

mean(TeamStats2019$CF.)
mean(TeamStats2019$FF.)
mean(TeamStats2019$xGF.)


sd(AdvancedStats2019$CF.)
sd(AdvancedStats2019$FF.)
sd(AdvancedStats2019$xGF.)
# Connor McDavid ----------------------------------------------------------
McDavid <- which(AdvancedStats2019$Player == "Connor McDavid")
McDavidStats <- which(NHL_2019_RegStats$Player == "Connor McDavid")



McDavidPts <- print(NHL_2019_RegStats$Total.Points[McDavidStats])
McDavidCF <- print(AdvancedStats2019$CF.[McDavid])
McDavidFF <- print(AdvancedStats2019$FF.[McDavid])
McDavidxGF <- print(AdvancedStats2019$xGF.[McDavid])
McDavidPDO <- print(AdvancedStats2019$PDO[McDavid])

# JT Miller ---------------------------------------------------------------
Miller <- which(AdvancedStats2019$Player == "J.T. Miller")
MillerStats <- which(NHL_2019_RegStats$Player == "J.T. Miller")

MillerPts <- print(NHL_2019_RegStats$Total.Points[MillerStats])
MillerCF <- print(AdvancedStats2019$CF.[Miller])
MillerFF <- print(AdvancedStats2019$FF.[Miller])
MillerxGF <- print(AdvancedStats2019$xGF.[Miller])
MillerPDO <- print(AdvancedStats2019$PDO[Miller])


# Matt Niskanen ------------------------------------------------------------
Nisk <- which(AdvancedStats2019$Player == "Matt Niskanen")
NiskStats <- which(NHL_2019_RegStats$Player == "Matt Niskanen")

NiskPts <- print(NHL_2019_RegStats$Total.Points[NiskStats])
NiskCF <- print(AdvancedStats2019$CF.[Nisk])
NiskFF <- print(AdvancedStats2019$FF.[Nisk])
NiskxGF <- print(AdvancedStats2019$xGF.[Nisk])
NiskPDO <- print(AdvancedStats2019$PDO[Nisk])
NiskG <- print(NHL_2019_RegStats$Giveaways[NiskStats])
NiskT <- print(NHL_2019_RegStats$Takeaways[NiskStats])
NiskB <- print(NHL_2019_RegStats$Shots.Blocked[NiskStats])

# Total Shots v.s. Shooting % --------------------------------------------------

plot(TeamStats2019$SH., TeamStats2019$Point..,
     main = "Shooting Percentage and Win Percentage",
     xlab = "Shooting Percentage",
     ylab = "Win Percentage",
     col = "blue",
     pch = 16)
abline(lm(TeamStats2019$Point..~TeamStats2019$SH.))

plot(TeamStats2019$SF, TeamStats2019$Point..,
     main = "Shots Taken and Win Percentage",
     xlab = "Shots Taken",
     ylab = "Win Percentage",
     col = "blue",
     pch = 16)
abline(lm(TeamStats2019$Point..~TeamStats2019$SF))


cor.test(TeamStats2019$SF, TeamStats2019$Point.., 
         method = "pearson")
cor.test(TeamStats2019$SH., TeamStats2019$Point.., 
         method = "pearson")
