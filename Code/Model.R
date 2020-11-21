library(knitr)
library(ggplot2)
library(skimr)
library(xtable)
library(rms) #for VIF
library(MASS)
library(pander)
library(arm)
library(pROC)
library(e1071)
library(caret)
require(gridExtra)

games <- read.csv("final data.csv")
games$win <- ifelse(games$winner == "Blue",1,0)
games$win_fac <- factor(games$win)
games$TopWinningTeam_num <- ifelse(games$TopWinningTeam == "Blue",1,0)
games$TopWinningTeam_fac <- factor(games$TopWinningTeam_num)
games$JungWinningTeam_num <- ifelse(games$TopWinningTeam == "Blue",1,0)
games$JungWinningTeam_fac <- factor(games$JungWinningTeam_num)
games$MidWinningTeam_num <- ifelse(games$MidWinningTeam == "Blue",1,0)
games$MidWinningTeam_fac <- factor(games$MidWinningTeam_num)
games$ADCWinningTeam_num <- ifelse(games$ADCWinningTeam == "Blue",1,0)
games$ADCWinningTeam_fac <- factor(games$ADCWinningTeam_num)
games$SupWinningTeam_num <- ifelse(games$SupWinningTeam == "Blue",1,0)
games$SupWinningTeam_fac <- factor(games$SupWinningTeam_num)
games$Carry <- factor(games$Carry)
games$TeamOfCarry_fac <- ifelse(games$TeamOfCarry == "Blue",1,0)
games$TeamOfCarry_fac <- factor(games$TeamOfCarry_fac)
games$Season <- factor(games$Season, levels = c("2014","2015","2016","2017","2018"))


# EDA
table(games$win_fac)

# Numeric predictor
#Top
ggplot(games,aes(x=win_fac, y=TopAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Top gold difference",
       x="win",y="gold") +
  theme_classic() + theme(legend.position="none")

#Jungle
ggplot(games,aes(x=win_fac, y=JungleAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Jungle gold difference",
       x="win",y="gold") +
  theme_classic() + theme(legend.position="none")

#Mid
ggplot(games,aes(x=win_fac, y=MidAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Mid gold difference",
       x="win",y="gold") +
  theme_classic() + theme(legend.position="none")

#ADC
ggplot(games,aes(x=win_fac, y=ADCAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="ADC gold difference",
       x="win",y="gold") +
  theme_classic() + theme(legend.position="none")

#Support
ggplot(games,aes(x=win_fac, y=SupportAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Support gold difference",
       x="win",y="gold") +
  theme_classic() + theme(legend.position="none")

#Gamelength
ggplot(games,aes(x=win_fac, y=GameLength, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Game Length",
       x="win",y="Length") +
  theme_classic() + theme(legend.position="none")

#KillDiff
ggplot(games,aes(x=win_fac, y=KillDiff, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Kill Difference",
       x="win",y="Kills") +
  theme_classic() + theme(legend.position="none")

#TeamOfCarry
table(games[,c("win_fac","TeamOfCarry_fac")])
table(games[,c("win_fac","TeamOfCarry_fac")])/sum(table(games[,c("win_fac","TeamOfCarry_fac")]))
apply(table(games[,c("win_fac","TeamOfCarry_fac")])/sum(table(games[,c("win_fac","TeamOfCarry_fac")])),
      2,function(x) x/sum(x))
tapply(games$win_fac, games$TeamOfCarry_fac, function(x) table(x)/sum(table(x)))
chisq.test(table(games[,c("win_fac","TeamOfCarry_fac")]))

# Season
table(games[,c("win_fac","Season")])
table(games[,c("win_fac","Season")])/sum(table(games[,c("win_fac","Season")]))
apply(table(games[,c("win_fac","Season")])/sum(table(games[,c("win_fac","Season")])),
      2,function(x) x/sum(x))
tapply(games$win_fac, games$Season, function(x) table(x)/sum(table(x)))
chisq.test(table(games[,c("win_fac","Season")]))

# Carry
table(games[,c("win_fac","Carry")])
table(games[,c("win_fac","Carry")])/sum(table(games[,c("win_fac","Carry")]))
apply(table(games[,c("win_fac","Carry")])/sum(table(games[,c("win_fac","Carry")])),
      2,function(x) x/sum(x))
tapply(games$win_fac, games$Carry, function(x) table(x)/sum(table(x)))
chisq.test(table(games[,c("win_fac","Carry")]))

# Win vs Top by Season
ggplot(games,aes(x=win_fac, y=TopAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Top gold difference, by Season",
       x="Win",y="Top gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Season)

# Win vs Jungle by Season
ggplot(games,aes(x=win_fac, y=JungleAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Jungle gold difference, by Season",
       x="Win",y="Jungle gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Season)

# Win vs Mid by Season
ggplot(games,aes(x=win_fac, y=MidAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Mid gold difference, by Season",
       x="Win",y="Mid gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Season)

# Win vs ADC by Season
ggplot(games,aes(x=win_fac, y=ADCAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs ADC gold difference, by Season",
       x="Win",y="ADC gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Season)

# Win vs Support by Season
ggplot(games,aes(x=win_fac, y=SupportAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Support gold difference, by Season",
       x="Win",y="Support gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Season)

# Win vs Top by TeamOfCarry
ggplot(games,aes(x=win_fac, y=TopAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Top gold difference, by TeamOfCarry",
       x="Win",y="Top gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ TeamOfCarry_fac)

# Win vs Mid by TeamOfCarry
ggplot(games,aes(x=win_fac, y=MidAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Mid gold difference, by TeamOfCarry",
       x="Win",y="Mid gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ TeamOfCarry_fac)

# Win vs Jungle by TeamOfCarry
ggplot(games,aes(x=win_fac, y=JungleAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Jungle gold difference, by TeamOfCarry",
       x="Win",y="Jungle gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ TeamOfCarry_fac)

# Win vs ADC by TeamOfCarry
ggplot(games,aes(x=win_fac, y=ADCAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs ADC gold difference, by TeamOfCarry",
       x="Win",y="ADC gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ TeamOfCarry_fac)

# Win vs Support by TeamOfCarry
ggplot(games,aes(x=win_fac, y=SupportAvg, fill=win_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Win vs Support gold difference, by TeamOfCarry",
       x="Win",y="Support gold difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ TeamOfCarry_fac)

# # Win vs Top by Carry
# ggplot(games,aes(x=win_fac, y=TopAvg, fill=win_fac)) +
#   geom_boxplot() +
#   scale_fill_brewer(palette="Reds") +
#   labs(title="Win vs Top gold difference, by Carry",
#        x="Win",y="Top gold difference") + 
#   theme_classic() + theme(legend.position="none") +
#   facet_wrap( ~ Carry)
# 
# # Win vs Mid by Carry
# ggplot(games,aes(x=win_fac, y=MidAvg, fill=win_fac)) +
#   geom_boxplot() +
#   scale_fill_brewer(palette="Reds") +
#   labs(title="Win vs Mid gold difference, by Carry",
#        x="Win",y="Mid gold difference") + 
#   theme_classic() + theme(legend.position="none") +
#   facet_wrap( ~ Carry)
# 
# # Win vs Jungle by Carry
# ggplot(games,aes(x=win_fac, y=JungleAvg, fill=win_fac)) +
#   geom_boxplot() +
#   scale_fill_brewer(palette="Reds") +
#   labs(title="Win vs Jungle gold difference, by Carry",
#        x="Win",y="Jungle gold difference") + 
#   theme_classic() + theme(legend.position="none") +
#   facet_wrap( ~ Carry)
# 
# # Win vs ADC by Carry
# ggplot(games,aes(x=win_fac, y=ADCAvg, fill=win_fac)) +
#   geom_boxplot() +
#   scale_fill_brewer(palette="Reds") +
#   labs(title="Win vs ADC gold difference, by Carry",
#        x="Win",y="ADC gold difference") + 
#   theme_classic() + theme(legend.position="none") +
#   facet_wrap( ~ Carry)
# 
# # Win vs Support by Carry
# ggplot(games,aes(x=win_fac, y=SupportAvg, fill=win_fac)) +
#   geom_boxplot() +
#   scale_fill_brewer(palette="Reds") +
#   labs(title="Win vs Support gold difference, by Carry",
#        x="Win",y="Support gold difference") + 
#   theme_classic() + theme(legend.position="none") +
#   facet_wrap( ~ Carry)


# Binnedplots

# par(mfcol=c(5,1))
# 
# #first plot for carry = ADC
# binnedplot(games$TopAvg[games$Carry=="ADC"], y=games$win[games$Carry=="ADC"], 
#            xlab = "ADC Gold Diff", ylab = "Win or Lose", main = "Binned ADC Gold Diff and win (carry=ADC)") 
# 
# #next the plot for carry = Top
# binnedplot(games$TopAvg[games$Carry=="Top"], y=games$win[games$Carry=="Top"], 
#            xlab = "Top Gold Diff", ylab = "Win or Lose", main = "Binned Top Gold Diff and win (carry=ADC)") 
# 
# #next the plot for carry = Mid
# binnedplot(games$TopAvg[games$Carry=="Mid"], y=games$win[games$Carry=="Mid"], 
#            xlab = "Mid Gold Diff", ylab = "Win or Lose", main = "Binned Mid Gold Diff and win (carry=ADC)") 
# 
# #next the plot for carry = Jungle
# binnedplot(games$TopAvg[games$Carry=="Jungle"], y=games$win[games$Carry=="Jungle"], 
#            xlab = "Jungle Gold Diff", ylab = "Win or Lose", main = "Binned Jungle Gold Diff and win (carry=ADC)") 
# 
# #next the plot for carry = Support
# binnedplot(games$TopAvg[games$Carry=="Support"], y=games$win[games$Carry=="Support"], 
#            xlab = "Support Gold Diff", ylab = "Win or Lose", main = "Binned Support Gold Diff and win (carry=ADC)") 


# Model Fitting
## Priliminary model
gamesreg <- glm(win ~ TopAvg + JungleAvg + MidAvg + ADCAvg + SupportAvg + GameLength + KillDiff + Season
                 + TeamOfCarry_fac, data = games, family = binomial)
summary(gamesreg)

rawresid_gamesreg <- residuals(gamesreg,"resp")
binnedplot(x=fitted(gamesreg),y=rawresid_gamesreg,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(games$TopAvg,rawresid_gamesreg,xlab="Top",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(games$MidAvg,rawresid_gamesreg,xlab="Mid",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(games$JungleAvg,rawresid_gamesreg,xlab="Jungle",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(games$ADCAvg,rawresid_gamesreg,xlab="ADC",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(games$SupportAvg,rawresid_gamesreg,xlab="Support",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(gamesreg) >= 0.5, "1","0")),
                            games$win_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

roc(games$win_fac,fitted(gamesreg),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

# ## Convert RoleWinningTeam
# gamesreg1 <- glm(win ~ TopWinningTeam_fac + JungWinningTeam_fac + MidWinningTeam_fac + ADCWinningTeam_fac
#                   + SupWinningTeam_fac + GameLength + KillDiff + Season
#                    + TeamOfCarry_fac, data = games, family = binomial)
# summary(gamesreg1)

## Interactions
gamesreg1 <- glm(win ~ TopAvg + JungleAvg + MidAvg + ADCAvg + SupportAvg + GameLength + KillDiff + Season
                 + TeamOfCarry_fac + SupportAvg:TeamOfCarry_fac, data = games, family = binomial)
summary(gamesreg1)

anova(gamesreg, gamesreg1, test='Chisq')

## Stepwise
model0 <- glm(win ~ 1, data = games, family = binomial)
model1 <- glm(win ~ TopAvg + JungleAvg + MidAvg + ADCAvg + SupportAvg + GameLength + KillDiff + Season
                   + TeamOfCarry_fac + TeamOfCarry_fac:Season + TopAvg:JungleAvg + MidAvg:JungleAvg 
                   + ADCAvg:JungleAvg + SupportAvg:JungleAvg + ADCAvg:SupportAvg + SupportAvg:TeamOfCarry_fac,
                  data = games, family = binomial)
model_step <- step(model0, scope=formula(model1), direction="both", trace=0)
summary(model_step)

model_step1 <- glm(formula = win ~ KillDiff + TopAvg + GameLength + ADCAvg + 
                     JungleAvg + MidAvg + ADCAvg:JungleAvg + TopAvg:JungleAvg + SupportAvg:TeamOfCarry_fac, 
                   family = binomial, data = games)
anova(model_step1, model_step, test='Chisq')

#Model Diagnostics
rawresid1 <- residuals(model_step,"resp")
binnedplot(x=fitted(model_step),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

vif(model_step)

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_step) >= 0.5, "1","0")),
                            as.factor(games$win),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

roc(games$win,fitted(model_step),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")


# ######### PART II #########
# # Carry
# table(games[,c("win_fac","Carry")])
# table(games[,c("win_fac","Carry")])/sum(table(games[,c("win_fac","Carry")]))
# apply(table(games[,c("win_fac","Carry")])/sum(table(games[,c("win_fac","Carry")])),
#       2,function(x) x/sum(x)) 
# 
# # TeamOfCarry_fac
# table(games$TeamOfCarry_fac)
# table(games$win_fac)
# 
# #Top
# table(games[,c("win_fac","TopWinningTeam_fac")])
# 
# # Model Fitting
# model0_fac <- glm(win ~ 1, data = games, family = binomial)
# model1_fac <- glm(win ~ TopWinningTeam_fac + JungWinningTeam_fac + MidWinningTeam_fac + ADCWinningTeam_fac
#                   + SupWinningTeam_fac + GameLength + KillDiff + Season
#               + Carry + TeamOfCarry_fac + Carry:Season + Carry:TeamOfCarry_fac,
#               data = games, family = binomial)
# model_step_fac <- step(model0_fac, scope=formula(model1_fac), direction="both", trace=0)
# summary(model_step_fac)
# 
# 
# # Model Validation
# Conf_mat1 <- confusionMatrix(as.factor(ifelse(fitted(model_step_fac) >= 0.5, "1","0")),
#                             as.factor(games$win),positive = "1")
# Conf_mat1$table
# Conf_mat1$overall["Accuracy"];
# Conf_mat1$byClass[c("Sensitivity","Specificity")]
# 
# roc(games$win,fitted(model_step_fac),plot=T,print.thres="best",legacy.axes=T,
#     print.auc =T,col="red3")

