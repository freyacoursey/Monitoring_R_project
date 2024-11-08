#### Updated Blue Tit Script ####

#rm(list=ls()) # don't have this at the start of code that you share! It will clear someone else's workspace... and they won't be happy!!

## jtools seems to work now

library(dplyr)
library(ggplot2)
library(ggfortify)
library(ggiraphExtra)
library(lme4)
library(ggthemes)
library(effects)
library(tidyr)
library(broom)
library(broom.mixed)
library(TMB)
library(MuMIn)
library(Hmisc)
library(optiRum)
library(crayon)
library(generics)
library(magrittr)
library(pander)
library(pkgconfig)
library(rlang)
library(tibble)
library(car)
library(MASS)
library(jtools)

# Attaching data and assigning factors #

# This data includes all breeding attempts in Milde from 2017-2023

#icloud import
Milde_data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Documents/UiB Passerines/Working statistics data/Main_data_milde.csv", header=TRUE)
Nestbox_data <- Milde_data

# Subsetting data by species
BT.data<-subset(Nestbox_data, Species=="Blåmeis")
GT.data<-subset(Nestbox_data, Species=="Kjøttmeis")

# Covariate to factor
BT.data$Year<-factor(BT.data$Year)
GT.data$Year<-factor(GT.data$Year)

BT.data$Nestbox<-factor(BT.data$Nestbox)
GT.data$Nestbox<-factor(GT.data$Nestbox)

BT.data$Species<-factor(BT.data$Species)
GT.data$Species<-factor(GT.data$Species)

# Factors to covariate
is.factor(BT.data$LayDate)
BT.data$LayDate <- as.numeric(as.character(BT.data$LayDate))

is.factor(GT.data$LayDate)
GT.data$LayDate <- as.numeric(as.character(GT.data$LayDate))

is.factor(BT.data$BroodSize)
BT.data$BroodSize <- as.numeric(as.character(BT.data$BroodSize))

is.factor(GT.data$BroodSize)
GT.data$BroodSize <- as.numeric(as.character(GT.data$BroodSize))

is.factor(BT.data$Fledged)
BT.data$Fledged <- as.numeric(as.character(BT.data$Fledged))

is.factor(GT.data$Fledged)
GT.data$Fledged <- as.numeric(as.character(GT.data$Fledged))

is.factor(BT.data$ClutchSize)
BT.data$ClutchSize <- as.numeric(as.character(BT.data$ClutchSize))

is.factor(GT.data$ClutchSize)
GT.data$ClutchSize <- as.numeric(as.character(GT.data$ClutchSize))

is.factor(BT.data$Hatch_gap)
BT.data$Hatch_gap <- as.numeric(as.character(BT.data$Hatch_gap))

is.factor(GT.data$Hatch_gap)
GT.data$Hatch_gap <- as.numeric(as.character(GT.data$Hatch_gap))

#### Collinearity ####
occupied.data<-subset(Nestbox_data, Occupied=="yes")
occupied.data$LayDate <- as.numeric(as.character(occupied.data$LayDate))
occupied.data <- filter(occupied.data, LayDate != "NA")

#### Run up to here before running great tit models ####

##### Descriptive statistics #####

#Occupancy rate
group_counts <- table(Nestbox_data$Occupied)
total_count <- sum(group_counts)
percentage_by_group <- (group_counts / total_count) * 100
# Combine group names and percentages into a data frame
occupancy_rate_table <- data.frame(
  Number = group_counts,
  Occupied = names(percentage_by_group),
  Percentage = percentage_by_group)
print(occupancy_rate_table)

#Species percentage
group_counts <- table(occupied.data$Species)
total_count <- sum(group_counts)
percentage_by_group <- (group_counts / total_count) * 100
# Combine group names and percentages into a data frame
species_number_table <- data.frame(
  Number = group_counts,
  Species = names(percentage_by_group),
  Percentage = percentage_by_group)
print(species_number_table)

#Occupancy rate
group_counts <- table(Nestbox_data$Occupied)
total_count <- sum(group_counts)
percentage_by_group <- (group_counts / total_count) * 100
# Combine group names and percentages into a data frame
result_base_R <- data.frame(
  Occupied = names(percentage_by_group),
  Percentage = percentage_by_group)
# 83%

#Mean clutch size of blue tits
mean(BT.data$ClutchSize)

#Mean clutch size of great tits
mean(GT.data$ClutchSize)

#Mean laydate of blue tits
mean(BT.data$LayDate)

#Mean laydate of great tits
GT.data.na.omit<-na.omit(GT.data)
mean(GT.data.na.omit$LayDate)

#Mean broodsize of blue tits
BT.data2<-subset(BT.data, BroodSize>=1)
mean(BT.data2$BroodSize)

#Mean broodsize of great tits
GT.data2<-subset(GT.data, BroodSize>=1)
mean(GT.data2$BroodSize)

#Mean broodsize of blue tits
BT.data.na.omit<-na.omit(BT.data2)
mean(BT.data.na.omit$Fledged)

#Mean broodsize of great tits
mean(GT.data2$Fledged)

# Graphs and plots


# Boxplot of species and fledglings
tit.data<-subset(occupied.data, Species=="Kjøttmeis"+"Blåmeis")
ggplot(occupied.data, aes(x=Species, y=Fledged)) +
  geom_boxplot(aes(fill = Species), show.legend = FALSE)+
  labs(x="Species", y="Number of Fledglings") + theme_minimal()

# Boxplot year and fledglings of blue tit
ggplot(BT.data2, aes(x=Year, y=Fledged)) +
  geom_boxplot(aes(fill = Year), show.legend = FALSE)+
  labs(x="Year", y="Number of Fledglings") + theme_minimal()

# Boxplot year and fledglings of blue tit
ggplot(GT.data2, aes(x=Year, y=Fledged)) +
  geom_boxplot(aes(fill = Year), show.legend = FALSE)+
  labs(x="Year", y="Number of Fledglings") + theme_minimal()

##########################################################

##### NESTBOX OCCUPANCY MODEL #####

# See nestbox occupancy script for this model.

##### CLUTCHSIZE MODEL #####

# Model predicting the number of eggs in a nest
# Includes nests which are occupied and where there is at least 1 egg

# Step by step model choice for Clutch Size

# Fit full model
BTclutch_fullmodel <- glm(ClutchSize ~ Year + LayDate + I(LayDate^2) + understory_percentage +
                         deciduous_largescale + deciduous_25m + deciduous_7m +
                         LayDate*Year + deciduous_largescale*Year +
                         deciduous_25m*Year + deciduous_7m*Year,
                       data=BT.data, family=poisson)
summary(BTclutch_fullmodel)

# Backward regression model
BTclutch_stepmodel <- stepAIC(BTclutch_fullmodel, direction = "backward",
                      trace = FALSE)
summary(BTclutch_stepmodel)

# Forward regression model
# Need to define the null model
BTclutch_nullmodel <- glm(ClutchSize ~ 1, data=BT.data, family=poisson)

# Run forward regression

BTclutch_stepmodel2 <- stepAIC(BTclutch_nullmodel, # start with a model containing no variables
                                direction = 'forward', # run forward selection
                                scope = list(upper = BTclutch_fullmodel, # the maximum to consider is a model with all variables
                                             lower = BTclutch_nullmodel), # the minimum to consider is a model with no variables
                                trace = 0) # do not show the step-by-step process of model selection

summary(BTclutch_stepmodel2)

# Final clutchsize model
BTclutch_model <- glm(ClutchSize ~ I(LayDate^2), data=BT.data, family=poisson)

## Model validation ##
par(mfrow=c(2,2))
plot(BTclutch_stepmodel)

# Everything looks fine here.
par(mfrow=c(1,1))
# Collinearity - only 1 covariate, so no problems.
# Check for over-dispersion
BTclutch_stepmodel$deviance/BTclutch_stepmodel$df.residual
# Over-dispersed

#calculate McFadden's R-squared for model
with(summary(BTclutch_stepmodel), 1 - deviance/null.deviance)
#0.079

#' BIOLOGY
#'
#' No effect of forest type.
#' - Blue tits are not reliant on the quality of their territory around the nestbox
#'   to get into egg laying condition.
#'
#' Negative quadratic effect of laydate on clutch size.
#' - Later laydate causes smaller clutch size.
#' - Earlier laydate cause smaller clutch size.
#' - Late layers may need to push forward hatching so that fledglings have more time
#'   to mature before winter. Hatch gap is negatively effected by laydate in the hatchgap
#'   model below. Clutch size is compromised by late laying.
#' - Early layers may not have enough food to produce a decent clutch.

# Scatterplot of clutch size against lay date
ggplot(BT.data, aes(x=LayDate, y=ClutchSize)) + geom_point(position = "jitter") +
  labs(x="Lay Date", y="Clutch Size", title="Clutch Size in Blue Tits") + theme_minimal() +
  stat_smooth(color="dark blue", method='lm', formula = y ~ I(x^2)) +
  theme(plot.title = element_text(hjust=0.5, size=14))

# This looks better....
ggplot(BT.data, aes(x=LayDate, y=ClutchSize)) + geom_point(position = "jitter") +
  labs(x="Lay Date", y="Clutch Size", title="Clutch Size in Blue Tits") + theme_minimal() +
  stat_smooth(color="dark blue", method='lm', formula = y ~ poly(x, 3, raw = TRUE)) +
  theme(plot.title = element_text(hjust=0.5, size=14)) + ylim(1,11)

##### Effect plot with jtools ####
effect_plot(BTclutch_stepmodel, pred = LayDate, interval = TRUE,
            partial.residuals = TRUE, jitter = 0.5)

##### HATCHING SUCCESS MODEL #####

# Model predicting the probably of at least one egg hatching
# in nests where there was at least one egg laid.

# Use a logistic reggression model (binomial - y/n)

# Subset of data where at least one egg laid:
BT.hatchsuccess<-subset(BT.data, ClutchSize>=1)
# But only three nests with eggs that didn't hatch.... very small sample size.
# Model won't be any good.

##### BROODSIZE MODEL #####

# Model preditcing the number of chicks in a brood
# Includes nests where at least one egg hatched

# Subset
BT.data2<-subset(BT.data, BroodSize>=1)

# Fit full model
BTbroodsize_fullmodel <- glm(BroodSize ~ Year + LayDate + I(LayDate^2) + understory_percentage +
                            deciduous_largescale + deciduous_25m + deciduous_7m +
                            LayDate*Year + deciduous_largescale*Year +
                            deciduous_25m*Year + deciduous_7m*Year,
                          data=BT.data2, family=poisson)
summary(BTbroodsize_fullmodel)

# Backward regression model
BTbroodsize_stepmodel <- stepAIC(BTbroodsize_fullmodel, direction = "backward",
                              trace = FALSE)
summary(BTbroodsize_stepmodel)

# Forward regression model
# Need to define the null model
BTbroodsize_nullmodel <- glm(BroodSize ~ 1, data=BT.data, family=poisson)

# Run forward regression

BTbroodsize_stepmodel2 <- stepAIC(BTbroodsize_nullmodel, # start with a model containing no variables
                               direction = 'forward', # run forward selection
                               scope = list(upper = BTbroodsize_fullmodel, # the maximum to consider is a model with all variables
                                            lower = BTbroodsize_nullmodel), # the minimum to consider is a model with no variables
                               trace = 0) # do not show the step-by-step process of model selection

summary(BTbroodsize_stepmodel2) # different from backward...

AICc(BTbroodsize_stepmodel, BTbroodsize_stepmodel2) # AIC from backward selection is better

# Stepwise regression model
BTbroodsize_stepmodel3 <- stepAIC(BTbroodsize_fullmodel, direction = "both",
                                  trace = FALSE)
summary(BTbroodsize_stepmodel3) # same as backwards

# Final broodsize model
BTbroodsize_model <- glm(BroodSize ~ deciduous_largescale, data=BT.data2, family=poisson)

# Might be worth adding back in laydate? Although it makes model worse.
BTbroodsize_model2 <- glm(BroodSize ~ deciduous_largescale + LayDate, data=BT.data2, family=poisson)
summary(BTbroodsize_model2)
AICc(BTbroodsize_model,BTbroodsize_model2)
# Only works with hatch gap... not including

## Model validation ##
par(mfrow=c(2,2))
plot(BTbroodsize_model)
plot(BTbroodsize_model2)
# Normal QQ plot shows that residuals are not normally distributed,
# but that's fine because I used a poisson error distribution..?
par(mfrow=c(1,1))
# Collinearity - all looks fine.
vif(BTbroodsize_model)
# Check for over-dispersion
BTbroodsize_model$deviance/BTbroodsize_model$df.residual
# Actually under-dispersed, but this is fine.

#calculate McFadden's R-squared for model
with(summary(BTbroodsize_model), 1 - deviance/null.deviance)
#0.0978

# with laydate
with(summary(BTbroodsize_model2), 1 - deviance/null.deviance)
#0.1278

#' BIOLOGY
#'
#' Slight negative effect of deciduous area within largescale radius on broodsize.
#' - Does not make much sense... although it seems to improve the model.
#' - Possible that exotic deciduous trees are having a negative effect. Blue tits may choose
#'   'leafy' territories, but exotic trees may not provide the right foraging habitat.
#'
#' Negative effect of laydate on brood size.... but not significant.
#' - Later laydate causes smaller brood size.
#' - Late layers may need to push forward hatching so that fledglings have more time
#'   to mature before winter, therefore there is less time to lay more eggs, then a smaller
#'   brood.

# Simple plot
plot(jitter(BroodSize) ~ LayDate, main="Blue tits", data=BT.data2, pch=19)

# Scatterplot of brood size against lay date
ggplot(BT.data2, aes(LayDate,BroodSize)) +
  geom_point(position = "jitter") +
  labs(x="Lay Date", y="Brood Size", title="Brood Size in Blue Tits") + theme_minimal() +
  stat_smooth(method = lm, color="dark blue") +
  theme(plot.title = element_text(hjust=0.5, size=14))

##### Effect plot with jtools ####
effect_plot(BTbroodsize_model2, pred = LayDate, interval = TRUE,
            partial.residuals = TRUE, jitter = 0.2, line.colors = "dark blue")

# Scatterplot of brood size against largescale deciduous area
ggplot(BT.data2, aes(deciduous_largescale,BroodSize)) +
  geom_point(position = "jitter") +
  labs(x="Number of deciduous trees between 25-75m", y="Brood Size",
       title="Brood Size in Blue Tits") + theme_minimal() +
  stat_smooth(method = lm, color="dark green") +
  theme(plot.title = element_text(hjust=0.5, size=14)) + ylim(0,11)

##### Effect plot with jtools ####
effect_plot(BTbroodsize_model2, pred = deciduous_largescale, interval = TRUE,
            partial.residuals = TRUE, jitter = 0.4, line.colors = "dark green")

##### FLEDGLING MODEL #####

# Model predicting the number of fledglings
# Includes nests where at least one egg hatched

# Change fledgling number to covariate, and subset
BT.data2<-subset(BT.data, BroodSize>=1)

# Fit the full model
BTfledge_fullmodel <- glm(Fledged ~ Year + LayDate + I(LayDate^2)+
                    understory_percentage +
                    deciduous_largescale + deciduous_25m + deciduous_7m +
                    LayDate*Year + deciduous_largescale*Year +
                    deciduous_25m*Year + deciduous_7m*Year,
                  data=BT.data2, family=poisson)
# Backward regression model
BTfledge_stepmodel <- stepAIC(BTfledge_fullmodel, direction = "backward",
                      trace = FALSE)
summary(BTfledge_stepmodel)

# Forward regression model
# Need to define the null model
BTfledge_nullmodel <- glm(Fledged ~ 1, data=BT.data2, family=poisson)

# Run forward regression

BTfledge_stepmodel2 <- stepAIC(BTfledge_nullmodel, # start with a model containing no variables
                                  direction = 'forward', # run forward selection
                                  scope = list(upper = BTfledge_fullmodel, # the maximum to consider is a model with all variables
                                               lower = BTfledge_nullmodel), # the minimum to consider is a model with no variables
                                  trace = 0) # do not show the step-by-step process of model selection

summary(BTfledge_stepmodel2)

AICc(BTfledge_stepmodel,BTfledge_stepmodel2) # forwards model better

#Final fledging model
BTfledge_model <- glm(Fledged ~ Year + LayDate + LayDate*Year + deciduous_25m + deciduous_25m*Year,
                          data=BT.data2, family=poisson)
summary(BTfledge_model)

## Model validation ##
par(mfrow=c(2,2))
plot(BTfledge_model)
# Normal QQ plot shows that residuals are not normally distributed,
# but that's fine because I used a poisson error distribution..?
par(mfrow=c(1,1))
# Collinearity
vif(BTfledge_model)
# Check for over-dispersion
BTfledge_model$deviance/BTfledge_model$df.residual
# Over-dispersed a little... but theta ~ 2, so not too bad

#calculate McFadden's R-squared for model
with(summary(BTfledge_model), 1 - deviance/null.deviance)
#0.536507

#' BIOLOGY
#'
#' Positive effect of deciduous area within 25m radius on fledgling number.
#' - Deciduous trees supply good foraging habitat with caterpillar prey.
#' - Blue tits are foraging within 25m for catterpillars.
#' - More catterpillars means higher survival rate of chicks.
#'
#' Interaction of year and deciduous area is not significant, BUT
#' - Graph shows a bit of a realtionship.
#' - In 'bad years' (2020, 2021), area of deciduous trees has less of an effect on fledglings.
#' - In bad years, food availability is low everywhere, even in 'high quality' territories.
#'
#' Negative relationship of laydate on fledgling number.
#' - Later laydate causes smaller fledgling number.
#' - Late breeding birds may miss the highest peak of caterpillar density,
#'   so less chicks survive.
#' - Or, late breeders may have less time to produce eggs and have to push forward incubating,
#'   so fewer chicks in the first place? This is more likely since laydate has a similar
#'   relationship with broodsize.
#'
#' Negative effect of hatch gap on fledgling number.
#' - Delayed hatching decreases number of fledglings.
#' - Maybe eggs become unviable during delay period?
#' - Ask Adele about this!
#'
#' Fledgling number not influenced year by year.
#' - Rather weather conditions within years likely to effect number of fledglings.
#'

# Reminder of model
BTfledge_model <- glm(Fledged ~ Year + I(LayDate^2)+ deciduous_25m + deciduous_25m*Year,
                      data=BT.data2, family=poisson)
summary(BTfledge_model)

# Simple plots
plot(jitter(Fledged) ~ LayDate, main="Blue tits", data=BT.data, pch=19)
plot(jitter(Fledged) ~ deciduous_25m, main="Blue tits", data=BT.data, pch=4, cex=0.9)
plot(jitter(Fledged) ~ Year, main="Blue tits", data=BT.data, pch=19)

# Scatterplot of deciduous trees at 25m and fledglings
ggplot(BT.data2, aes(deciduous_25m,Fledged)) +
  geom_point(position = "jitter") +
  labs(x="Number of Deciduous Trees Within 25m Radius", y="Number of Fledglings") + theme_minimal() +
  stat_smooth(method = lm, color="dark green") + xlim(0,6)

# Scatterplot of deciduous trees over 25m and fledglings showing interaction
ggplot(BT.data2, aes(deciduous_25m,Fledged, colour=Year)) +
  geom_point(position = "jitter") +
  labs(x="Number of Deciduous Trees Within 25m Radius", y="Number of Fledglings") + theme_minimal() +
  stat_smooth(method = lm, se=FALSE)

# Scatterplot of LayDate and fledglings
ggplot(BT.data2, aes(x=LayDate, y=Fledged)) + geom_point(position = "jitter") +
  labs(x="Lay Date", y="Fledgling Number", title="Number of Fledglings in Blue Tits") + theme_minimal() +
  stat_smooth(color="dark blue", method='lm', formula = y ~ I(x^2)) +
  theme(plot.title = element_text(hjust=0.5, size=14))

# Scatterplot of LayDate and fledglings showing interaction of year
ggplot(BT.data2, aes(LayDate,Fledged, colour=Year)) +
  geom_point(position = "jitter") +
  labs(x="LayDate", y="Number of Fledglings") + theme_minimal() +
  stat_smooth(method = lm, se=FALSE)

# Scatterplot of year and fledglings
BT.data2.summary <- aggregate(. ~ Year, mean, data=BT.data2) # summary of means for crossbar

ggplot(BT.data2, aes(x=Year, y=Fledged, colour=Year)) +
  geom_jitter(width=0.2) +
  geom_crossbar(data=BT.data2.summary, aes(ymin = Fledged, ymax = Fledged),
                size=0.5,col="black", width = .5) +
  labs(x="Year", y="Number of Fledglings", title="Fledgling Number in Blue Tits") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5, size=14))

# Boxplot year and fledglings
ggplot(BT.data2, aes(x=Year, y=Fledged)) +
  geom_boxplot(aes(fill = Year), show.legend = FALSE)+
  labs(x="Year", y="Number of Fledglings") + theme_minimal()

##### FLEDGING SUCCESS MODEL #####

# Don't think I'll include this...
# Model predicting the probably of at least one chick fledging
# in nests where there was at least one egg laid.

# Use a logistic reggression model (binomial - y/n)

# Subset of data where at least one egg laid:
BT.hatchsuccess<-subset(BT.data, ClutchSize>=1)
# 21 nests with chicks that didn't fledge.... okay sample size.

# Add column of success (yes/no)

BT.hatchsuccess<-mutate(BT.hatchsuccess, FledgeSuccess = ifelse(Fledged >0, "1", "0"))
BT.hatchsuccess <- filter(BT.hatchsuccess, FledgeSuccess != "NA")
BT.hatchsuccess$FledgeSuccess <- as.numeric(BT.hatchsuccess$FledgeSuccess)

# Fit the full model
fledgesuccess_fullmodel <- glm(FledgeSuccess ~ Year + LayDate + I(LayDate^2) + understory_percentage +
                    deciduous_largescale + deciduous_25m + deciduous_7m +
                    LayDate*Year + deciduous_largescale*Year +
                    deciduous_25m*Year + deciduous_7m*Year,
                  data=BT.hatchsuccess, family=binomial)

# Backwards regression model
fledgesuccess_stepmodel <- stepAIC(fledgesuccess_fullmodel, direction = "backward",
                      trace = FALSE)
summary(fledgesuccess_stepmodel)
