install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
players<- read_excel("players.xlsx", sheet = 1, skip = 2)
head(players)
str(players)
players$AgeDuringSeason <- as.numeric(players$AgeDuringSeason)
t.test(players$AgeDuringSeason, mu = 28)
mean_age <- mean(players$AgeDuringSeason, na.rm = TRUE)
ggplot(players, aes(x=AgeDuringSeason)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed", size = 1.5) +
  annotate("text", x = mean_age + 0.5, y = 45, label = "Mean Age", color = "red", size = 5) +
  labs(title= "Distribution of Player Ages", x = "Age", y = "Frequency") +
  theme_minimal()
fw_ages<-players$AgeDuringSeason[players$Position== "FW"]
mf_ages<-players$AgeDuringSeason[players$Position == "MF"]
shapiro.test(fw_ages)
shapiro.test(mf_ages)
var.test(fw_ages,mf_ages)
t.test(fw_ages, mf_ages,var.equal = TRUE)
box_data <- players %>%
  filter(Position== "FW"|Position == "MF")
ggplot(box_data,aes(x=Position,y =AgeDuringSeason, fill= Position)) +
  geom_boxplot() +
  labs(title = "Comparison of Ages between Forwards and Midfielders",
       x = "Position",y = "Age") +
  scale_fill_manual(values=c("FW"= "darkorange3", "MF"= "lightblue")) +
  theme_minimal()
str(players$xGoalsAdded)
sum(is.na(players$xGoalsAdded))
table(players$Position)
players$xGoalsAdded<-as.numeric(players$xGoalsAdded)
players$HighPerformer<-ifelse(players$xGoalsAdded > 10, 1, 0)
fw_df_data<-players[players$Position %in% c("FW", "DF"), ]
fw_data<-fw_df_data[fw_df_data$Position== "FW", ]
df_data<-fw_df_data[fw_df_data$Position== "DF", ]
fw_high<- sum(fw_data$HighPerformer, na.rm = TRUE)
df_high <- sum(df_data$HighPerformer, na.rm = TRUE)
fw_total<- nrow(fw_data)
df_total <-nrow(df_data)
prop.test(c(fw_high, df_high), c(fw_total, df_total), correct = F)
players$xGoalsAdded <- as.numeric(players$xGoalsAdded)
players$HighPerformer <- ifelse(!is.na(players$xGoalsAdded) & players$xGoalsAdded > 10, 1, 0)
filtered_players <- players %>%
  filter(Position %in% c("FW", "DF")) %>%
  mutate(Role = recode(Position,"FW" = "Forwards","DF" = "Defenders"))
high_perf <- filtered_players %>%
  filter(HighPerformer == 1) %>%
  count(Role)
ggplot(high_perf, aes(x = "", y = n, fill = Role)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  scale_fill_manual(
    values= c("Defenders" = "darkseagreen1", "Forwards" = "darkslategray4"),
    name= NULL)+
  labs(title = "Proportion of High-Performing Players",x = NULL,y = NULL) +
  theme_void()+
  theme(legend.position = "bottom")
players$CurrentMarketValue <- as.numeric(players$CurrentMarketValue)
reg_data<- players %>%
  filter(!is.na(xGoalsAddedPer90), !is.na(CurrentMarketValue))
reg_data <- reg_data %>%
  mutate(Exp_xGAP90 = exp(xGoalsAddedPer90))
model <- lm(CurrentMarketValue ~ Exp_xGAP90, data = reg_data)
summary(model)
ggplot(reg_data, aes(x = xGoalsAddedPer90, y = CurrentMarketValue)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ exp(x), se = TRUE, color = "blue") +
  labs(title = "Relationship between xGoalsAddedPer90 and CurrentMarketValue",x = "Expected goals per 90 minutes (xGoalsAddedPer90)",
       y = "Current Market Value (€)") +
  theme_minimal()
players$Salary<- as.numeric(players$Salary)
selected_data<- players %>%
  filter(League %in% c("Bundesliga", "MLS", "Serie A")) %>%
  mutate(LogSalary = log(Salary))
anova_result <- aov(LogSalary ~ League, data = selected_data)
summary(anova_result)
ggplot(selected_data,aes(x=League,y = LogSalary, fill = League)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  scale_fill_manual(values = c("Bundesliga" = "darkred","MLS" = "lightpink",        
    "Serie A" = "lightblue")) +
  labs(title = "Salary (€) by Bundesliga, MLS, Serie A",x = "League",
    y = "Salary (€)") +
  theme_minimal()
