library(tidyverse) 
library(dplyr) 
library(ggplot2)  
Book2 <- read_csv("~/Desktop/Coding projects for fun/Outfield arm strength and DRS in R/Book2.csv") 
arm_strength <- read_csv("~/Desktop/Coding projects for fun/Outfield arm strength and DRS in R/arm_strength.csv")
arm_strength <- filter(arm_strength, primary_position >= 7) 
arm_strength <- arm_strength[ -c(3, 5:14) ]  
arm_strength <- arm_strength[ -c(4, 6:13) ]  
arm_strength <- filter(arm_strength,arm_of==arm_overall) %>% arrange(desc(max_arm_strength)) %>% 
  mutate(max_minus_avg=(max_arm_strength-arm_of)) %>% rename(Name=fielder_name) 
LF <- filter(joined_df,primary_position==7 & max_arm_strength >95) %>% arrange(desc(max_arm_strength)) 
CF <- filter(joined_df,primary_position==8 & max_arm_strength >95) %>% arrange(desc(max_arm_strength)) 
RF <- filter(joined_df,primary_position==9 & max_arm_strength >95) %>% arrange(desc(max_arm_strength))
view(arm_strength) 
view(Book2) 
#join Book2 and arm_strength on name 
joined_df <- full_join(arm_strength,Book2) %>% drop_na() %>% mutate(defense_value=((DRS+OAA)/2)) 

view(joined_df)
#Is average arm strength correlated to DRS? 
ggplot(data=joined_df)+ggtitle("Outfield Arm vs DRS")+
  geom_point(mapping=aes(x=arm_of,y=DRS))
of_arm_cor <- cor(joined_df$arm_of,joined_df$DRS) # 0.1815593
#Is max arm strength correlated to DRS? 
ggplot(joined_df, aes(x=max_arm_strength,y=DRS)) +ggtitle("Max Arm Strength vs DRS")+
  geom_point(data = joined_df, colour = 'red', size = 3)+  stat_smooth(method = "lm", 
                                                                       formula = y ~ x, 
                                                                       geom = "smooth") 
max_of_arm_cor <-cor(joined_df$max_arm_strength,joined_df$DRS) #0.207474
#Is a larger difference between average and max arm strength correlated to DRS? 
ggplot(joined_df, aes(x=max_minus_avg,y=DRS)) +ggtitle("Max-Avg Arm Strength vs DRS")+
  geom_point(data = joined_df, colour = 'blue', size = 3)+  stat_smooth(method = "lm", 
                                                                       formula = y ~ x, 
                                                                       geom = "smooth") 
max_minus_avg_cor <-cor(joined_df$max_minus_avg,joined_df$DRS) #0.06607503
#Are OAA and DRS correlated to each other for outfielders 
ggplot(joined_df, aes(x=OAA,y=DRS)) +ggtitle("OAA vs DRS")+
  geom_point(data = joined_df, colour = 'orange', size = 3)+  stat_smooth(method = "lm", 
                                                                        formula = y ~ x, 
                                                                        geom = "smooth") 

OAA_DRS_cor <-cor(joined_df$OAA,joined_df$DRS) #0.5855432
#Is average arm strength correlated to OAA? 
ggplot(joined_df, aes(x=arm_of,y=OAA)) +ggtitle("Avg Arm Strength vs OAA")+
  geom_point(data = joined_df, colour = 'black', size = 3)+  stat_smooth(method = "lm", 
                                                                       formula = y ~ x, 
                                                                       geom = "smooth")  
of_arm_OAA <- cor(joined_df$OAA,joined_df$arm_of)#0.1266179
#Is max arm strength correlated to OAA? 
ggplot(joined_df, aes(x=max_arm_strength,y=OAA)) +ggtitle("Max Arm Strength vs OAA")+
  geom_point(data = joined_df, colour = 'purple', size = 3)+  stat_smooth(method = "lm", 
                                                                       formula = y ~ x, 
                                                                       geom = "smooth") 
max_arm_OAA <- cor(joined_df$OAA,joined_df$max_arm_strength)#0.2298644
#Is a larger difference between average and max arm strength correlated to OAA?   
ggplot(joined_df, aes(x=max_minus_avg,y=OAA)) +ggtitle("Max-Avg Arm Strength vs OAA")+
  geom_point(data = joined_df, colour = 'green', size = 3)+  stat_smooth(method = "lm", 
                                                                        formula = y ~ x, 
                                                                        geom = "smooth") 
max_avg_oaa <- cor(joined_df$OAA,joined_df$max_minus_avg)# 0.1820796 
correlations <- c(max_avg_oaa,max_arm_OAA,of_arm_OAA,OAA_DRS_cor,max_minus_avg_cor,
                  max_of_arm_cor,of_arm_cor) 
correlations[c(3)]

#####DRS 
ggplot(joined_df,aes(x=max_minus_avg,y=max_arm_strength))+
  ggtitle("Outfield Arm difference vs Max velo")+xlab("Max-Avg")+
  ylab("Max velocity")+
  geom_point(data=joined_df,colour='red',size=4)+
  stat_smooth(method = 'lm', formula = y~x, geom = "smooth") 
### New Stat
OAA_Defense_value <-ggplot(joined_df,aes(x=defense_value,y=OAA))+ggtitle('Defensive value VS OAA')+ 
  ylab('OAA')+xlab('Defensive Value ((DRS+OAA)/2')+geom_point(colour='blue',size=4)+
  stat_smooth(method = 'lm',formula=y~x,geom = 'smooth') 
DRS_Defense_value <-ggplot(joined_df,aes(x=defense_value,y=DRS))+ggtitle('Defensive value VS DRS')+ 
  ylab('DRS')+xlab('Defensive Value ((DRS+OAA)/2')+geom_point(colour='pink',size=4)+
  stat_smooth(method = 'lm',formula=y~x,geom = 'smooth')  
DRS_OAA <- ggplot(joined_df,aes(x=DRS,y=OAA))+ggtitle('DRS VS OAA')+ 
  ylab('OAA')+xlab('DRS')+geom_point(colour='purple',size=4)+
  stat_smooth(method = 'lm',formula=y~x,geom = 'smooth')  
cor(joined_df$OAA,joined_df$defense_value)
cor(joined_df$DRS,joined_df$defense_value) 
cor(joined_df$OAA,joined_df$DRS) 
