#Welcome to the WONDERFUL world of win probability:) 
#Your goal is to run the code and respond to the questions within this script. 
#You can record your responses in this script and turn it in or in a separate Word document. 
#Remember that your work here also appears on the upcoming and LAST sprint assignment. 

###########Getting Started##########
install.packages('tidyverse') #installing packages
library(tidyverse)
install.packages("nflfastR")
library(nflfastR)

pbp2018 <- load_pbp(2018) #loading the data 
pbp2019 <- load_pbp(2019)

pbpfull = bind_rows(pbp2018,pbp2019)
rm(pbp2018, pbp2019)

#Question 1: What's happening in the getting started code block? 

############TASK 1##########

#Before we can build our win probability model, we need to do some data cleaning and preparation.
#We’ll need to be particularly careful as we do this as some of the preparation steps are mildly tricky. 
#Each row in the data set corresponds to a play in an NFL game.
#Your first data preparation step is to create a new variable (use the “mutate” function) to indicate which team won the game. 
#The end of game score differential is given in the “result” variable and is given in terms of home team. 
# If “result” is positive then the home team (given as “home_team” in the dataset) is the winner. 
#If the result is negative, then the away team (“away_team”) is the winner. 
#If the result is equal to zero, then the result is a tie. 
#Call your new variable “winteam”. Hint: You will need to employ “ifelse” statements to make this work.


pbpfull = pbpfull %>% mutate(winteam = ifelse(result > 0, home_team,
                                              ifelse(result == 0, "tie",
                                                     ifelse(result < 0, away_team, "NA"))))

#Next we need to add a variable to indicate if the team in possession wins. Call this new variable “poswins”.
#If the “winteam” variable is equivalent to the “posteam” variable, then the “poswins” variable will equal the string “PosWins”, otherwise “PosLoses”. 
#This will be our response varible. 
#To ensure that the variable factor levels are in the correct order, be sure to add the following command after your “ifelse” that create the “winteam” variable:

pbpfull = pbpfull %>% mutate(poswins = ifelse(winteam == posteam,"PosWins","PosLoses")) %>%
  mutate(poswins = fct_relevel(poswins, "PosLoses"))

#The dataset includes a variable for game “spread”. The spread is a type of betting option available for many types of sports. 
#If a team is favored to win, the spread (in terms of that team) will be negative. 
#If the team is the underdog, then the spread (in terms of that team) will be positive.
#In this dataset, the spread is given relative to the home team. Add a variable called “posspread” (representing the spread relative to the team in possession) 
#that is equal to the spread (given as “spread_line”) if the “posteam” is equal to the “home_team”. 
#If the “posteam” is not equal to the “home_team”, then “posspread” should be "-1*spread_line".

pbpfull = pbpfull %>% mutate(posspread = ifelse(posteam == home_team, spread_line, -1*spread_line))

#Use the code below to convert the “qtr” (quarter), “down”, and “poswins” variables to factors. 
#This allows for the opportunity to assess these variables by level. 

cols = c("qtr","down","poswins")
pbpfull = pbpfull %>% mutate_at(cols,as_factor)

pbpfull = pbpfull %>% drop_na(yardline_100) %>%
  drop_na(game_seconds_remaining) %>%
  drop_na(down) %>%
  drop_na(posspread) %>%
  drop_na(score_differential)

pbpfull = pbpfull %>% filter(qtr != 5)

pbpfull = pbpfull %>% filter(result != 0)

#Question 2: The remaining code is not annotated (after designating factors). What is happening in these code lines? 

#####TASK 2###########

#We start the model construction process by building a logistic regression model. 
#This model is very similar to a linear regression model.  
#A logistic regression model predicts log(p/(1 − p)) where p is the probability that an event occurs. 
#In the case of a win probability model, p refers to the probability that the team in possession of the ball ultimately wins the game. 
#The logistic regression equation is then:

mod2 <- glm(
  poswins ~ yardline_100 +
    game_seconds_remaining +
    down +
    ydstogo +
    posspread +
    score_differential +
    qtr +                                          
    score_differential:game_seconds_remaining,     
  data = pbpfull,
  family = "binomial"
)

summary(mod2)

#Question 3: Describe the variables in the model. What do they mean? 
#Hint: this website will help you: https://www.nflfastr.com/reference/fast_scraper.html

######TASK 3##########
#Let's test the predictive accuracy. 

install.packages('caTools')
library('caTools')
sample<-sample.split(pbpfull$poswins, SplitRatio = .8)
train<-subset(pbpfull, sample==TRUE)
test<-subset(pbpfull, sample==FALSE)

#modtrain <- glm(poswins ~ yardline_100 + game_seconds_remaining + down +
              #ydstogo + posspread + score_differential, data = train, family = binomial)
#glm.probs <- predict(modtrain, newdata = test, type = "response")
#glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
#table(glm.pred,test$poswins)

glm2_probs <- predict(mod2, newdata = test, type = "response")
glm2_pred  <- ifelse(glm2_probs > 0.5, 1, 0)
table(glm2_pred, test$poswins)

#Question 4: Is this model any good? 

#Question 5 (and the beginning of Day 2): Do the coefficients make sense? 
#Hint: A positive coefficient suggests a positive association between the predictor variable 
#and the likelihood of the event happening (positive class or win).
#a negative coefficient suggests a negative association between the predictor variable 
#and the likelihood of the event happening (positive class).

######TASK 4#########
#Now we use the logistic regression model to make predictions. 
#The predictions will be probabilities for each row in the dataset and indicate the probability of the team in possession winning the game given the
#conditions occurring before the play represented in each row. 
#The code below produces an object that holds the predicted probabilities.

#predictions_log = predict(mod1, type = "response")

predictions_log2 <- predict(mod2, type = "response")

#The code also modifies the probabilities to always be in terms of the home team. 
#The predicted probabilities for the home team are stored in a variable called “prob_home_log”.

#pbpfull = pbpfull %>% mutate(problog = predictions_log) %>%
  #mutate(prob_home_log = ifelse(posteam == home_team, problog , 1-problog))
#ggplot(pbpfull,aes(x=prob_home_log)) + geom_histogram()

pbpfull <- pbpfull %>%
  mutate(problog2 = predictions_log2) %>%
  mutate(prob_home_log2 = ifelse(posteam == home_team, problog2, 1 - problog2))

ggplot(pbpfull,aes(x=prob_home_log2)) + geom_histogram()
#You just created a chart to show the distribution of the “prob_home_log” variable. 

#Question 6: Comment on the distribution.

######TASK 5#########
ggplot(pbpfull,aes(x=score_differential,y=problog2)) + geom_point()

#Create a plot that compares the “problog” variable (the win probability of the team in possession) to
#the “score_differential” variable. 

#Question 7: Does this plot align with your expectations? Discuss.

######TASK 6#########

#Now let’s plot the win probability model results for a specific game. 
#Let’s choose the Saints versus Texans game from Week 1 of the 2019 season. 
#The Saints won 30-28 at home. 

#Question 8: Describe the win probability visual - what can you learn about the game?

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == "2018_05_IND_NE") %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log2)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "red")
gameid = "2018_05_IND_NE"
homeid = pbpfull %>% filter(game_id == gameid) %>% select(home_team) %>% distinct()
awayid = pbpfull %>% filter(game_id == gameid) %>% select(away_team) %>% distinct()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == gameid) %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log2)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "red") +
  annotate("label", x = 3500, y = .95, label = paste0(homeid$home_team)) +
  annotate("label", x = 3500, y = .05, label = paste0(awayid$away_team))

gameid = "2018_05_IND_NE"
homeid = pbpfull %>% filter(game_id == gameid) %>% select(home_team) %>% distinct()
awayid = pbpfull %>% filter(game_id == gameid) %>% select(away_team) %>% distinct()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == "2018_05_IND_NE") %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log2)) +
  geom_rect(aes(xmin=0, xmax=3600, ymin=0.5, ymax=1), fill = "#C60C30", alpha = 1) + geom_rect(aes(xmin=0, xmax=3600, ymin=0, ymax=0.5), fill = "#002C5F", alpha = 1) + geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "red") +
  annotate("label", x = 3500, y = .95, label = paste0(homeid$home_team)) +
  annotate("label", x = 3500, y = .05, label = paste0(awayid$away_team))

