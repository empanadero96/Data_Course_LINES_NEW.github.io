library(tidyverse)
library(patchwork)
library(janitor)
library(easystats)
library(modelr)
library(broom)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(directlabels)


# Load in the 100m splits data set
data100m <- read_csv(file = "olympic100mSplits.csv")

# Tidy the data100m  data set, call it "df100m"
df100m <- 
  pivot_longer(data100m, 2:8, names_to = "runner", values_to = "times")

colnames(df100m)[1] = "splits"

df100m$splits<-gsub("m","",as.character(df100m$splits))

# Flip the data100m data set, call it "flip100m"
names100<-c("RT", "Start-10m", "10-20m", "20-30m", "30-40m", "40-50m", "50-60m", "60-70m", "70-80m", "80-90m", "90-100m", "TOTAL")

flip100m <- setNames(data.frame(t(data100m[,-1])), data100m[,1])

colnames(flip100m) <- names100

# Load in the 400m splits data set
data400m <- read_csv(file = "Olympic400mSplits.csv")

# Tidy the data400m data set, call it "df400m"
df400m <- 
  pivot_longer(data400m, 2:9, names_to = "runner", values_to = "times")

colnames(df400m)[1] = "splits"

#Flip the data400m data set, call it "flip400m"
names400 <- c("Start-100m", "100-150m", "150-200m", "200-250m", "250-300m", "300-350m", "350-400m", "1st200m", "2nd200m", "Differential", "TOTAL")

flip400m <- setNames(data.frame(t(data400m[,-1])), data400m[,1])

colnames(flip400m) <- names400

#Plot df100m
p1 <- df100m %>% 
  filter(!grepl("RT", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("Start-10", splits)) %>% 
  ggplot(mapping = aes(x = splits, y = times)) +
  geom_point(color = "white") +
  facet_wrap(~runner) +
  theme(axis.text.x = element_text(angle = 90))+
  theme_solarized_2(light=F) +
  labs(x = "SPLITS", y = "SECONDS")+
  theme(axis.title.x=element_text(colour="white")) +
  theme(axis.title.y=element_text(colour="white")) +
  theme(axis.text.x = element_text(color = "white")) +
  theme(axis.text.y = element_text(color = "white")) +
  theme(strip.text = element_text(colour = "white")) +
  theme(axis.text.x = element_text(angle = 90))

p1

#Save this plot (100m_splits)
ggsave(file = "100m_splits.png")

# plot df400m
p2 <- df400m %>% 
  filter(!grepl("Differential", splits)) %>% 
  filter(!grepl("RT", splits)) %>% 
  filter(!grepl("0-100", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>%
  filter(!grepl("1st", splits)) %>% 
  filter(!grepl("2nd", splits)) %>% 
  ggplot(mapping = aes(x = splits, y = times)) +
  theme_solarized_2(light=F) +
  geom_point(color = "white") +
  facet_wrap(~runner) +
  theme(axis.title.x=element_text(colour="white")) +
  theme(axis.title.y=element_text(colour="white")) +
  theme(axis.text.x = element_text(color = "white")) +
  theme(axis.text.y = element_text(color = "white")) +
  theme(strip.text = element_text(colour = "white")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "SPLITS", y = "TIMES")

  
p2

#Save this plot (400m_splits)
ggsave(file = "400m_splits.png")

#Load in the bigger 100m data set
olympic100m <- read.csv("olympic100mNewData.csv")

olympic100m <- t(olympic100m) %>% as.data.frame()
colnames(olympic100m) <- olympic100m[1,]
olympic100m$athlete <- rownames(olympic100m)
olympic100m <- olympic100m[2:20,c(16, 1:15)]
olympic100m[2:16] <- lapply(olympic100m[2:16], FUN = as.numeric)



cor.test(olympic100m$TOTAL, olympic100m$RT)
cor.test(olympic100m$TOTAL, olympic100m$`Start-10m`)
cor.test(olympic100m$TOTAL, olympic100m$`10-20m`)
cor.test(olympic100m$TOTAL, olympic100m$`20-30m`)
cor.test(olympic100m$TOTAL, olympic100m$`30-40m`)
cor.test(olympic100m$TOTAL, olympic100m$`40-50m`)
cor.test(olympic100m$TOTAL, olympic100m$`50-60m`)
cor.test(olympic100m$TOTAL, olympic100m$`60-70m`)
cor.test(olympic100m$TOTAL, olympic100m$`70-80m`)
cor.test(olympic100m$TOTAL, olympic100m$`80-90m`)
cor.test(olympic100m$TOTAL, olympic100m$`90-100m`)
cor.test(olympic100m$TOTAL, olympic100m$`Age`)
cor.test(olympic100m$TOTAL, olympic100m$`height(cm)`)
cor.test(olympic100m$TOTAL, olympic100m$`weight(lb)`)


#Find correlation for 100m
cor.test(flip100m$TOTAL, flip100m$RT)
cor.test(flip100m$TOTAL, flip100m$`Start-10m`)
cor.test(flip100m$TOTAL, flip100m$`10-20m`)
cor.test(flip100m$TOTAL, flip100m$`20-30m`)
cor.test(flip100m$TOTAL, flip100m$`30-40m`)
cor.test(flip100m$TOTAL, flip100m$`40-50m`)
cor.test(flip100m$TOTAL, flip100m$`50-60m`)
cor.test(flip100m$TOTAL, flip100m$`60-70m`)
cor.test(flip100m$TOTAL, flip100m$`70-80m`)
cor.test(flip100m$TOTAL, flip100m$`80-90m`)
cor.test(flip100m$TOTAL, flip100m$`90-100m`)

#See that the 20-30m and 60-70m are significant; P-value less that 0.05

#Find correlation for 400m
cor.test(flip400m$TOTAL, flip400m$`Start-100m`)
cor.test(flip400m$TOTAL, flip400m$`100-150m`)
cor.test(flip400m$TOTAL, flip400m$`150-200m`)
cor.test(flip400m$TOTAL, flip400m$`200-250m`)
cor.test(flip400m$TOTAL, flip400m$`250-300m`)
cor.test(flip400m$TOTAL, flip400m$`300-350m`)
cor.test(flip400m$TOTAL, flip400m$`350-400m`)
cor.test(flip400m$TOTAL, flip400m$`1st200m`)
cor.test(flip400m$TOTAL, flip400m$`2nd200m`)
cor.test(flip400m$TOTAL, flip400m$`Differential`)

#See that 200-250m, 300-300m, 2nd200m, and Differential are significant; P-value less than 0.05

# Do some plotting with our new findings in the 100 meter dash
# Plot the 20-30m splits compared to total

plot2030m <- df100m %>% 
  filter(splits == "TOTAL" | splits == "20-30") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot2030m)[2] <- "split"

p3 <- ggplot(plot2030m, aes(x = TOTAL, y = split, col = runner)) + 
  geom_point() +
  labs(x = "TOTAL", y = "20-30m split")
  
p3

# Save this plot (20-30m_splits)
ggsave(file = "20-30m_splits.png")

# Plot the 60-70m splits compared to total
plot6070m <- df100m %>% 
  filter(splits == "TOTAL" | splits == "60-70") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot6070m)[2] <- "split"

p4 <- ggplot(plot6070m, aes(x = TOTAL, y = split, col = runner)) + 
  geom_point() +
  labs(x = "TOTAL", y = "60-70m split")

p4

# Save this plot (60-70m_splits)
ggsave(file = "60-70m_splits.png")

#Compare the 20-30m split to the 60-70m split
comp100msplits <- df100m %>% 
  filter(splits == "60-70" | splits == "20-30") %>%
  pivot_wider(names_from = splits, values_from = times)

colnames(comp100msplits)[c(2:3)]<-c("early", "late")

p5 <- ggplot(comp100msplits, aes(x = early, y=,late, color = runner))+ 
  geom_point() +
  labs(x = "20-30m split", y = "60-70m split")

p5

#Save this plot (comp100m_splits)
ggsave(file = "comp100m_splits.png")

# Do some plotting with our new findings in the 400 meter dash

#200-250m, 300-350m, 2nd200m, and Differential

#Plot the 200-250m data
plot200250m <- df400m %>% 
  filter(splits == "TOTAL" | splits == "200-250") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot200250m)[2] <- "split"

p6 <- ggplot(plot200250m, aes(x = TOTAL, y = split, col = runner)) + 
  geom_point()+
  labs(y = "200-250m split")

p6

#Save this plot(200-250m_splits)
ggsave(file = "200-250m_splits.png")

#Plot the 300-350m data
plot300350m <- df400m %>% 
  filter(splits == "TOTAL" | splits == "300-350") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot300350m)[2] <- "split"

p7 <- ggplot(plot300350m, aes(x = TOTAL, y = split, col = runner)) + 
  geom_point(size = 4)+
  labs(y = "300-350m split")

p7

#Whoa! p7 is by FAR the the split with the greatest correlation
#Definitely use this

#Save this plot(300-350m_splits)
ggsave(file = "300-350m_splits.png")

#Plot the data for the 2nd 200m
plot2nd200m <- df400m %>% 
  filter(splits == "TOTAL" | splits == "2nd_200") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot2nd200m)[2] <- "split"

p8 <- ggplot(plot2nd200m, aes(x = TOTAL, y = split, col = runner)) + 
  geom_point()+
  labs(y = "2nd200m split")

p8

#Save this plot(2nd200m_split)
ggsave(file = "2nd200m_split.png")

#Plot the data for the Differential
plot400mdiffer <- df400m %>% 
  filter(splits == "TOTAL" | splits == "Differential") %>% 
  pivot_wider(names_from = splits, values_from = times)

colnames(plot400mdiffer)[2] <- "split"

p9 <- ggplot(plot2nd200m, aes(x = TOTAL, y = split, col = runner)) + 
  geom_point()+
  labs(y = "1st and 2nd 200m difference")

p9

#Save this plot(1st_2nd_diff)
ggsave(file = "1st_2nd_diff.png")

# Calculate the speed of the runners
df100m$speed <- 22.3694/df100m$times

df400m$speed <- 111.847/df400m$times

# Make an animated plot
# Save it as "animation100m"
animation100m <- df100m %>% 
  filter(!grepl("RT|TOTAL", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("Start-10", splits)) %>% 
  mutate(splits_int = as.integer(factor(splits, 
                                        levels = c("10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")))) %>% 
  nest(data = -runner) %>% 
  mutate(
    lm_model = map(data, ~loess(speed ~ splits_int, data = .x)),
    augmented = map(lm_model, augment) %>% map(select, .fitted)
  ) %>%
  unnest(c(augmented, data)) %>% 
  ggplot(aes(splits, .fitted, col = runner, group = runner)) +
  geom_line(size=1) +
  labs(
    y = "Speed (mph)",
    x = "Splits",
    color = "Runner",
    title="Speed Comparison of Top 100 Meter \nDash Runners"
  ) +
  theme_solarized_2(light=F) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust=0.5)) +
  transition_reveal(splits_int) +
  coord_fixed(ratio = 0.5)

#Save it to files
anim_save("100m_animation.gif", animation100m)

# Animate the 400m dash
animation400m <- df400m %>% 
  filter(!grepl("RT|TOTAL|Differential", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("2nd_200", splits)) %>% 
  filter(!grepl("1st_200", splits)) %>% 
  filter(!grepl("0-100", splits)) %>% 
  mutate(splits_int = as.integer(factor(splits, 
                                        levels = c("100-150","150-200","200-250","250-300","300-350","350-400")))) %>% 
  nest(data = -runner) %>% 
  mutate(
    lm_model = map(data, ~loess(speed ~ splits_int, data = .x)),
    augmented = map(lm_model, augment) %>% map(select, .fitted)
  ) %>%
  unnest(c(augmented, data)) %>% 
  ggplot(aes(splits, .fitted, col = runner, group = runner)) +
  geom_line(size=1) +
  labs(
    y = "Speed (mph)",
    x = "Splits",
    color = "Runner",
    title="Speed Comparison of Top 400 Meter \nDash Runners"
  ) +
  theme_solarized_2(light=F) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust=0.5)) +
  transition_reveal(splits_int) +
  coord_fixed(ratio = 0.5)

# Save it to files
#Save it to files
anim_save("400m_animation.gif", animation400m)

df400m %>% 
  filter(!grepl("RT|TOTAL|Differential", splits)) %>% 
  filter(!grepl("TOTAL", splits)) %>% 
  filter(!grepl("2nd_200", splits)) %>% 
  filter(!grepl("1st_200", splits)) %>% 
  filter(!grepl("0-100", splits)) %>% 
  mutate(splits_int = as.integer(factor(splits, 
                                        levels = c("100-150","150-200","200-250","250-300","300-350","350-400")))) %>% 
  nest(data = -runner) %>% 
  mutate(
    lm_model = map(data, ~loess(speed ~ splits_int, data = .x)),
    augmented = map(lm_model, augment) %>% map(select, .fitted)
  ) %>%
  unnest(c(augmented, data)) %>% 
  ggplot(aes(splits, .fitted, col = runner, group = runner)) +
  geom_line(size=1) +
  labs(
    y = "Speed (mph)",
    x = "Splits",
    color = "Runner",
    title="Speed Comparison of Top 400 Meter \nDash Runners"
  ) +
  theme_solarized_2(light=F) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust=0.5)) +
  transition_reveal(splits_int)





