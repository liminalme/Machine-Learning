#Exploratory Data Analysis
# Structure of titanic1
str(titanic)

# 2 - Distribution of sexes within the classes of the ship using ggplot() 
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# 3 - Estimate chances of survival
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")+ facet_grid(.~Survived)

# 4 - An object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Scatter plot
ggplot(titanic, aes(x = Pclass,y=Age, colour =Sex )) +
  geom_point(size=3,alpha=0.5, position=posn.jd)+ facet_grid(.~Survived)

