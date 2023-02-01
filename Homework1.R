exdata <- read.csv("C:/Users/Synoeca/Documents/STAT 510/Vitruvian2015.csv")
exdata
#freq(exdata$Height, plot = T, main = 'Height (barplot)')
heightBarplot <- table(exdata$Height)
heightBarplot
#barplot(heightBarplot)
#boxplot(exdata$Height, exdata$Arms, main = "Boxplot", names = c("A", "B"), col = c("green", "yellow"))
#hist(exdata$Height)
#pie(table(exdata$Gender))
stem(exdata$Height, scale = 2)