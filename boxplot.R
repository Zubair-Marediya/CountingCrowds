data = read.csv("dataproject.csv")
View(data)
c()


require(ggplot2)
data$Grid.Color=factor(data$Grid.Color , rev(as.character(levels(data$Grid.Color))))
ggplot(data, aes(x = Grid.Color, y = Grid.Count, fill = Grid.Color)) + 
  geom_boxplot(alpha = 0.3, colour = c("red2", "yellow", "dodgerblue", "springgreen3", "hotpink2")) + 
  scale_fill_manual(name = "Grid Color", values = c("red2", "yellow", "dodgerblue", "springgreen3", "hotpink2"), labels = c("76-100%", "51-75%", "26-50%", "6-25%", "0-5%"))  + 
  coord_flip() + 
  ggtitle("Boxplots of Grid Counts For Each Stratum") +
  theme(axis.title = element_text(size = 25, lineheight = 0.8, face = "bold"),
        plot.title = element_text(size = 25, lineheight= 0.8, face="bold"),
        panel.background = element_rect(fill = "white", color = "black")) +
  xlab("Stratum") + 
  ylab("Grid Count")


ggplot(data, aes(x = Grid.Color, y = Grid.Count, fill = Grid.Color)) + geom_boxplot() + coord_flip()

r=c(435,498,579,637,591,670,578,474,424,431,444,509,561,511)
y=c(547,410,613,218,390,443,410,190,550)
b=c(101,143,50,100,171,183,93,74)
g=c(27,26,21,27,22,41,62,35,42,23)
p=c(13,14,5,11,19,20,16,8,16,1,7,10,7)

70*mean(r)+43*mean(y)+40*mean(b)+52*mean(g)+65*mean(p)

red = data$Grid.Count[data$Grid.Color == "r"]
yellow = data$Grid.Count[data$Grid.Color == "y"]
blue = data$Grid.Count[data$Grid.Color == "b"]
green = data$Grid.Count[data$Grid.Color == "g"]
purple = data$Grid.Count[data$Grid.Color == "p"]

stratum.list = list(red, yellow, blue, green, purple)
stratum.population = c(70, 43, 40, 52, 65)

stratum.mean = sapply(stratum.list, mean)
stratum.variance = sapply(stratum.list, var)
strata.total = sum(stratum.population * stratum.mean)

stratum.samplesize = c(14, 9, 8, 10, 13)

FPC = 1 - stratum.samplesize/stratum.population
strata.totalSE = sqrt( sum(stratum.population^2 * FPC * (stratum.variance/stratum.samplesize)) )

CI.upper = strata.total + 1.96 * strata.totalSE
CI.lower = strata.total - 1.96 * strata.totalSE
