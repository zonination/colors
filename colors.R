# Color Generator - Shows available colors
colors <- read.csv("colors.csv")
library(ggplot2)

colors$hex<-paste("#",
                  as.hexmode(colors$R),
                  as.hexmode(colors$G),
                  as.hexmode(colors$B),
                  sep="")

colors$Family <- factor(colors$Family,
                        c("Reds","Oranges","Yellows","Greens",
                          "Cyans","Blues","Violets","Magentas",
                          "Blacks","Greys","Whites"))

ggplot(colors,aes(x=1,y=Category))+
  geom_tile(aes(fill=Category),color="black")+
  scale_fill_manual("",values=colors$hex)+
  guides(fill="none")+
  labs(title="Color Definitions",
       x="",y="")+
  facet_grid(Family~.,scales="free_y", space="free_y")+
  theme_bw()+
  theme(axis.text.x=element_blank())
ggsave("color-def.png",height=10,width=2,dpi=100,type="cairo-png")