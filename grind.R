library(ggplot2)
raw <- read.csv("all.csv",header=TRUE)

library(dplyr)
data <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2,
 mhat = m2.kg/(m1.kg+m2.kg+mc.kg),
 ahat = ameas.ms2/9.81,
 )

library(svglite)
fig2 <- ggplot(data)+geom_point(aes(x=mhat,y=ahat,color=source))+
     ylim(0,0.3)+xlim(0,0.3)+
     geom_abline(slope=1.0,intercept=0.0,color='blue')+
     xlab('$\\frac{m_2}{m_1+m_2+m_c}$')+
     ylab('$\\hat{a}$')+
    theme_bw(base_size=8)+
    guides(color=guide_legend(position="inside"))+
    theme(legend.position.inside=c(0.1,0.7))
svglite('fig3.svg',width=6,height=4,pointsize=8)
print(fig2)
dev.off()

fit <- lm(ahat~mhat-1,data)
summary(fit)
