library(ggplot2)
raw <- read.csv("all.csv",header=TRUE)

library(dplyr)
filtered <- filter(
    raw,
    source!="krasnopolsky",
    source!="govardhanen",
    2*x.m/t.s^2/9.81<0.3,
)

data <- mutate(
 filtered,
 ameas.ms2 = 2*x.m/t.s^2,
 mhat = m2.kg/(m1.kg+m2.kg+mc.kg),
 ahat = ameas.ms2/9.81,
 )

data2 <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2,
 mhat = m2.kg/(m1.kg+m2.kg+mc.kg),
 ahat = ameas.ms2/9.81,
 )    

library(svglite)
library(scales)
fig3 <- ggplot(data)+geom_point(aes(x=mhat,y=ahat,color=source))+
     ylim(0,0.3)+xlim(0,0.3)+
     geom_abline(slope=1.0,intercept=0.0,color='blue')+
     xlab('$\\hat{m}$')+
    ylab('$\\hat{a}$')+
    scale_color_manual(values=hue_pal()(9),
                         name=NULL,
                         labels=c("\\citet{arenas-2024-testing}",
                                  "\\citet{avalur-2024-verifying}",
                                  "\\citet{canada-2024-experimental}",
                                  #"\\citet{govardhanen-2024-newtons}",
                                  "\\citet{kedharnath-2024-examining}",
                                  "\\citet{kishore-2024-relationship}",
                                  #"\\citet{krasnopolsky-2024-testing}",
                                  "\\citet{perle-2024-experimental}",
                                  "\\citet{yagnyeshwaran-2024-verifying}"))+
    theme_bw(base_size=8)+
    guides(color=guide_legend(position="right"))+
    theme(aspect.ratio=1)
svglite('fig3.svg',width=6,height=4,pointsize=8)
print(fig3)
dev.off()

fit1 <- lm(ahat~mhat,data)
summary(fit1)

fit2 <- lm(ahat-mhat~mhat,data)
summary(fit2)

fig2 <- ggplot(data2)+geom_point(aes(x=mhat,y=ahat,color=source))+
     ylim(0,1.2)+xlim(0,1.2)+
     geom_abline(slope=1.0,intercept=0.0,color='blue')+
     xlab('$\\hat{m}$')+
    ylab('$\\hat{a}$')+
    scale_color_manual(values=hue_pal()(9),
                         name=NULL,
                         labels=c("\\citet{arenas-2024-testing}",
                                  "\\citet{avalur-2024-verifying}",
                                  "\\citet{canada-2024-experimental}",
                                  "\\citet{govardhanen-2024-newtons}",
                                  "\\citet{kedharnath-2024-examining}",
                                  "\\citet{kishore-2024-relationship}",
                                  "\\citet{krasnopolsky-2024-testing}",
                                  "\\citet{perle-2024-experimental}",
                                  "\\citet{yagnyeshwaran-2024-verifying}"))+
    theme_bw(base_size=8)+
    guides(color=guide_legend(position="right"))+
    theme(aspect.ratio=1)
svglite('fig2.svg',width=6,height=4,pointsize=8)
print(fig2)
dev.off()

fit3 <- lm(ahat-mhat~mhat,data2)
summary(fit3)
