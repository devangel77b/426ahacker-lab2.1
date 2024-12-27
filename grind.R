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
library(scales)
fig2 <- ggplot(data)+geom_point(aes(x=mhat,y=ahat,color=source))+
     ylim(0,0.5)+xlim(0,0.5)+
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
svglite('fig3.svg',width=6,height=4,pointsize=8)
print(fig2)
dev.off()

fit <- lm(ahat~mhat-1,data)
summary(fit)
