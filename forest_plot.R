library(ggplot2)
library(dplyr)
library(stringr)

#### Figure 1 ---- 
f1 <- readxl::read_excel("Figure 1 - Experiences with Housing Insecurity.xlsx", sheet=3)

f1a <- f1 %>% filter(model=="HM") %>%
            ggplot(aes(x=measure, y=n, label=label)) + 
            geom_col(width=.4, alpha=.5) + geom_text(nudge_y=150, size=6) + 
            coord_flip() + theme_bw(base_size=16) + 
            scale_x_discrete(labels=function(x)(str_wrap(x, width=15))) + 
            scale_y_continuous(limits=c(0,1500)) +
            ylab("(a)") + xlab("") + 
   ggtitle("Housed & Homeless Students")

f1b <- f1 %>% filter(model=="HI") %>%
         ggplot(aes(x=measure, y=n, label=label)) + 
         geom_col(width=.4, alpha=.5) + geom_text(nudge_y=100, size=6) + 
         coord_flip() +theme_bw(base_size=16) + 
         scale_x_discrete(labels=function(x)(str_wrap(x, width=30))) + 
         scale_y_continuous(limits=c(0,500)) +
         ylab("(b)") + xlab("") + 
         ggtitle("Experiences with Housing Insecurity \n in the past 12 months") 
         #theme(plot.title = element_text(size=12))

png("fig1.png", width=1000, height=600)
   gridExtra::grid.arrange(f1a, f1b, nrow=1, widths=c(1,1.5))
dev.off()

#### Forest plots combined ---- 

hi <- readxl::read_excel("Data forest plots v3.xlsx", sheet=1)
names(hi)[3:5] <- c("hi.or", "hi.lcl", "hi.ucl")
hm <- readxl::read_excel("Data forest plots v3.xlsx", sheet=2)
names(hm)[3:5] <- c("hm.or", "hm.lcl", "hm.ucl")

fp <- full_join(hi, hm) 

fp$hi.txt<- paste0(
   sprintf("%.2f", round(fp$hi.or,2)), 
   " (", 
   sprintf("%.2f", round(fp$hi.lcl,2)), 
   ",", 
   sprintf("%.2f", round(fp$hi.ucl,2)), 
   ")"
   )

fp$hm.txt<- paste0(
   sprintf("%.2f", round(fp$hm.or,2)), 
   " (", 
   sprintf("%.2f", round(fp$hm.lcl,2)), 
   ",", 
   sprintf("%.2f", round(fp$hm.ucl,2)), 
   ")"
)

# set plot order according to hypothesis order
# pell, student, nonwhite, lgbtq, cfimpact, servicesindex, buttecountyres, rentdummy, directoutreach

fp$y <- 10-c(1,2,3, 4, 8, 5, 6, 7, 9)


nvar <- max(fp$y)
xlim <- c(0, 6)
eps <- .07


png("models.png", width=960, height=620)

par(mar=c(4, 14, 0.5, 17))
plot.new()
plot.window(xlim=xlim, ylim=c(0.5, nvar+1.5), xlab="a")
segments(1, 0, 1, nvar+.5,  col="grey")
axis(1, at=seq(0.5, max(xlim), by=.5))
# hi model
points(x=fp$hi.or, y=(fp$y-eps), pch=17, cex=1.1)
arrows(fp$hi.lcl, (fp$y-eps), fp$hi.ucl, (fp$y-eps), code=3, length=0.05, angle=90)
# hm model
points(x=fp$hm.or, y=(fp$y+eps), pch=1, cex=1.2)
arrows(fp$hm.lcl, (fp$y+eps), fp$hm.ucl, (fp$y+eps), code=3, length=0.05, angle=90)

legend(x=5, y=1.5, 
       legend = c("HI", "HM"), 
       pch = c(17,1),
       pt.cex = 1.5, 
       cex = 1.1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


mtext(fp$Label, at=fp$y, adj=1, side=2, las=2, cex=1.1)
mtext("Measure", at=nvar+1, adj=1, side=2, las=2, cex=1.5, font=2)
mtext(fp$hi.txt, at=fp$y, adj=0, side=4, las=2, cex=1.2)
mtext(fp$hm.txt, at=fp$y, adj=-1.3, side=4, las=2, cex=1.2)

mtext("Housing\nInsecurity (HI)", at=nvar+1, adj=0, side=4, las=2, cex=1, font=2)
mtext("Homelessness (HM)"       , at=nvar+1.1, adj=-1.1, side=4, las=2, cex=1, font=2)
mtext("--OR (95% CI)--"        , at=nvar+1.5, adj=-.5, side=4, las=2, cex=1.2, font=2)

dev.off()



#### Forest plots v3 ---- 
#### \- HI ----
hi <- readxl::read_excel("Tables for Robin - Forest Plots  v. 3.xlsx", sheet=1)

# extract data
nvar <- NROW(hi)
est <- hi$`Odds Ratio`
lcl <- hi$`LB CI`
ucl <- hi$`UB CI`

# extract labels
var.names <- hi$Label
est.t <- sprintf("%.2f", round(est,2))
lcl.t <- sprintf("%.2f", round(lcl,2))
ucl.t <- sprintf("%.2f", round(ucl,2))
or.text <- paste0(est.t, " (", lcl.t, ",", ucl.t, ")")


xlim <- c(0, 7)
y <- nvar:1



png("housing insecurity.png", width=960, height=620)

par(mar=c(4, 13, 0.5, 10))
plot.new()
plot.window(xlim=xlim, ylim=c(0.5, nvar+1.5), xlab="a")
points(x=est, y=y, pch=17)
axis(1, at=seq(0.5, max(xlim), by=.5))
arrows(lcl, y, ucl, y, code=3, length=0.05, angle=90)
segments(1, 0, 1, nvar+.5,  col="grey")
mtext(var.names, at=y, adj=1, side=2, las=2)
mtext("Measure", at=nvar+1, adj=1, side=2, las=2, cex=1.5, font=2)
mtext(or.text, at=y, adj=0, side=4, las=2)
mtext("OR (95% CI)", at=nvar+1, adj=0, side=4, las=2, cex=1.5, font=2)

dev.off()

#### \- HM ----
hm <- readxl::read_excel("Tables for Robin - Forest Plots  v. 3.xlsx", sheet=2)

# exclude constant or intercept
exclude <- "_cons"
hm <- hm %>% filter(!(hm$Variable %in% exclude))

# extract data
nvar <- NROW(hm)
est <- hm$`Odds Ratio`
lcl <- hm$`LB CI`
ucl <- hm$`UB CI`

# extract labels
var.names <- hm$Label
est.t <- sprintf("%.2f", round(est,2))
lcl.t <- sprintf("%.2f", round(lcl,2))
ucl.t <- sprintf("%.2f", round(ucl,2))
or.text <- paste0(est.t, " (", lcl.t, ",", ucl.t, ")")


xlim <- c(0, 5.5)
y <- nvar:1


png("homeless.png", width=960, height=620)

par(mar=c(4, 13, 0.5, 10))
plot.new()
plot.window(xlim=xlim, ylim=c(0.5, nvar+1.5), xlab="a")
points(x=est, y=y, pch=17)
axis(1, at=seq(0.5, max(xlim), by=.5))
arrows(lcl, y, ucl, y, code=3, length=0.05, angle=90)
segments(1, 0, 1, nvar+.5,  col="grey")
mtext(var.names, at=y, adj=1, side=2, las=2)
mtext("Measure", at=nvar+1, adj=1, side=2, las=2, 
      cex=1.5, font=2)
mtext(or.text, at=y, adj=0, side=4, las=2)
mtext("OR (95% CI)", at=nvar+1, adj=0, side=4, las=2, 
      cex=1.5, font=2)

dev.off()



#### Forest plots v2 ---- 

#### \- HI ----

hi <- readxl::read_excel("Tables for Robin - Forest Plots Educ Researcher ms.xlsx", sheet=1)

# exclude constant or intercept
exclude <- "_cons"
hi <- hi %>% filter(!(hi$Variable %in% exclude))

# extract data
nvar <- NROW(hi)
est <- hi$`Odds Ratio`
lcl <- hi$`LB CI`
ucl <- hi$`UB CI`

# extract labels
var.names <- hi$Label
est.t <- sprintf("%.2f", round(est,2))
lcl.t <- sprintf("%.2f", round(lcl,2))
ucl.t <- sprintf("%.2f", round(ucl,2))
or.text <- paste0(est.t, " (", lcl.t, ",", ucl.t, ")")


xlim <- c(0, 5)
y <- nvar:1

png("housing insecurity for ms.png", width=960, height=620)

par(mar=c(4.5, 13, 0.5, 10))
plot.new()
plot.window(xlim=xlim, ylim=c(0.5, nvar+1.5))
title(xlab="Odds Ratio")
points(x=est, y=y, pch=17)
axis(1, at=seq(0.5, max(xlim), by=.5))
arrows(lcl, y, ucl, y, code=3, length=0.05, angle=90)
segments(1, 0, 1, nvar+.5,  col="grey")
mtext(var.names, at=y, adj=1, side=2, las=2)
mtext("Measure", at=nvar+1, adj=1, side=2, las=2, 
      cex=1.5, font=2)
mtext(or.text, at=y, adj=0, side=4, las=2)
mtext("OR (95% CI)", at=nvar+1, adj=0, side=4, las=2, 
      cex=1.5, font=2)

dev.off()


#### \- HM ----

hm <- readxl::read_excel("Tables for Robin - Forest Plots Educ Researcher ms.xlsx", sheet=2)

# extract data
nvar <- NROW(hm)
est <- hm$`Odds Ratio`
lcl <- hm$`LB CI`
ucl <- hm$`UB CI`

# extract labels
var.names <- hm$Label
est.t <- sprintf("%.2f", round(est,2))
lcl.t <- sprintf("%.2f", round(lcl,2))
ucl.t <- sprintf("%.2f", round(ucl,2))
or.text <- paste0(est.t, " (", lcl.t, ",", ucl.t, ")")


xlim <- c(0, 5)
y <- nvar:1


png("homeless plot for ms.png", width=960, height=620)

par(mar=c(4, 13, 0.5, 10))
plot.new()
plot.window(xlim=xlim, ylim=c(0.5, nvar+1.5))
title(xlab="Odds Ratio")
points(x=est, y=y, pch=17)
axis(1, at=seq(0.5, max(xlim), by=.5))
arrows(lcl, y, ucl, y, code=3, length=0.05, angle=90)
segments(1, 0, 1, nvar+.5,  col="grey")
mtext(var.names, at=y, adj=1, side=2, las=2)
mtext("Measure", at=nvar+1, adj=1, side=2, las=2, 
      cex=1.5, font=2)
mtext(or.text, at=y, adj=0, side=4, las=2)
mtext("OR (95% CI)", at=nvar+1, adj=0, side=4, las=2, 
      cex=1.5, font=2)

dev.off()



