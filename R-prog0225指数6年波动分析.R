#setwd("d:/MyR")

A <- read.csv(file = "zhishu2009_2014.csv", header = TRUE)
A_09_11 <- subset(A, year < 2012)[,c(1,2,5)]
A_12_14 <- subset(A, year >= 2012)[,c(1,2,5)]

A_09_11_array <- array(c(A_09_11[,3][1:6],NA, A_09_11[,3][7:11] ), dim = c(3, 4))
dimnames(A_09_11_array) <- list(c("2009","2010","2011"),c("上证综指","深证成指","创业板","中小板"))
bar_x <- barplot(A_09_11_array, beside = TRUE, main = "板块指数涨幅(2009-2011)",
        density = c(20,40,60), col = "blue", las = 1,
        legend.text = attr(A_09_11_array, "dimnames")[[1]],args.legend = list(x = "top", text.width = strwidth("1"),adj = c(1,0.5)))
bar_y <- c(A_09_11[,3][1:6],NA, A_09_11[,3][7:11] )
text(bar_x, bar_y + sign(bar_y)*6, format(paste(bar_y, "%")), xpd = TRUE, col = "red")


A_12_14_array <- array(c(A_12_14[,3] ), dim = c(3, 4))
dimnames(A_12_14_array) <- list(c("2012","2013","2014"),c("上证综指","深证成指","创业板","中小板"))
bar_x <- barplot(A_12_14_array, beside = TRUE, main = "板块指数涨幅(2012-2014)",
        density = c(20,40,60), col = "green", las = 1,
        legend.text = attr(A_12_14_array, "dimnames")[[1]],args.legend = list(x = "topright", text.width = strwidth("1"),adj = c(1,0.5)))
bar_y <- A_12_14[,3]
text(bar_x, bar_y + sign(bar_y)*4, format(paste(bar_y, "%")), xpd = TRUE, col = "red")

par(op)
