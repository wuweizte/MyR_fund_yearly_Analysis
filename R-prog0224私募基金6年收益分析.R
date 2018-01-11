setwd("d:/MyR")
##
#library(stringr)

#file_input <- str_c("simujijin",2009:2014,".csv")
#A <- read.csv(file = file_input[1], header = TRUE)
#A_return <- A[,2]

##
A <- read.csv(file = "simujijin2009.csv", header = TRUE)
A_return <- A[,2]

B <- read.csv(file = "simujijin2010.csv", header = TRUE)
B_return <- B[,2]

C <- read.csv(file = "simujijin2011.csv", header = TRUE)
C_return <- C[,2]

D <- read.csv(file = "simujijin2012.csv", header = TRUE)
D_return <- D[,2]

E <- read.csv(file = "simujijin2013.csv", header = TRUE)
E_return <- E[,2]

F <- read.csv(file = "simujijin2014.csv", header = TRUE)
F_return <- F[,2]

year_sum <- c(A_return, B_return, C_return, D_return, E_return, F_return) 

op <- par(mfcol = c(2,2))

#2009~2011
plot(A_return, type = "n", ylim = c(0, 0.038),xlim = c(-63, 310), axes = FALSE,
main = '私募基金收益分布密度曲线(2009-2011)',xlab = '私募基金年收益率',ylab = '分布密度')

x_break_number <- c(min(year_sum), seq(from = -60, to = 300, by = 10),max(year_sum))
axis(1, at = x_break_number, labels = x_break_number)

axis(2, las = 1)

density_mean_sd <- function(x, lwd, lty, lcol){
  r <- density(x)
  lines(r, col = lcol, lwd = lwd, lty = lty)

  (x_mean <- mean(x))
  (x_sd <- x_mean + sd(x))
  
  (seq1 <- length(r$x[r$x < x_mean]) + 1)
  (seq2 <- length(r$x[r$x < x_sd]) + 1)
   
   lines(c(x_mean, x_mean), c(0, r$y[seq1]), col = 'red', lwd = 1, lty = lty)
   lines(c(x_sd, x_sd), c(0, r$y[seq2]), col = 'red', lwd = 1, lty = lty)

}

density_mean_sd(x = A_return, lwd = 1, lty = 3, lcol = 'blue')
density_mean_sd(x = B_return, lwd = 2, lty = 2, lcol = 'blue')
density_mean_sd(x = C_return, lwd = 3, lty = 1, lcol = 'blue')

legend("topright", legend = c(2009, 2010, 2011), col = 'blue', lty = c(3, 2, 1), lwd = c(1,2,3), inset = .02, text.width = strwidth("1"),adj = c(1,0.5))


#2012~2014
plot(D_return, type = "n", ylim = c(0, 0.048),xlim = c(-63, 310), axes = FALSE,las = 1,
main = '私募基金收益分布密度曲线(2012-2014)',xlab = '私募基金年收益率',ylab = '分布密度')

x_break_number <- c(min(year_sum), seq(from = -60, to = 300, by = 10),max(year_sum))
axis(1, at = x_break_number, labels = x_break_number)

axis(2,las = 1)

density_mean_sd(x = D_return, lwd = 1, lty = 3, lcol = 'green')
density_mean_sd(x = E_return, lwd = 2, lty = 2, lcol = 'green')
density_mean_sd(x = F_return, lwd = 3, lty = 1, lcol = 'green')

legend("topright", legend = c(2012, 2013, 2014), col = 'green', lty = c(3, 2, 1), lwd = c(1,2,3), inset = .02, text.width = strwidth("1"),adj = c(1,0.5))
