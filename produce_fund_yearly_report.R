#### Author Comment Part
# modified on 2018-1-11

#### File Descriptiong Part
# 代码目的：用于分析私募排排网提供的基金收益率年度变化信息

#### Library Quoting Part
rm(list = ls())

library(grid)
library(ggplot2)
library(RColorBrewer)

## Function Definition Part

density_median <- function(x){
        r <- density(x)
        x_median <- median(x)
        seq <- length(r$x[r$x < x_median]) + 1
        return(r$y[seq])

}

Draw_return_curve_2009_2011 <- function(){

        A <- read.csv(file = "simujijin2009.csv", header = TRUE)
        A_return <- A[,2]
        
        B <- read.csv(file = "simujijin2010.csv", header = TRUE)
        B_return <- B[,2]
        
        C <- read.csv(file = "simujijin2011.csv", header = TRUE)
        C_return <- C[,2]
        
        
        result_2009_2011 <- data.frame(return = c(C_return, B_return, A_return),
                                       year = factor(c(rep(2011,length(C_return)),
                                                       rep(2010,length(B_return)),  
                                                       rep(2009,length(A_return))),
                                                     )
                                        ) 
        
        p1 <- ggplot(result_2009_2011, aes(x = return, colour = year, fill = year)) +
                geom_density(alpha = .5) +

                scale_colour_manual(values = c("#999999","#56B4E9","#009E73")) +
                scale_fill_manual(values = c("#999999","#56B4E9","#009E73")) +
                
                annotate("segment", x = median(A_return), xend = median(A_return),
                         y = 0, yend = density_median(A_return), linetype = "dashed",
                         color = "red") +
                annotate("text", x = median(A_return),y = density_median(A_return),
                         label = paste("2009年收益率中值 = ",
                                       format(median(A_return), digits = 4), "%",
                                       sep = ""),
                         hjust = -.1, vjust = .1,size = 3.3, color = "red" ) +
                
                annotate("segment", x = median(B_return), xend = median(B_return),
                         y = 0, yend = density_median(B_return), linetype = "dashed",
                         color = "red") +
                annotate("text", x = median(B_return),y = density_median(B_return),
                         label = paste("2010年收益率中值 = ",
                                       format(median(B_return), digits = 4), "%",
                                       sep = ""),
                         hjust = -.1, size = 3.3, color = "red" ) +        
                
                annotate("segment", x = median(C_return), xend = median(C_return),
                         y = 0, yend = density_median(C_return), linetype = "dashed",
                         color = "red") +
                annotate("text", x = median(C_return),y = density_median(C_return),
                         label = paste("2011年收益率中值 = ",
                                       format(median(C_return), digits = 4), "%",
                                       sep = ""),
                         hjust = 1, vjust = -1, size = 3.3, color = "red" ) +
                
                ggtitle("         私募基金收益分布密度曲线（2009 - 2011）") +
                theme(plot.title = element_text(size = 20)) +
                
                xlab("私募基金年收益率") +
                ylab("分布密度") +
                coord_cartesian(xlim = c(-70, 160), ylim = c(0, 0.055)) +
                
                annotate("segment", x = 60, xend = 10, y = 0.05, yend = 0.05,
                          linetype = 5, size = 1,color = "orange", arrow = arrow())
                          
        
        return(p1)

}
        

# Draw_return_curve_2011_2015 <- function(){
# 
#         C <- read.csv(file = "simujijin2011.csv", header = TRUE)
#         C_return <- C[,2]
#         
#         D <- read.csv(file = "simujijin2012.csv", header = TRUE)
#         D_return <- D[,2]
#         
#         E <- read.csv(file = "simujijin2013.csv", header = TRUE)
#         E_return <- E[,2]
#         
#         F <- read.csv(file = "simujijin2014.csv", header = TRUE)
#         F_return <- F[,2]
#         
#         G <- read.csv(file = "simujijin2015.csv", header = TRUE)
#         G_return <- G[,2]
#         
#         result_2011_2015 <- data.frame(return = c(C_return, D_return, E_return, 
#                                                   F_return, G_return),
#                                        year = factor(c(rep(2011,length(C_return)),
#                                                        rep(2012,length(D_return)),
#                                                        rep(2013,length(E_return)),
#                                                        rep(2014,length(F_return)),
#                                                        rep(2015,length(G_return))))
#         ) 
#         p2 <- ggplot(result_2011_2015, aes(x = return,colour = year, fill = year)) +
#                 geom_density(alpha = .5) +
#                 # scale_colour_brewer(palette = "Dark2") +
#                 # scale_fill_brewer(palette = "Dark2") +
# 
#                 scale_colour_manual(values = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00")) +
#                 scale_fill_manual(values = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00")) +
#                 
#                                 
#                 annotate("segment", x = median(C_return), xend = median(C_return),
#                          y = 0, yend = density_median(C_return), linetype = "dashed",
#                          color = "blue") +
#                 annotate("text", x = median(C_return),y = density_median(C_return),
#                          label = paste("2011年收益率中值 = ",
#                                        format(median(C_return), digits = 4), "%",
#                                        sep = ""),
#                          hjust = 1,vjust = -.5, size = 3.3, color = "blue" ) +
#                 
#                 annotate("segment", x = median(D_return), xend = median(D_return),
#                          y = 0, yend = density_median(D_return), linetype = "dashed",
#                          color = "blue") +
#                 annotate("text", x = median(D_return),y = density_median(D_return),
#                          label = paste("2012年收益率中值 = ",
#                                        format(median(D_return), digits = 4), "%",
#                                        sep = ""),
#                          hjust = 1,vjust = -.5, size = 3.3, color = "blue" ) +        
#                 
#                 annotate("segment", x = median(E_return), xend = median(E_return),
#                          y = 0, yend = density_median(E_return), linetype = "dashed",
#                          color = "blue") +
#                 annotate("text", x = median(E_return),y = density_median(E_return),
#                          label = paste("2013年收益率中值 = ",
#                                        format(median(E_return), digits = 4), "%",
#                                        sep = ""),
#                          hjust = 0, vjust = -2, size = 3.3, color = "blue" ) +
#                 
#                 annotate("segment", x = median(F_return), xend = median(F_return),
#                          y = 0, yend = density_median(F_return), linetype = "dashed",
#                          color = "blue") +
#                 annotate("text", x = median(F_return),y = density_median(F_return),
#                          label = paste("2014年收益率中值 = ",
#                                        format(median(F_return), digits = 4), "%",
#                                        sep = ""),
#                          hjust = -.1, vjust = 0.7, size = 3.3, color = "blue" ) +
#                 
#                 annotate("segment", x = median(G_return), xend = median(G_return),
#                          y = 0, yend = density_median(G_return), linetype = "dashed",
#                          color = "blue") +
#                 annotate("text", x = median(G_return),y = density_median(G_return),
#                          label = paste("2015年收益率中值 = ",
#                                        format(median(G_return), digits = 4), "%",
#                                        sep = ""),
#                          hjust = -.2, vjust = 0, size = 3.3, color = "blue" ) +
#                 
#                 
#                 ggtitle("         私募基金收益分布密度曲线（2011 - 2015）") +
#                 theme(plot.title = element_text(size = 20)) +
#                 
#                 xlab("私募基金年收益率") +
#                 ylab("分布密度") +
#                 coord_cartesian(xlim = c(-70, 160), ylim = c(0, 0.055)) +
#                 
#                 annotate("segment", x = 10, xend = 60, y = 0.05, yend = 0.05,
#                          linetype = 5, size = 0.8,color = "purple", arrow = arrow())
#         
#         return(p2)
# }

Draw_return_curve_2015_2016 <- function(){
        

        G <- read.csv(file = "simujijin2015.csv", header = TRUE)
        G_return <- G[,2]

        H <- read.csv(file = "simujijin2016.csv", header = TRUE)
        H_return <- H[,2]
        
        result_2015_2016 <- data.frame(return = c(G_return, H_return),
                                       year = factor(c(rep(2015,length(G_return)),
                                                       rep(2016,length(H_return))))
        ) 
        p2 <- ggplot(result_2015_2016, aes(x = return,colour = year, fill = year)) +
                geom_density(alpha = .5) +
                
                scale_colour_manual(values = c("#D55E00", "#009E73")) +
                scale_fill_manual(values = c("#D55E00", "#009E73")) +
                
                annotate("segment", x = median(G_return), xend = median(G_return),
                         y = 0, yend = density_median(G_return), linetype = "dashed",
                         color = "red") +
                annotate("text", x = median(G_return),y = density_median(G_return),
                         label = paste("2015年收益率中值 = ",
                                       round(median(G_return), digits = 2), "%",
                                       sep = ""),
                         hjust = -.2, vjust = 0, size = 3.3, color = "red" ) +

                annotate("segment", x = median(H_return), xend = median(H_return),
                         y = 0, yend = density_median(H_return), linetype = "dashed",
                         color = "red") +
                annotate("text", x = median(H_return),y = density_median(H_return),
                         label = paste("2016年收益率中值 = ",
                                       round(median(H_return), digits = 2), "%",
                                       sep = ""),
                         hjust = -.2, vjust = 0, size = 3.3, color = "red" ) +
                
                
                ggtitle("         私募基金收益分布密度曲线（2015 - 2016）") +
                theme(plot.title = element_text(size = 20)) +
                
                xlab("私募基金年收益率") +
                ylab("分布密度") +
                coord_cartesian(xlim = c(-70, 160), ylim = c(0, 0.055)) +
                
                annotate("segment", x = 60, xend = 10, y = 0.05, yend = 0.05,
                         linetype = 5, size = 1,color = "orange", arrow = arrow())
        
        return(p2)
}

# Draw_return_curve_2016_2017 <- function(){
#         
#         
#         G <- read.csv(file = "simujijin2016.csv", header = TRUE)
#         G_return <- G[,2]
#         
#         H <- read.csv(file = "simujijin2017.csv", header = TRUE)
#         H_return <- H[,2]
#         
#         result_2016_2017 <- data.frame(return = c(G_return, H_return),
#                                        year = factor(c(rep(2016,length(G_return)),
#                                                        rep(2017,length(H_return))))
#         ) 
#         p2 <- ggplot(result_2016_2017, aes(x = return,colour = year, fill = year)) +
#                 geom_density(alpha = .5) +
#                 
#                 scale_colour_manual(values = c("#009E73","#CC79A7")) +
#                 scale_fill_manual(values = c("#009E73","#CC79A7")) +
#                 
#                 annotate("segment", x = median(G_return), xend = median(G_return),
#                          y = 0, yend = density_median(G_return), linetype = "dashed",
#                          color = "red") +
#                 annotate("text", x = median(G_return),y = density_median(G_return),
#                          label = paste("2016年收益率中值 = ",
#                                        round(median(G_return), digits = 2), "%",
#                                        sep = ""),
#                          hjust = -.2, vjust = 0, size = 3.3, color = "red" ) +
#                 
#                 annotate("segment", x = median(H_return), xend = median(H_return),
#                          y = 0, yend = density_median(H_return), linetype = "dashed",
#                          color = "red") +
#                 annotate("text", x = median(H_return),y = density_median(H_return),
#                          label = paste("2017年收益率中值 = ",
#                                        round(median(H_return), digits = 2), "%",
#                                        sep = ""),
#                          hjust = -.2, vjust = 0, size = 3.3, color = "red" ) +
#                 
#                 
#                 ggtitle("         私募基金收益分布密度曲线（2016 - 2017）") +
#                 theme(plot.title = element_text(size = 20)) +
#                 
#                 xlab("私募基金年收益率") +
#                 ylab("分布密度") +
#                 coord_cartesian(xlim = c(-70, 160), ylim = c(0, 0.055)) +
#                 
#                 annotate("segment", x = 10, xend = 60, y = 0.05, yend = 0.05,
#                          linetype = 5, size = 0.8,color = "purple", arrow = arrow())
#         
#         return(p2)
# }

Draw_return_curve <- function(arg.year, 
                              arg.ylim,
                              arg.hjust,
                              arg.vjust){

        for(i in 1:length(arg.year)){

             csv.file.name <- paste0("simujijin", arg.year[i],".csv")
             fund.return <- read.csv(file = csv.file.name, header = TRUE)[,2]
             if(i == 1){
             
                     result <- data.frame(return = fund.return,
                                          year = c(rep(arg.year[i],length(fund.return)))
                     )         
             }else{
                     result <- rbind(result,
                                     data.frame(return = fund.return,
                                          year = c(rep(arg.year[i],length(fund.return)))))
             }
              
        }
        result$year <- as.factor(result$year)
        
        color.group <- c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00")
        
        p2 <- ggplot(result, aes(x = return,colour = year, fill = year)) +
                geom_density(alpha = .5) +
                
                scale_colour_manual(values = head(color.group, length(arg.year))) +
                scale_fill_manual(values = head(color.group, length(arg.year)))
        
        for(i in 1:length(arg.year)){
                
                return <- result[result$year == arg.year[i],"return"]                
                
                p2 <- p2 + 
                        annotate("segment", x = median(return), xend = median(return),
                                 y = 0, yend = density_median(return), linetype = "dashed",
                                 color = "red") +
                        
                        annotate("text", x = median(return),y = density_median(return),
                                 label = paste(arg.year[i],"年收益率中值 = ",
                                               round(median(return), digits = 2), "%",
                                               sep = ""),
                                 hjust = arg.hjust[i], 
                                 vjust = arg.vjust[i], 
                                 size = 3.3, color = "red" ) 
                        
                        
        }           
        
                
        p2 <- p2 + ggtitle(paste0("         私募基金收益分布密度曲线（",
                                  head(arg.year,1), "-", 
                                  tail(arg.year,1),"）")) +
                
                theme(plot.title = element_text(size = 20)) +
                
                xlab("私募基金年收益率") +
                ylab("分布密度") +
                coord_cartesian(xlim = c(-70, 160), ylim = arg.ylim) +
                
                annotate("segment", 
                         x = 10, xend = 60, 
                         y = max(arg.ylim) - 0.005, yend = max(arg.ylim) - 0.005,
                         linetype = 5, size = 0.8,color = "purple", arrow = arrow())
        
        return(p2)
}



Draw_board_index_2009_2011 <- function(){
        
        A <- read.csv(file = "zhishu.csv", header = TRUE)
        levels(A[["zhishu"]]) <- list(上证综指 = "shangzheng", 
                                          深证成指 = "shenzheng", 
                                          创业板 = "chuangye",
                                          港股通精选100 = "ganggutong")
        
        
        A_09_11 <- subset(A, year <= 2011)[,c(1,2,5)]
        A_09_11[["year"]] <- factor(A_09_11[["year"]])
        
        p3 <- ggplot(A_09_11, aes(x = zhishu, y = zhangfu, color = year,fill = year)) +
                geom_bar(position = "dodge", stat = "identity", alpha = .5) +
                scale_colour_manual(values = c("#999999","#56B4E9","#009E73")) +
                scale_fill_manual(values = c("#999999","#56B4E9","#009E73")) +
                
                geom_text(aes(label = paste(format(zhangfu,nsmall = 1),"%",sep = ""),
                              vjust = -0.9 * sign(zhangfu)),
                          position = position_dodge(.9),size = 3,
                          colour = "black") +
                xlab("") +
                ylab("涨幅(%)") +
                ggtitle("         板块指数涨幅（2009 - 2011）")  +
                theme(plot.title = element_text(size = 20))  +
                
                coord_cartesian(ylim = c(-45, 125)) 
        
        return(p3)
        
}

Draw_board_index_2011_2015 <- function(){
        
        A <- read.csv(file = "zhishu.csv", header = TRUE)
        levels(A[["zhishu"]]) <- list(上证综指 = "shangzheng", 
                                          深证成指 = "shenzheng", 
                                          创业板 = "chuangye",
                                          港股通精选100 = "ganggutong")
        
        
        A_11_15 <- subset(A, year %in% (2011:2015))[,c(1,2,5)]
        A_11_15[["year"]] <- factor(A_11_15[["year"]])
        
        p4 <- ggplot(A_11_15, aes(x = zhishu, y = zhangfu, colour = year, fill = year)) +
                geom_bar(position = "dodge", stat = "identity",alpha = .5) +
                # scale_colour_brewer(palette = "Dark2") +
                # scale_fill_brewer(palette = "Dark2") +
                
                scale_colour_manual(values = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00")) +
                scale_fill_manual(values = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00")) +
                
                                
                geom_text(aes(label = paste(format(zhangfu,digits = 2),"%",sep = ""),
                              vjust = -1 * sign(zhangfu)),
                          position = position_dodge(.9),size = 3,
                          colour = "black", hjust = 0.4) +
                xlab("") +
                ylab("涨幅(%)") +
                ggtitle("         板块指数涨幅（2011 - 2015）")  +
                theme(plot.title = element_text(size = 20)) +
                
                coord_cartesian(ylim = c(-45, 100))
                                
        return(p4)
        
}

Draw_board_index_2015_2016 <- function(){
        
        A <- read.csv(file = "zhishu.csv", header = TRUE)
        levels(A[["zhishu"]]) <- list(上证综指 = "shangzheng", 
                                          深证成指 = "shenzheng", 
                                          创业板 = "chuangye",
                                          港股通精选100 = "ganggutong")
        
        
        A_15_16 <- subset(A, year %in% c(2015:2016))[,c(1,2,5)]
        A_15_16[["year"]] <- factor(A_15_16[["year"]])
        
        p4 <- ggplot(A_15_16, aes(x = zhishu, y = zhangfu, colour = year, fill = year)) +
                geom_bar(position = "dodge", stat = "identity",alpha = .5) +

                scale_colour_manual(values = c("#D55E00", "#009E73")) +
                scale_fill_manual(values = c("#D55E00", "#009E73")) +
                
                geom_text(aes(label = paste(format(zhangfu,digits = 2),"%",sep = ""),
                              vjust = -1 * sign(zhangfu)),
                          position = position_dodge(.9),size = 3,
                          colour = "black", hjust = 0.4) +
                xlab("") +
                ylab("涨幅(%)") +
                ggtitle("         板块指数涨幅（2015 - 2016）")  +
                theme(plot.title = element_text(size = 20)) +
                
                coord_cartesian(ylim = c(-45, 100))
        
        return(p4)
        
}

Draw_board_index_2016_2017 <- function(){
        
        A <- read.csv(file = "zhishu.csv", header = TRUE)
        levels(A[["zhishu"]]) <- list(上证综指 = "shangzheng", 
                                          深证成指 = "shenzheng", 
                                          创业板 = "chuangye",
                                          港股通精选100 = "ganggutong")
        
        
        A_16_17 <- subset(A, year %in% c(2016:2017))[,c(1,2,5)]
        A_16_17[["year"]] <- factor(A_16_17[["year"]])
        
        p4 <- ggplot(A_16_17, aes(x = zhishu, y = zhangfu, colour = year, fill = year)) +
                geom_bar(position = "dodge", stat = "identity",alpha = .5) +
                
                scale_colour_manual(values = c("#009E73","#CC79A7")) +
                scale_fill_manual(values = c("#009E73","#CC79A7")) +
                
                geom_text(aes(label = paste(format(zhangfu,digits = 2),"%",sep = ""),
                              vjust = -1 * sign(zhangfu)),
                          position = position_dodge(.9),size = 3,
                          colour = "black", hjust = 0.4) +
                xlab("") +
                ylab("涨幅(%)") +
                ggtitle("         板块指数涨幅（2016 - 2017）")  +
                theme(plot.title = element_text(size = 20)) +
                
                coord_cartesian(ylim = c(-45, 100))
        
        return(p4)
        
}


## Execution Part

setwd("D:/MyR/jijin_yearlyAnalysis")


p1 <- Draw_return_curve_2009_2011()
# p2 <- Draw_return_curve_2011_2015()
p2 <- Draw_return_curve(arg.year = 2011:2015, 
                        arg.ylim = c(0, 0.065),
                        arg.hjust = c( 1,   1,  0, -0.1, -0.2),
                        arg.vjust = c(-1,-0.5, -2,  0.7,    0))

p3 <- Draw_board_index_2009_2011()
p4 <- Draw_board_index_2011_2015()


p5 <- Draw_return_curve_2015_2016()
p6 <- Draw_return_curve(arg.year = 2016:2017, 
                        arg.ylim = c(0, 0.055),
                        arg.hjust = c( -0.2, -0.2),
                        arg.vjust = c(    0,    0))

p7 <- Draw_board_index_2015_2016()
p8 <- Draw_board_index_2016_2017()

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 5)))
# vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(p1, vp = vplayout(1, 1:3))
# print(p2, vp = vplayout(2, 1:3))
# 
# print(p3, vp = vplayout(1, 4:5))
# print(p4, vp = vplayout(2, 4:5))
# 

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 5)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p5, vp = vplayout(1, 1:3))
print(p6, vp = vplayout(2, 1:3))

print(p7, vp = vplayout(1, 4:5))
print(p8, vp = vplayout(2, 4:5))



