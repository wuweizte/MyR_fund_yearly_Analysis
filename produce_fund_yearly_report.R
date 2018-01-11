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


Draw_return_curve <- function(arg.year, 
                              arg.ylim,
                              arg.hjust,
                              arg.vjust, 
                              arg.direction,
                              arg.colorgroup){


        ### read data files
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
        
        ### draw graph
        p2 <- ggplot(result, aes(x = return,colour = year, fill = year)) +
                geom_density(alpha = .5) +
                scale_colour_manual(values = arg.colorgroup) +
                        scale_fill_manual(values = arg.colorgroup)

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
                coord_cartesian(xlim = c(-70, 160), ylim = arg.ylim) 

        ### draw arrow
        if(arg.direction == "up"){
                
                p2 <- p2 +  annotate("segment", 
                                            x = 10, xend = 60, 
                                            y = max(arg.ylim) - 0.005, yend = max(arg.ylim) - 0.005,
                                            linetype = 5, size = 0.8,color = "purple", arrow = arrow())
        }else{
                p2 <- p2 + annotate("segment", 
                                            x = 60, xend = 10, 
                                            y = max(arg.ylim) - 0.005, yend = max(arg.ylim) - 0.005,
                                            linetype = 5, size = 0.8,color = "orange", arrow = arrow())
                
        }
        return(p2)
}


Draw_board_index <- function(arg.year, 
                              arg.ylim,
                              arg.direction,
                             arg.colorgroup){
        

        A <- read.csv(file = "zhishu.csv", header = TRUE)
        levels(A[["zhishu"]]) <- list(上证综指 = "shangzheng", 
                                          深证成指 = "shenzheng", 
                                          创业板 = "chuangye",
                                          港股通精选100 = "ganggutong")
        
        
        result <- subset(A, year %in% c(arg.year))[,c(1,2,5)]
        result[["year"]] <- factor(result[["year"]])
        

        p <- ggplot(result, aes(x = zhishu, y = zhangfu, colour = year, fill = year)) +
                geom_bar(position = "dodge", stat = "identity",alpha = .5) 
                p <- p + scale_colour_manual(values = arg.colorgroup) +
                        scale_fill_manual(values = arg.colorgroup)

        p <- p + geom_text(aes(label = paste(format(zhangfu,digits = 2),"%",sep = ""),
                              vjust = -1 * sign(zhangfu)),
                          position = position_dodge(.9),size = 3,
                          colour = "black", hjust = 0.4) +
                xlab("") +
                ylab("涨幅(%)") +
                ggtitle(paste0("         板块指数涨幅（",
                        head(arg.year,1), "-", 
                        tail(arg.year,1),"）"))  +
                theme(plot.title = element_text(size = 20)) +

                coord_cartesian(ylim = arg.ylim)
        return(p)
}




## Execution Part

setwd("D:/MyR/jijin_yearlyAnalysis")


################### the 1st down-up cycle
p1 <- Draw_return_curve(arg.year = 2009:2011, 
                        arg.ylim = c(0, 0.055),
                        arg.hjust = c( -0.1,-0.1,  1),
                        arg.vjust = c(  0.1,   0, -1),
                        arg.direction = "down",
                        arg.colorgroup = c("#999999","#56B4E9","#009E73"))

p2 <- Draw_return_curve(arg.year = 2011:2015, 
                        arg.ylim = c(0, 0.065),
                        arg.hjust = c( 1,   1,  0, -0.1, -0.2),
                        arg.vjust = c(-1,-0.5, -2,  0.7,    0),
                        arg.direction = "up",
                        arg.colorgroup = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00"))

p3 <- Draw_board_index(arg.year = 2009:2011,
                       arg.ylim = c(-45, 125),
                       arg.direction = "down",
                       arg.colorgroup = c("#999999","#56B4E9","#009E73"))

p4 <- Draw_board_index(arg.year = 2011:2015,
                       arg.ylim = c(-45, 100),
                       arg.direction = "up",
                       arg.colorgroup = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00"))

###################### the 2nd down-up cycle

p5 <- Draw_return_curve(arg.year = 2015:2016, 
                        arg.ylim = c(0, 0.055),
                        arg.hjust = c( -0.2, -0.2),
                        arg.vjust = c(    0,    0),
                        arg.direction = "down",
                        arg.colorgroup = c("#999999","#009E73"))

p6 <- Draw_return_curve(arg.year = 2016:2017, 
                        arg.ylim = c(0, 0.055),
                        arg.hjust = c( -0.2, -0.2),
                        arg.vjust = c(    0,    0),
                        arg.direction = "up",
                        arg.colorgroup = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00"))

p7 <- Draw_board_index(arg.year = 2015:2016,
                       arg.ylim = c(-45, 100),
                       arg.direction = "down",
                       arg.colorgroup = c("#999999","#009E73"))

### if the mean of 2018 is increasing , change 2017 to 2018 in the following code
p8 <- Draw_board_index(arg.year = 2016:2017,
                       arg.ylim = c(-45, 100),
                       arg.direction = "up",
                       arg.colorgroup = c("#009E73","#CC79A7", "#E69F00", "forestgreen", "#D55E00"))

####################################   show the 1st down-up cycle
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 7)))
# vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(p1, vp = vplayout(1, 1:4))
# print(p2, vp = vplayout(2, 1:4))
# 
# print(p3, vp = vplayout(1, 5:7))
# print(p4, vp = vplayout(2, 5:7))
# 

###################################   show the 2nd down-up cycle
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 7)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p5, vp = vplayout(1, 1:4))
print(p6, vp = vplayout(2, 1:4))

print(p7, vp = vplayout(1, 5:7))
print(p8, vp = vplayout(2, 5:7))



