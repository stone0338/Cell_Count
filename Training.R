library(magick)
library(rtiff)
library(reshape2)
library(neuralnet)

file_train_yes <- c("yes_1.tif",
                    "yes_2.tif",
                    "yes_3.tif",
                    "yes_4.tif",
                    "yes_5.tif",
                    "yes_6.tif",
                    "yes_7.tif",
                    "yes_8.tif")

file_train_no <- c("no_1.tif",
                   "no_2.tif",
                   "no_3.tif",
                   "no_4.tif",
                   "no_5.tif",
                   "no_6.tif",
                   "no_7.tif",
                   "no_8.tif")

# Train Positive Data
data_train_yes_sum <- data.frame()
for (i in 1:10) {
image_train_yes <- readTiff(paste("Training_Images", file_train_yes[1], sep="/"))

red <- image_train_yes@red
red_L <- cbind(NA,red[,-ncol(red)])
red_R <- cbind(red[,-1],NA)
red_T <- rbind(NA,red[-nrow(red),])
red_B <- rbind(red[-1,],NA)
red_LT <- rbind(NA, red_L[-nrow(red_L),])
red_LB <- rbind(red_L[-1,],NA)
red_RT <- rbind(NA, red_R[-nrow(red_R),])
red_RB <- rbind(red_R[-1,],NA)
red_melt <- melt(red, varnames = c("row","col"), value.name = "red")
red_L_melt <- melt(red_L, varnames = c("row","col"), value.name = "red")
red_R_melt <- melt(red_R, varnames = c("row","col"), value.name = "red")
red_T_melt <- melt(red_T, varnames = c("row","col"), value.name = "red")
red_B_melt <- melt(red_B, varnames = c("row","col"), value.name = "red")
red_LT_melt <- melt(red_LT, varnames = c("row","col"), value.name = "red")
red_LB_melt <- melt(red_LB, varnames = c("row","col"), value.name = "red")
red_RT_melt <- melt(red_RT, varnames = c("row","col"), value.name = "red")
red_RB_melt <- melt(red_RB, varnames = c("row","col"), value.name = "red")
red_all_neighbors <- cbind(red_melt, red_L_melt, red_R_melt, red_T_melt, red_B_melt, red_LT_melt, red_LB_melt,red_RT_melt,red_RB_melt)
red_all_neighbors <- red_all_neighbors[!duplicated(as.list(red_all_neighbors))]
red_neighbor_average <- data.frame(red_all_neighbors[,1:2], red_neighbors_mean = rowMeans(red_all_neighbors[3:11], na.rm = TRUE))

green <- image_train_yes@green
green_L <- cbind(NA,green[,-ncol(green)])
green_R <- cbind(green[,-1],NA)
green_T <- rbind(NA,green[-nrow(green),])
green_B <- rbind(green[-1,],NA)
green_LT <- rbind(NA, green_L[-nrow(green_L),])
green_LB <- rbind(green_L[-1,],NA)
green_RT <- rbind(NA, green_R[-nrow(green_R),])
green_RB <- rbind(green_R[-1,],NA)
green_melt <- melt(green, varnames = c("row","col"), value.name = "green")
green_L_melt <- melt(green_L, varnames = c("row","col"), value.name = "green")
green_R_melt <- melt(green_R, varnames = c("row","col"), value.name = "green")
green_T_melt <- melt(green_T, varnames = c("row","col"), value.name = "green")
green_B_melt <- melt(green_B, varnames = c("row","col"), value.name = "green")
green_LT_melt <- melt(green_LT, varnames = c("row","col"), value.name = "green")
green_LB_melt <- melt(green_LB, varnames = c("row","col"), value.name = "green")
green_RT_melt <- melt(green_RT, varnames = c("row","col"), value.name = "green")
green_RB_melt <- melt(green_RB, varnames = c("row","col"), value.name = "green")
green_all_neighbors <- cbind(green_melt, green_L_melt, green_R_melt, green_T_melt, green_B_melt, green_LT_melt, green_LB_melt,green_RT_melt,green_RB_melt)
green_all_neighbors <- green_all_neighbors[!duplicated(as.list(green_all_neighbors))]
green_neighbor_average <- data.frame(green_all_neighbors[,1:2], green_neighbors_mean = rowMeans(green_all_neighbors[3:11], na.rm = TRUE))

blue <- image_train_yes@blue
blue_L <- cbind(NA,blue[,-ncol(blue)])
blue_R <- cbind(blue[,-1],NA)
blue_T <- rbind(NA,blue[-nrow(blue),])
blue_B <- rbind(blue[-1,],NA)
blue_LT <- rbind(NA, blue_L[-nrow(blue_L),])
blue_LB <- rbind(blue_L[-1,],NA)
blue_RT <- rbind(NA, blue_R[-nrow(blue_R),])
blue_RB <- rbind(blue_R[-1,],NA)
blue_melt <- melt(blue, varnames = c("row","col"), value.name = "blue")
blue_L_melt <- melt(blue_L, varnames = c("row","col"), value.name = "blue")
blue_R_melt <- melt(blue_R, varnames = c("row","col"), value.name = "blue")
blue_T_melt <- melt(blue_T, varnames = c("row","col"), value.name = "blue")
blue_B_melt <- melt(blue_B, varnames = c("row","col"), value.name = "blue")
blue_LT_melt <- melt(blue_LT, varnames = c("row","col"), value.name = "blue")
blue_LB_melt <- melt(blue_LB, varnames = c("row","col"), value.name = "blue")
blue_RT_melt <- melt(blue_RT, varnames = c("row","col"), value.name = "blue")
blue_RB_melt <- melt(blue_RB, varnames = c("row","col"), value.name = "blue")
blue_all_neighbors <- cbind(blue_melt, blue_L_melt, blue_R_melt, blue_T_melt, blue_B_melt, blue_LT_melt, blue_LB_melt,blue_RT_melt,blue_RB_melt)
blue_all_neighbors <- blue_all_neighbors[!duplicated(as.list(blue_all_neighbors))]
blue_neighbor_average <- data.frame(blue_all_neighbors[,1:2], blue_neighbors_mean = rowMeans(blue_all_neighbors[3:11], na.rm = TRUE))

data_train_yes <- cbind(red_melt,red_neighbor_average,green_melt,green_neighbor_average,blue_melt,blue_neighbor_average)
data_train_yes <- data_train_yes[!duplicated(as.list(data_train_yes))]

data_train_yes$count <- 1
data_train_yes_sum <- rbind(data_train_yes_sum,data_train_yes)
}


# Train Negative Data
data_train_no_sum <- data.frame()
for (x in 1:10) {
image_train_no <- readTiff(paste("Training_Images", file_train_no[1], sep="/"))

red <- image_train_no@red
red_L <- cbind(NA,red[,-ncol(red)])
red_R <- cbind(red[,-1],NA)
red_T <- rbind(NA,red[-nrow(red),])
red_B <- rbind(red[-1,],NA)
red_LT <- rbind(NA, red_L[-nrow(red_L),])
red_LB <- rbind(red_L[-1,],NA)
red_RT <- rbind(NA, red_R[-nrow(red_R),])
red_RB <- rbind(red_R[-1,],NA)
red_melt <- melt(red, varnames = c("row","col"), value.name = "red")
red_L_melt <- melt(red_L, varnames = c("row","col"), value.name = "red")
red_R_melt <- melt(red_R, varnames = c("row","col"), value.name = "red")
red_T_melt <- melt(red_T, varnames = c("row","col"), value.name = "red")
red_B_melt <- melt(red_B, varnames = c("row","col"), value.name = "red")
red_LT_melt <- melt(red_LT, varnames = c("row","col"), value.name = "red")
red_LB_melt <- melt(red_LB, varnames = c("row","col"), value.name = "red")
red_RT_melt <- melt(red_RT, varnames = c("row","col"), value.name = "red")
red_RB_melt <- melt(red_RB, varnames = c("row","col"), value.name = "red")
red_all_neighbors <- cbind(red_melt, red_L_melt, red_R_melt, red_T_melt, red_B_melt, red_LT_melt, red_LB_melt,red_RT_melt,red_RB_melt)
red_all_neighbors <- red_all_neighbors[!duplicated(as.list(red_all_neighbors))]
red_neighbor_average <- data.frame(red_all_neighbors[,1:2], red_neighbors_mean = rowMeans(red_all_neighbors[3:11], na.rm = TRUE))

green <- image_train_no@green
green_L <- cbind(NA,green[,-ncol(green)])
green_R <- cbind(green[,-1],NA)
green_T <- rbind(NA,green[-nrow(green),])
green_B <- rbind(green[-1,],NA)
green_LT <- rbind(NA, green_L[-nrow(green_L),])
green_LB <- rbind(green_L[-1,],NA)
green_RT <- rbind(NA, green_R[-nrow(green_R),])
green_RB <- rbind(green_R[-1,],NA)
green_melt <- melt(green, varnames = c("row","col"), value.name = "green")
green_L_melt <- melt(green_L, varnames = c("row","col"), value.name = "green")
green_R_melt <- melt(green_R, varnames = c("row","col"), value.name = "green")
green_T_melt <- melt(green_T, varnames = c("row","col"), value.name = "green")
green_B_melt <- melt(green_B, varnames = c("row","col"), value.name = "green")
green_LT_melt <- melt(green_LT, varnames = c("row","col"), value.name = "green")
green_LB_melt <- melt(green_LB, varnames = c("row","col"), value.name = "green")
green_RT_melt <- melt(green_RT, varnames = c("row","col"), value.name = "green")
green_RB_melt <- melt(green_RB, varnames = c("row","col"), value.name = "green")
green_all_neighbors <- cbind(green_melt, green_L_melt, green_R_melt, green_T_melt, green_B_melt, green_LT_melt, green_LB_melt,green_RT_melt,green_RB_melt)
green_all_neighbors <- green_all_neighbors[!duplicated(as.list(green_all_neighbors))]
green_neighbor_average <- data.frame(green_all_neighbors[,1:2], green_neighbors_mean = rowMeans(green_all_neighbors[3:11], na.rm = TRUE))

blue <- image_train_no@blue
blue_L <- cbind(NA,blue[,-ncol(blue)])
blue_R <- cbind(blue[,-1],NA)
blue_T <- rbind(NA,blue[-nrow(blue),])
blue_B <- rbind(blue[-1,],NA)
blue_LT <- rbind(NA, blue_L[-nrow(blue_L),])
blue_LB <- rbind(blue_L[-1,],NA)
blue_RT <- rbind(NA, blue_R[-nrow(blue_R),])
blue_RB <- rbind(blue_R[-1,],NA)
blue_melt <- melt(blue, varnames = c("row","col"), value.name = "blue")
blue_L_melt <- melt(blue_L, varnames = c("row","col"), value.name = "blue")
blue_R_melt <- melt(blue_R, varnames = c("row","col"), value.name = "blue")
blue_T_melt <- melt(blue_T, varnames = c("row","col"), value.name = "blue")
blue_B_melt <- melt(blue_B, varnames = c("row","col"), value.name = "blue")
blue_LT_melt <- melt(blue_LT, varnames = c("row","col"), value.name = "blue")
blue_LB_melt <- melt(blue_LB, varnames = c("row","col"), value.name = "blue")
blue_RT_melt <- melt(blue_RT, varnames = c("row","col"), value.name = "blue")
blue_RB_melt <- melt(blue_RB, varnames = c("row","col"), value.name = "blue")
blue_all_neighbors <- cbind(blue_melt, blue_L_melt, blue_R_melt, blue_T_melt, blue_B_melt, blue_LT_melt, blue_LB_melt,blue_RT_melt,blue_RB_melt)
blue_all_neighbors <- blue_all_neighbors[!duplicated(as.list(blue_all_neighbors))]
blue_neighbor_average <- data.frame(blue_all_neighbors[,1:2], blue_neighbors_mean = rowMeans(blue_all_neighbors[3:11], na.rm = TRUE))

data_train_no <- cbind(red_melt,red_neighbor_average,green_melt,green_neighbor_average,blue_melt,blue_neighbor_average)
data_train_no <- data_train_no[!duplicated(as.list(data_train_no))]

data_train_no$count <- 0
data_train_no_sum <- rbind(data_train_no_sum,data_train_no)
}

data_train <- rbind(data_train_yes_sum, data_train_no_sum)
data_train <- data_train[,c("row",
                            "col",
                            "red_neighbors_mean",
                            "green_neighbors_mean",
                            "blue_neighbors_mean",
                            "count")]

#data_train$red_norm <- data_train$red/(data_train$red+data_train$green+data_train$blue)
#data_train$green_norm <- data_train$green/(data_train$red+data_train$green+data_train$blue)
#data_train$blue_norm <- data_train$blue/(data_train$red+data_train$green+data_train$blue)

nn1 <- neuralnet(count ~ red_neighbors_mean + green_neighbors_mean + blue_neighbors_mean, data_train, hidden = c(5,5), linear.output = FALSE)

rm(list=setdiff(ls(), "nn1"))

#saveRDS(nn1,"nn1.rds")