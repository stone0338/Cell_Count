library(jpeg)
library(rtiff)
library(tiff)
library(pixmap)
library(magick)
library(reshape2)
library(neuralnet)

if (!exists("nn1")) {nn1 <- readRDS("nn1.rds")}
  
filenames_1 <- list.files("Counting_Images", pattern="*.jpg", full.names=TRUE)
filenames_2 <- list.files("Counting_Images", pattern="*.tif", full.names=TRUE)

filenames <- c(filenames_1,filenames_2)

filenames_no_path <- substr(filenames,17,80)

file_number <- length(filenames)

result_summary <- read.csv("Counting_Images/Results.csv")

for (i in 1:file_number) {
  name_length <- nchar(filenames[i])
  
  if (grepl(".jpg", filenames[i])) { 
    image_test <- readJPEG(filenames[i])
    name_no_extension <- substr(filenames[i], 1, name_length-4)
    writeTIFF(image_test,paste(name_no_extension, ".tiff", sep = ""))
    image_test <- readTiff(paste(name_no_extension, ".tiff", sep = ""))
    file.remove(paste(name_no_extension, ".tiff", sep = ""))
  }
  else {
    if (grepl(".tiff", filenames[i])) {
      name_no_extension <- substr(filenames[i], 1, name_length-5)
      image_test <- readTiff(filenames[i])
    }
    else {
      if (grepl(".tif", filenames[i])) {
        
        name_no_extension <- substr(filenames[i], 1, name_length-4)
        image_test <- readTiff(filenames[i])
      }
    }
  }

red <- image_test@red
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

rm(list=setdiff(ls(), c("nn1","image_test","name_no_extension","filenames_no_path","result_summary", "i", "filenames","file_number", "red_neighbor_average")))

green <- image_test@green
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

rm(list=setdiff(ls(), c("nn1","image_test","name_no_extension","filenames_no_path","result_summary", "i", "filenames","file_number", "red_neighbor_average","green_neighbor_average")))

blue <- image_test@blue
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

rm(list=setdiff(ls(), c("nn1","image_test","name_no_extension","filenames_no_path","result_summary", "i", "filenames","file_number", "red_neighbor_average","green_neighbor_average", "blue_neighbor_average")))

data_test <- cbind(red_neighbor_average,green_neighbor_average,blue_neighbor_average)
data_test <- data_test[!duplicated(as.list(data_test))]
data_test <- data_test[,c("row",
                          "col",
                          "red_neighbors_mean",
                          "green_neighbors_mean",
                          "blue_neighbors_mean")]

rm(list=setdiff(ls(), c("nn1","data_test","image_test","name_no_extension","filenames_no_path","result_summary", "i", "filenames","file_number")))

predicted_data_test <- compute(nn1, data_test[,3:5])
prediction <- predicted_data_test$net.result
prediction[prediction >= 0.9] <- 1
prediction[prediction < 0.9] <- 0
data_test_2 <- data.frame(data_test[,1:2],prediction = prediction)
data_test_3 <- dcast(data_test_2, row~col)
data_test_3 <- data_test_3[,-1]

image_test_2 <- image_test
image_test_2@red[data_test_3 == 0] <- 1
image_test_2@green[data_test_3 == 0] <- 1
image_test_2@blue[data_test_3 == 0] <- 1

writeTiff(image_test_2, paste(name_no_extension, "_Counted.tiff", sep = ""))

file.copy(filenames[i], "./Counted_Images",overwrite = TRUE)
file.remove(filenames[i])
file.copy(paste(name_no_extension, "_Counted.tiff", sep = ""), "./Counted_Images",overwrite = TRUE)
file.remove(paste(name_no_extension, "_Counted.tiff", sep = ""))

area_positive <- sum(prediction)
counts <- data.frame(file_name = filenames_no_path[i], Area = area_positive)
result_summary <- rbind(result_summary,counts)
rm(list=setdiff(ls(), c("nn1","filenames_no_path","result_summary", "i", "filenames","file_number")))
}

write.csv(result_summary, "Counting_Images/Results.csv", row.names = FALSE)
