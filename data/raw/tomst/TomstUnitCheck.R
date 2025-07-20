

############################################## Figure out units - Tomst ############################################## 

# Wrote this to look at the first measured value for Tomst probes. The accuracy is .27 um which means the largest nonzero increment can be 0.27 micrometeres. 
# to run with data - eitehr linked to synology or downloaded on your own computer, be sure to change the paths as necessary. 

# open latest download folder 

file_list <- list.files("~/Desktop/Dendro/Data/Vcp/05.21.25/")
SNKey <- read.csv("~/Desktop/Dendro/Data/Vcp/tomst_sn_names.csv")
colnames <- c("num", "ts", "timezone", "temp", "qcFlag", "empty", "value", "empty", "empty")

tensionTableVcp <- data.frame()

for (fileX in file_list) {
  data <- read.delim(paste0("~/Desktop/Dendro/Data/Vcp/05.21.25/", fileX), sep = ";", header = F)
  colnames(data) <- colnames
  #get SN 
  filename <- basename(fileX)
  Serial <- sub(".*data_(\\d+)_2025.*", "\\1", filename)
  name <- SNKey[SNKey$SN==Serial,]
  treeName <- name$name
  # Find index of first nonzero value
  start_index <- which(data$value >= 1200 & !is.na(data$value))[1]
  # Extract 5 rows starting from that index
  subset_df <- data[ start_index:(start_index + 4), ]
  subset_df$convertedValue <- (subset_df$value - 1279)*(8890/(34000-1279))
  subTable <- data.frame(SerialNo = Serial,
                         TreeName = treeName,
                         OriginalValue = subset_df$value[1],
                         ConvertedValue1 = subset_df$convertedValue[1],
                         Increment1 = subset_df$convertedValue[2] - subset_df$convertedValue[1],
                         Increment2 = subset_df$convertedValue[3] - subset_df$convertedValue[2],
                         Increment3 = subset_df$convertedValue[4] - subset_df$convertedValue[3],
                         Increment4 = subset_df$convertedValue[5] - subset_df$convertedValue[4],
                         Site = "Vcp"
  )
  tensionTableVcp <- rbind(subTable, tensionTableVcp)
}

## Repeat for Vcs 

file_list <- list.files("~/Desktop/Dendro/Data/Vcs/05.15.2025/")
SNKey <- read.csv("~/Desktop/Dendro/Data/Vcs/tomst_sn_names.csv")
colnames <- c("num", "ts", "timezone", "temp", "qcFlag", "empty", "value", "empty", "empty")

tensionTableVcs <- data.frame()

for (fileX in file_list) {
  data <- read.delim(paste0("~/Desktop/Dendro/Data/Vcs/05.15.2025/", fileX), sep = ";", header = F)
  colnames(data) <- colnames
  #get SN 
  filename <- basename(fileX)
  Serial <- sub(".*data_(\\d+)_2025.*", "\\1", filename)
  name <- SNKey[SNKey$SN==Serial,]
  treeName <- name$Name
  # Find index of first nonzero value
  start_index <- which(data$value >= 1200 & !is.na(data$value))[1]
  # Extract 5 rows starting from that index
  subset_df <- data[ start_index:(start_index + 4), ]
  subset_df$convertedValue <- (subset_df$value - 1279)*(8890/(34000-1279))
  subTable <- data.frame(SerialNo = Serial,
                         TreeName = treeName,
                         OriginalValue = subset_df$value[1],
                         ConvertedValue1 = subset_df$convertedValue[1],
                         Increment1 = subset_df$convertedValue[2] - subset_df$convertedValue[1],
                         Increment2 = subset_df$convertedValue[3] - subset_df$convertedValue[2],
                         Increment3 = subset_df$convertedValue[4] - subset_df$convertedValue[3],
                         Increment4 = subset_df$convertedValue[5] - subset_df$convertedValue[4],
                         Site = "Vcs"
  )
  tensionTableVcs <- rbind(subTable, tensionTableVcs)
}


## and Mpj 

file_list <- list.files("~/Desktop/Dendro/Data/Mpj/5.13.25/")
SNKey <- read.csv("~/Desktop/Dendro/Data/Mpj/tomst_sn_names.csv")
colnames <- c("num", "ts", "timezone", "temp", "qcFlag", "empty", "value", "empty", "empty")

tensionTableMpj <- data.frame()

for (fileX in file_list) {
  data <- read.delim(paste0("~/Desktop/Dendro/Data/Mpj/5.13.25/", fileX), sep = ";", header = F)
  colnames(data) <- colnames
  #get SN 
  filename <- basename(fileX)
  Serial <- sub(".*data_(\\d+)_2025.*", "\\1", filename)
  name <- SNKey[SNKey$SN==Serial,]
  treeName <- name$name
  # Find index of first nonzero value
  start_index <- which(data$value > 3 & !is.na(data$value))[1]
  # Extract 5 rows starting from that index
  subset_df <- data[ start_index:(start_index + 4), ]
  subset_df$convertedValue <- (subset_df$value - 1279)*(8890/(34000-1279))
  subTable <- data.frame(SerialNo = Serial,
                         TreeName = treeName,
                         OriginalValue = subset_df$value[1],
                         ConvertedValue1 = subset_df$convertedValue[1],
                         Increment1 = subset_df$convertedValue[2] - subset_df$convertedValue[1],
                         Increment2 = subset_df$convertedValue[3] - subset_df$convertedValue[2],
                         Increment3 = subset_df$convertedValue[4] - subset_df$convertedValue[3],
                         Increment4 = subset_df$convertedValue[5] - subset_df$convertedValue[4],
                         Site = "Mpj"
  )
  tensionTableMpj <- rbind(subTable, tensionTableMpj)
}

## and Wjs 

file_list <- list.files("~/Desktop/Dendro/Data/Wjs/05.22.25/")
SNKey <- read.csv("~/Desktop/Dendro/Data/Wjs/tomst_sn_names.csv")
colnames <- c("num", "ts", "timezone", "temp", "qcFlag", "empty", "value", "empty", "empty")

tensionTableWjs <- data.frame()

for (fileX in file_list) {
  data <- read.delim(paste0("~/Desktop/Dendro/Data/Wjs/05.22.25/", fileX), sep = ";", header = F)
  colnames(data) <- colnames
  #get SN 
  filename <- basename(fileX)
  Serial <- sub(".*data_(\\d+)_2025.*", "\\1", filename)
  name <- SNKey[SNKey$SN==Serial,]
  treeName <- name$name
  # Find index of first nonzero value
  start_index <- which(data$value != 0 & data$value != 1 & !is.na(data$value))[1]
  # Extract 5 rows starting from that index
  subset_df <- data[ start_index:(start_index + 4), ]
  subset_df$convertedValue <- (subset_df$value - 1279)*(8890/(34000-1279))
  subTable <- data.frame(SerialNo = Serial,
                         TreeName = treeName,
                         OriginalValue = subset_df$value[1],
                         ConvertedValue1 = subset_df$convertedValue[1],
                         Increment1 = subset_df$convertedValue[2] - subset_df$convertedValue[1],
                         Increment2 = subset_df$convertedValue[3] - subset_df$convertedValue[2],
                         Increment3 = subset_df$convertedValue[4] - subset_df$convertedValue[3],
                         Increment4 = subset_df$convertedValue[5] - subset_df$convertedValue[4],
                         Site = "Wjs"
  )
  tensionTableWjs <- rbind(subTable, tensionTableWjs)
}

tensionTableAll <- rbind(tensionTableVcp, tensionTableVcs, tensionTableWjs, tensionTableMpj)

write.csv(tensionTableAll, "~/Desktop/Dendro/startValueTab.csv")
