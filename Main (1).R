install.packages("stringdist")
install.packages("Matrix")
install.packages("quanteda")
install.packages("permutations")

library(rjson)
library(quanteda)
library(Matrix)
library("permutations")
#library(tidyverse)
library(stringr)

setwd("C:\\Users\\svenr\\OneDrive\\Documenten\\Uni\\Master\\Computer Science")

#setwd("/Users/gijshoving/Documents/Econometrics/Master QM/Block 2/CS for Business Analytics/Assignment")
#setwd("C:/Users/545700gh/Documents/CS")
#setwd("C:/Users/542549sn/OneDrive - Erasmus University Rotterdam/Documents")

source("Functions (2).R")

# Specify the path to your JSON file
json_file_path <- "TVs-all-merged.json"

# Read the JSON file into R
json_data <- fromJSON(file = json_file_path)

product_list <- create_product_list(json_data)

bands<- seq(10,260,50)
nr_bootstraps <- 5

perc_permutations <- 0.5
expl_shops <- TRUE
expl_brands <- TRUE
use_shingles <- TRUE
n_shingles <- 3

# Clean the titles
replace_vec = c(' inch' = 'inch', 'inch' = 'inch','Inch' = 'inch', 
                'inches' = 'inch', '"' = 'inch','-inch' = 'inch', 
                'INCH' = 'inch', '-Inch' = 'inch', '”' = 'inch', "'" = 'inch',
                ' hz' = 'hz', 'hz' = 'hz', 'Hertz' = 'hz','HZ' = 'hz',
                'Hz' = 'hz', '-hz' = 'hz', 'hz' = 'hz')
product_list <- standardise_product_list(product_list, replace_vec)


regex_pattern <- "[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*"

results_bands <- matrix(0,nrow = 9, ncol=100*nr_bootstraps) 
results_threshold <- matrix(0,nrow = 3, ncol=100*nr_bootstraps) 

set.seed(283284342)


brands<- sapply(sapply(product_list, function(product) product$featuresMap$"Brand"), function(x) ifelse(is.null(x),NA,x))
#brands_name <- sapply(sapply(product_list, function(product) product$featuresMap$"Brand Name"), function(x) ifelse(is.null(x),NA,x))
#brands_name_dot <- sapply(sapply(product_list, function(product) product$featuresMap$"Brand Name:"), function(x) ifelse(is.null(x),NA,x))
#manufacturer <-  sapply(sapply(product_list, function(product) product$featuresMap$"Manufacturer:"), function(x) ifelse(is.null(x),NA,x))
brands <- tolower(brands)
brands<-gsub("sceptre in", "sceptre",brands)
brands<-gsub("lg electronics", "lg",brands)
brands<- gsub("pansonic", "panasonic",brands)
for(i in 1:length(brands)){
  product_list[[i]]$brand<- brands[i]
}

for(j in 1:nr_bootstraps){
  bootstr <- bootstrap(product_list)
  train_list <- bootstr[[1]]
  test_list <- bootstr[[2]]
  
  binary_matrix_train <- create_model_words_binary_matrix(train_list, regex_pattern)
  #list_feature<- optimal_featuresize(binary_matrix_train, perc_perm = 0.5)
  #feature_size <- list_feature[[1]]
  #bands<-list_feature[[2]]
  feature_size<-floor(0.5*nrow(binary_matrix_train))
  bands <- c(2,10,20,floor(feature_size/(10:5)), floor(feature_size/seq(4.5,3,-0.5)),floor(feature_size/seq(2.75,2,-0.25)),feature_size)
  print("train signature")
  signature_matrix_train <- create_signature_matrix(binary_matrix_train,n_perm = feature_size)
 
  binary_matrix_test <- create_model_words_binary_matrix(test_list, regex_pattern)
  print("test signature")
  correction<-nrow(binary_matrix_test)/nrow(binary_matrix_train)
  signature_matrix_test <- create_signature_matrix(binary_matrix_test,n_perm = floor(correction*feature_size))
  
  if(use_shingles){
    binary_matrix_train<-create_shingles_binary_matrix(length_shingle = n_shingles, train_list)
    binary_matrix_test<-create_shingles_binary_matrix(length_shingle = n_shingles, test_list)
  }
  
  for(b in 1:length(bands)){  
    print(paste("Band", b, "out of", length(bands), "bootstrap", j, "out of", nr_bootstraps))
    
    print("train LSH")
    
    
    LSH_train <- LSH(signature_matrix_train, compare_LSH, bands = bands[b])
    results_threshold[1,(b-1)*nr_bootstraps+j]<-(1/bands[b])^(1/(nrow(signature_matrix_train)/bands[b]))
    print("train classification")
    threshold <- seq(0.05, 0.95, 0.05)
    results_train<- matrix(0,nrow=8,ncol = length(threshold))
    
    Jaccard_matrix_train <- Jaccard_classifications(LSH_train,binary_matrix = binary_matrix_train,product_list = train_list,exploit_different_shops = expl_shops, exploit_different_brands = expl_brands)
    true_same_train <- get_same_product(train_list)
    for (i in 1:length(threshold)) {
      results_train[,i]<-evaluate_threshold(threshold[i],Jaccard_matrix_train,true_same_train,LSH_train)
    }
    optimal_threshold<- which.max(results_train[4,])
    
    print("test LSH")
    LSH_test <- LSH(signature_matrix_test, compare_LSH, bands = correction*bands[b])
    results_threshold[2,(b-1)*nr_bootstraps+j]<-(1/bands[b])^(1/(nrow(signature_matrix_test)/(correction*bands[b])))
    results_threshold[3,(b-1)*nr_bootstraps+j]<-(1/bands[b])^(1/(nrow(signature_matrix_test)/(bands[b])))
    
    print("test classification")
    Jaccard_matrix_test <- Jaccard_classifications(LSH_test,binary_matrix = binary_matrix_test,product_list = test_list,exploit_different_shops = expl_shops, exploit_different_brands = expl_brands)
    true_same_test <- get_same_product(test_list)
    results_test<-evaluate_threshold(threshold[optimal_threshold],Jaccard_matrix_test,true_same_test,LSH_test)
    results_bands[1:4,(b-1)*nr_bootstraps+j]<-results_test[1:4]
    results_bands[5,(b-1)*nr_bootstraps+j]<- threshold[optimal_threshold]
    results_bands[6:9,(b-1)*nr_bootstraps+j]<- results_test[5:8]
    
  }
  
}

row.names(results_bands) <- c("recall","precision","F1","optimal_thershold","pair_quality","pair_completeness","F1_star", "fraction_comparisons")

#Save results_bands for the relevant setting
results_bands_tt<- results_bands
results_bands_ft<- results_bands
results_bands_tf<- results_bands
results_bands_ff- results_bands

average_results_tt<-matrix(0,9,18)
average_results_tf<-matrix(0,9,18)
average_results_ft<-matrix(0,9,18)
average_results_ff<-matrix(0,9,18)
average_threshold<-matrix(0,3,18)
for(i in 1:18){
  average_results_tt[,i]<- rowMeans(results_bands_tt[,((i-1)*nr_bootstraps*+1):(i*nr_bootstraps)])
  average_results_tf[,i]<- rowMeans(results_bands_tf[,((i-1)*nr_bootstraps*+1):(i*nr_bootstraps)])
  average_results_ft[,i]<- rowMeans(results_bands_ft[,((i-1)*nr_bootstraps*+1):(i*nr_bootstraps)])
  average_results_ff[,i]<- rowMeans(results_bands_ff[,((i-1)*nr_bootstraps*+1):(i*nr_bootstraps)])
  average_threshold[,i]<- rowMeans(results_threshold[,((i-1)*nr_bootstraps*+1):(i*nr_bootstraps)])
}


names_datf<- c("recall","precision","F1","F1_clas","opt_threshold","pair_quality","pair_completeness","F1_star", "fraction_comparisons")
datf_tt<-as.data.frame(t(average_results_tt))
colnames(datf_tt)<-names_datf
datf_tf<-as.data.frame(t(average_results_tf))
colnames(datf_tf)<-names_datf
datf_ft<-as.data.frame(t(average_results_ft))
colnames(datf_ft)<-names_datf
datf_ff<-as.data.frame(t(average_results_ff))
colnames(datf_ff)<-names_datf
datf_threshold<- as.data.frame(t(average_threshold))
colnames(datf_threshold)<- c("threshold train", "threshold test with correction", "threshold test without correction")
datf_threshold$fraction_comparisons<- datf_tt$fraction_comparisons

names_meas_df <- c("Brands and shops", "Brands", "Shops", "None","Fraction_of_comparisons") 
F1_star<-as.data.frame(matrix(c(datf_tt$F1_star,datf_tf$F1_star,datf_ft$F1_star,datf_ff$F1_star, datf_tt$fraction_comparisons),nrow=18,ncol=5))
colnames(F1_star)<- names_meas_df

F1<-as.data.frame(matrix(c(datf_tt$F1,datf_tf$F1,datf_ft$F1,datf_ff$F1, datf_tt$fraction_comparisons),nrow=18,ncol=5))
colnames(F1)<- names_meas_df

F1_clas<-as.data.frame(matrix(c(datf_tt$F1_clas,datf_tf$F1_clas,datf_ft$F1_clas,datf_ff$F1_clas, datf_tt$fraction_comparisons),nrow=18,ncol=5))
colnames(F1_clas)<- names_meas_df

PQ<-as.data.frame(matrix(c(datf_tt$pair_quality,datf_tf$pair_quality,datf_ft$pair_quality,datf_ff$pair_quality, datf_tt$fraction_comparisons),nrow=18,ncol=5))
colnames(PQ)<- names_meas_df

PC<-as.data.frame(matrix(c(datf_tt$pair_completeness,datf_tf$pair_completeness,datf_ft$pair_completeness,datf_ff$pair_completeness, datf_tt$fraction_comparisons),nrow=18,ncol=5))
colnames(PC)<- names_meas_df

library(tidyr)
library(ggplot2)
F1_star_long <- pivot_longer(F1_star, cols = -Fraction_of_comparisons, names_to = "Category", values_to = "Value")

# Plot using ggplot2
ggplot(F1_star_long, aes(x = Fraction_of_comparisons, y = Value, color = Category)) +
  geom_line(color="black") +
  labs(title = "F1*",
       x = "Fraction of Comparisons",
       y = "F1*") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.0002, NA)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_classic()

F1_long <- pivot_longer(F1, cols = -Fraction_of_comparisons, names_to = "Category", values_to = "Value")

# Plot using ggplot2
ggplot(F1_long, aes(x = Fraction_of_comparisons, y = Value, color = Category)) +
  geom_line() +
  labs(title = "Comparison Lines",
       x = "Fraction of Comparisons",
       y = "F1") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.0002, NA)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_classic()

F1_clas_long <- pivot_longer(F1_clas, cols = -Fraction_of_comparisons, names_to = "Category", values_to = "Value")

# Plot using ggplot2
ggplot(F1_clas_long, aes(x = Fraction_of_comparisons, y = Value, color = Category)) +
  geom_line() +
  labs(title = "Comparison Lines",
       x = "Fraction of Comparisons",
       y = "F1^c") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.0002, NA)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_classic()




PQ_long <- pivot_longer(PQ, cols = -Fraction_of_comparisons, names_to = "Category", values_to = "Value")
# Plot using ggplot2
ggplot(PQ_long, aes(x = Fraction_of_comparisons, y = Value, color = Category)) +
  geom_line(color="black") +
  labs(title = "Pair quality",
       x = "Fraction of Comparisons",
       y = "Pair Quality") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.0002, NA)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_classic()

PC_long <- pivot_longer(PC, cols = -Fraction_of_comparisons, names_to = "Category", values_to = "Value")
# Plot using ggplot2
ggplot(PC_long, aes(x = Fraction_of_comparisons, y = Value, color = Category)) +
  geom_line(color="black") +
  labs(title = "Pair quality",
       x = "Fraction of Comparisons",
       y = "Pair Completeness") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.0002, NA)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_classic()


threshold_longer <- pivot_longer(datf_threshold, cols = -fraction_comparisons, names_to = "Category", values_to = "Value")
ggplot(threshold_longer, aes(x = fraction_comparisons, y = Value, color = Category)) +
  geom_line() +
  labs(title = "Comparison Lines",
       x = "Fraction of Comparisons",
       y = "Threshold") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.0002, NA)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_classic()
