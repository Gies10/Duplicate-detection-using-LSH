create_product_list <-function(Data){
  product_list<- list()
  for(i in 1:length(Data)){
    product_list<-append(product_list,Data[[i]])
  }
  return(product_list)
}

create_shingles <- function(text, n = 3) {
    # Convert the text to lowercase
    text <- tolower(text)
    # Remove any empty spaces
    text <- gsub("\\s", "", text)
    
    # Create shingles
    shingles <- lapply(1:(nchar(text) - n + 1), function(i) {
      substr(text, i, i + n - 1)
    })
  return(shingles)
}

create_shingles_binary_matrix <-function(length_shingle,product_list){
  total_shingles<-c()
  shingles_list<- list()
  for(i in 1:length(product_list)){
    shingles <- unlist(create_shingles(product_list[[i]]$title,length_shingle))
    shingles_list[[i]]<- shingles
    total_shingles<- c(total_shingles,shingles)
  }
  
  unique_total_shingles<-unique(total_shingles)
  
  binary_matrix<-matrix(0,length(unique_total_shingles),length(product_list))
  for(i in 1:length(product_list)){
    binary_matrix[,i]<- as.numeric(unique_total_shingles %in% shingles_list[[i]])
  }
  return(binary_matrix)
}

create_model_words_binary_matrix <- function(product_list, regex_pattern){
  model_words<-extract_model_words(product_list, regex_pattern)
  unique_model_words <- unique(unlist(model_words))
  
  binary_matrix<-matrix(0,length(unique_model_words),length(product_list))
  
  for(i in 1:length(product_list)){
    binary_matrix[,i]<- as.numeric(unique_model_words %in% model_words[[i]])
  }
  return(binary_matrix)
}

create_signature_matrix <- function(binary_matrix,n_perm){
  # Create permutation matrix
  #set.seed(2)
  n_rows <- dim(binary_matrix)[1]
  n_products <-  dim(binary_matrix)[2]
  permutation_matrix = matrix(0,n_perm,n_rows)
  for (i in 1:n_perm) {
    permutation_matrix[i,] = sample(c(1:n_rows))
  }
  
  # Calculate the signature matrix
  signature_matrix <- matrix(0,n_perm,n_products)
  for (i in 1:n_perm){
    #print(i)
    permuted_binary_matrix <- binary_matrix[permutation_matrix[i, ], ]
    ones_indices <- which(permuted_binary_matrix == 1, arr.ind = TRUE)
    signature_matrix[i, ] <- ones_indices[order(ones_indices[, "col"]), "row"][duplicated(ones_indices[, "col"]) == 0]
  }
  return(signature_matrix)
}

compare_LSH<-function(signature_matrix_i, signature_matrix_j, bands){
  rows <- length(signature_matrix_i)/bands
  for(b in 1:bands){
    if(identical(signature_matrix_i[((b-1)*rows+1):(b*rows)],signature_matrix_j[((b-1)*rows+1):(b*rows)])){
      return(1)
    }
  }
  return(0)
}

LSH <- function(signature_matrix, bucket_function, bands) {
  amount_products <- ncol(signature_matrix)
  same_bucket <- matrix(NA, amount_products, amount_products)
  
  for (i in 1:(amount_products - 1)) {
    #print(i)
    for (j in (i + 1):amount_products) {
      same_bucket[i, j] <- bucket_function(signature_matrix[, i], signature_matrix[, j], bands)
    }
  }
  
  return(same_bucket)
}

##Jaccard and classification
Jaccard <- function(binary_matrix, vector1, vector2){
  ones1 <- which(binary_matrix[,vector1]==1)
  ones2 <- which(binary_matrix[,vector2]==1)
  jaccard <- length(intersect(ones1,ones2))/length(union(ones1,ones2))
  return(jaccard)
}

Jaccard_classifications <- function(buckets, binary_matrix, product_list, exploit_different_shops=FALSE, exploit_different_brands = FALSE){
  amount_products <- length(product_list)
  classification <- matrix(NA, amount_products, amount_products)
  for(i in 1:(amount_products-1)){
    #print(i)
    for (j in (i+1): amount_products) {
      if(buckets[i,j]==1){
        ###Used exploit different shops here, maybe that doesn't work because of sampling with replacement in bootstrap
        if(exploit_different_shops && product_list[[i]]$shop == product_list[[j]]$shop){
          classification[i,j]<-0
        } else if(exploit_different_brands && product_list[[i]]$brand != product_list[[j]]$brand && !is.na(product_list[[i]]$brand)&&!is.na(product_list[[j]]$brand)) {
          classification[i,j]<-0
        } else {
          classification[i,j]<-Jaccard(binary_matrix,i,j)
        }
      } else {
        classification[i,j]<-0
      }
    }
  }
  return(classification)
}

classification <- function(Jaccard_matrix, threshold){
  classification_matrix <- ifelse(Jaccard_matrix > threshold, 1, 0)
  return(classification_matrix)
}

get_same_product<-function(product_list){
  amount_products <- length(product_list)
  samesies <- matrix(NA, amount_products, amount_products)
  for(i in 1:(amount_products-1)){
    for (j in (i+1): amount_products) {
      if(product_list[[i]]$modelID==product_list[[j]]$modelID){
        samesies[i,j] <- 1 
      } else{
        samesies[i,j] <-0
      }
    }
  }
  return(samesies)
}

get_confusion_matrix<-function(real_same, classification){
  amount_products <- nrow(classification)
  TP<-0
  FP<-0
  FN<-0
  TN<-0
  for(i in 1:(amount_products-1)){
    for (j in (i+1): amount_products) {
      if(real_same[i,j]==1){
        if(classification[i,j]==1){
          TP <- TP + 1
        } else {
          FN <- FN + 1
        }
      } else {
        if(classification[i,j]==1){
          FP <- FP + 1
        } else {
          TN <- TN + 1
        }
      }
    }
  }
  return(matrix(c(TP,FN,FP,TN),nrow = 2,ncol = 2))
}

bootstrap <- function(product_list){
  amount_products <- length(product_list)
  train_set <- unique(sample(1:amount_products, amount_products, replace=TRUE))
  train_list <- product_list[train_set]
  test_list <- product_list[setdiff(1:length(product_list),train_set)]
  return(list(train_list,test_list))
}

evaluate_threshold<- function(threshold,Jaccard,true_same,LSH){
  classify <- classification(Jaccard,threshold)
  CM <- get_confusion_matrix(true_same,classify)
  TP <- CM[1,1]
  FN <- CM[2,1]
  FP <- CM[1,2]
  TN <- CM[2,2]
  
  recall <- TP/(TP+FN)
  precision <- TP/(TP+FP)
  F1 <- 2/(1/precision + 1/recall)
  
  classify_0 <-matrix(as.numeric(classify==0),nrow(classify),ncol(classify))
  true_same_0 <-  matrix(as.numeric(true_same==0),nrow(true_same),ncol(true_same))
  TP_clas <- sum(LSH*classify*true_same,na.rm = TRUE)
  FP_clas <- sum(LSH*classify*true_same_0,na.rm = TRUE)
  FN_clas <- sum(LSH*classify_0*true_same,na.rm = TRUE)
  recall_clas <- TP_clas/(TP_clas+FN_clas)
  precision_clas <- TP_clas/(TP_clas+FP_clas)
  F1_clas <- 2/(1/precision_clas + 1/recall_clas)
  
  CM_LSH <- get_confusion_matrix(true_same,LSH)
  TP_LSH <- CM_LSH[1,1]
  FN_LSH <- CM_LSH[2,1]
  FP_LSH <- CM_LSH[1,2]
  TN_LSH <- CM_LSH[2,2]
  
  #pair_quality <- TP_LSH/(TP_LSH+FP_LSH)
  #pair_completeness <- TP_LSH/(TP_LSH+FN_LSH)
  
  pair_quality <- sum(LSH*true_same, na.rm = TRUE)/sum(LSH, na.rm = TRUE)
  pair_completeness <- sum(LSH*true_same, na.rm = TRUE)/sum(true_same, na.rm = TRUE)
  F1_star <- 2/(1/pair_quality + 1/pair_completeness)
  
  fraction_comparisons <- sum(LSH, na.rm = TRUE)/sum(!is.na(LSH))
  
  return(c(recall,precision,F1,F1_clas,pair_quality,pair_completeness,F1_star, fraction_comparisons))
}

standardise_product_list <- function (product_list, replace_vec)
{
  titles <- sapply(product_list, function(product) product$title)
  title_list_std = lapply(titles, function(x) {
    title = str_replace_all(x, replace_vec)
    title = tolower(title)
    #title = tolower(gsub("[[:punct:]]", "", title))
    title = str_trim(gsub("\\s+", " ", title)) 
    return(title)
  })
  for(i in 1:length(product_list)){
    product_list[[i]]$title <- title_list_std[i]
  }
  return (product_list)
}

# Function to extract model words based on the regex
extract_model_words <- function(product_list, regex_pattern) {
  titles <- sapply(product_list, function(product) product$title)
  model_words <- lapply(titles, function(x) {
    matches <- str_extract_all(x, regex_pattern)
    matches <- str_remove_all(matches[[1]], "[0-9]{4}(?:inch)|[0-9]{3}(?:inch)|[0-9]{5}(?:inch)|[0-9]{6}(?:inch)|[0-9]{7}(?:inch)")
    # Combine the matches into a single string
    result <- unlist(matches)
    result <- result[result != ""]
    return(result)})
  return(model_words)
}




optimal_featuresize <- function(binary_matrix, perc_perm=0.5){
  feature_size <- perc_perm * nrow(binary_matrix)
  #round(nrow(binary_matrix)/c(16,2)) %*% c(16,27)
  size <- floor(perc_perm * nrow(binary_matrix)+100)
  # Function to find the common multiple closest to x (smaller than x)
  max_length<-0
  final_bands <- 0
  lcm <- 0
  for(i in size:1){
    bands <- seq(0,size,1)
    bands <- bands[which(i%%bands==0)]
    if (length(bands)>max_length){
      max_length <- length(bands)
      lcm <- i
      final_bands <- bands
    }
  }
  #lcm <- nearest_common_multiple_smaller_than_x(size, c(16,27))
  return(list(lcm, final_bands[which(final_bands>1)]))
}
