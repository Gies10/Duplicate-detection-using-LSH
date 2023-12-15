# Load the package required to read JSON files.
install.packages("rjson") # Optional
library("rjson")
# Give the input file name to the function.
myData <- fromJSON(file="C:\\Users\\svenr\\Downloads\\TVs-all-merged\\TVs-all-merged.json")
hoj<- as.data.frame(myData)
# Print the result.
print(myData)


##Check on how many webshops a television is sold
length(unique(names(myData)))
length_of_lists<- rep(0,length(myData))
for(i in 1:length(myData)){
  length_of_lists[i]<- length(myData[[i]])
}
sum(length_of_lists>1)
length_of_lists>1
sum(length_of_lists)
table(length_of_lists)

###Find products with same shops
product_list <- create_product_list(myData)
true_same<-get_same_product(product_list)
indeces_true_same<-which(true_same==1, arr.ind = TRUE)
shop<- matrix(0, nrow(indeces_true_same),2)
for(i in 1:nrow(indeces_true_same)){
  shop[i,]<- c(product_list[[indeces_true_same[i,1]]]$shop, product_list[[indeces_true_same[i,2]]]$shop)
}

which(shop[,1]==shop[,2])
same_shop <- indeces_true_same[which(shop[,1]==shop[,2]),]

##Check how many identical televisions have a different brand (should be 0)
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
sum(brands[indeces_true_same[,1]]!=brands[indeces_true_same[,2]]&!is.na(brands[indeces_true_same[,1]])&!is.na(brands[indeces_true_same[,2]]))

###Check which products are not similar at all
product_list <- create_product_list(myData)
replace_vec = c(' inch' = 'inch', 'inch' = 'inch','Inch' = 'inch', 
                'inches' = 'inch', '"' = 'inch','-inch' = 'inch', 
                'INCH' = 'inch', '-Inch' = 'inch', '”' = 'inch', "'" = 'inch',
                ' hz' = 'hz', 'hz' = 'hz', 'Hertz' = 'hz','HZ' = 'hz',
                'Hz' = 'hz', '-hz' = 'hz', 'hz' = 'hz')
product_list <- standardise_product_list(product_list, replace_vec)
whole<-matrix(1,nrow=length(product_list), ncol=length(product_list))
regex_pattern <- "[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*"
binary_whole<-create_model_words_binary_matrix(product_list, regex_pattern)
jaccard_whole<-Jaccard_classifications(whole,binary_whole,product_list,FALSE,FALSE)
sum(jaccard_whole>0,na.rm=TRUE)/sum(!is.na(jaccard_whole))
