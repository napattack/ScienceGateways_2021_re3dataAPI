library(httr)
library(xml2)


re3data_request <- GET("http://re3data.org/api/v1/repositories")
re3data_IDs <- xml_text(xml_find_all(read_xml(re3data_request), xpath = "//id"))
URLs <- paste("https://www.re3data.org/api/v1/repository/", re3data_IDs, sep = "")


extract_repository_info <- function(url,count) {
  list(
    re3data_ID = xml_text(xml_find_all(repository_metadata_XML, "//r3d:re3data.orgIdentifier")),
    repositoryName = xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryName")),
    repositoryUrl = xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryURL")),
    api = paste(unique(xml_text(xml_find_first(repository_metadata_XML, paste0("//r3d:api","[",as.character(count),"]"))))),
    apiType = paste(unique(xml_text(xml_find_first(repository_metadata_XML, paste0("//r3d:api","[",as.character(count),"]","/@apiType")))))
  )
}



repository_info <- data.frame(matrix(ncol = 5, nrow = 0))

colnames(repository_info) <- c("re3data_ID", "repositoryName", "repositoryUrl","api","apiType")

for (url in URLs) {
  repository_metadata_request <- GET(url)
  repository_metadata_XML <-read_xml(repository_metadata_request) 
  api <- paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:api"))))
  count<-length(api)
  if(count>0){
    for(i in c(1:count)){
      results_list <- extract_repository_info(repository_metadata_XML,i)
      repository_info <- rbind(repository_info, results_list)    
    }}else next
  
}

head(repository_info)



