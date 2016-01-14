library(rvest)
library(magrittr)
library(stringr)
library(RCurl)
library(gdata)
library(httr)

####################
# variables
####################
# whitehouseblackmarket web address 
WHBM.Addr <- "http://www.whitehouseblackmarket.com/store/product-list/?No=0&Nrpp=1000"  
# whitehouseblackmarket web address of Android forum 
WHBM.Addr2 <- "http://www.whitehouseblackmarket.com/store/product-list/?No=1000&Nrpp=2000"
# Combine links
WHBM.linklist <- 
  c(WHBM.Addr, WHBM.Addr2)
# article data matrix
WHBM.data <- data.frame()
count <-  0
mainDir <- paste0(getwd(), "/WHBM/")
subDir <-  "individualFiles/"
fName <- "WHBM.csv"
####################
# functions
####################
# Function GetHtml()
GetHtml <- function(webaddr){
  options(RCurlOptions = list(proxy = "52.71.73.177",proxyport=80)) 
  return
  webaddr %>% 
    getURLContent(useragent = "R", .encoding = "UTF-8") %>% 
    read_html()
}
# Function GetHtmlText()
GetHtmlText <- function(para){
  return
  para %>%
    html_text() %>%
    gsub('[\r\n\t]', '', .) %>%
    trim()
}
# Function GetHtmlAttr()
GetHtmlAttr <- function(para,attr){
  return
  para %>%
    html_attr(attr) %>%
    gsub('[\r\n\t]', '', .) %>%
    trim()
}

PriceProcess<- function(p){
  if(p %>% str_detect("was")){
    priceNow <- strsplit(p, "was")[[1]][1] %>% 
      gsub('\\D', '', .) %>%
      gsub('\\s', '', .)
    priceWas <- strsplit(p, "was")[[1]][2] %>% 
      gsub('\\D', '', .) %>%
      gsub('\\s', '', .)
    price <- NA
    return
    c(price, priceNow, priceWas)
  }else{
    price <-  p %>%
      gsub('\\D', '', .) %>%
      gsub('\\s', '', .)
    priceNow <- NA
    priceWas <- NA
    return
    c(price, priceNow, priceWas)
  }
}

####################
# Get articles data
####################

if(!file.exists(paste0(mainDir, fName))){
  tryCatch({
    
  #create folder for results
  dir.create(file.path(mainDir))
  dir.create(file.path(mainDir, subDir))
  
  # loop
  while(TRUE){
    
    if(url.exists(WHBM.linklist[1])){
      
      # Fetch Freq
      #Sys.sleep(600000)
      Sys.sleep(10)
      
      WHBM.data <- data.frame()
      
      # 0-1000, 1000-2000 items from api links
      for (i in 1:length(WHBM.linklist))
      {
        # Get articles html
        WHBM.article <-
          WHBM.linklist[i] %>% 
          GetHtml() 
        
        WHBM.name <- 
          WHBM.article %>%
          html_nodes(xpath = "//div[@class='product-information']/a[1]") %>% 
          GetHtmlText() %>%
          trim()
        
        WHBM.styleid <- 
          WHBM.article %>%
          html_nodes(xpath = "//div[@class='product-information']/a[1]") %>% 
          GetHtmlAttr("onclick") %>% 
          gsub("[s_objectID='product__name;]", "", .) %>%
          gsub('\\s', '', .)
        
        WHBM.rating <- 
          WHBM.article %>%
          html_nodes(xpath = "//div[@class='BVRRRatingNormalOutOf']/span[1]") %>% 
          GetHtmlText() %>%
          gsub('\\s', '', .)
        
        WHBM.rating.count<- 
          WHBM.article %>%
          html_nodes(xpath = "//div[@class='BVRRRatingNormalOutOf']/span[4]") %>% 
          GetHtmlText() %>%
          gsub('\\s', '', .)
        
        # Get price html
        WHBM.price.text<- 
          WHBM.article %>%
          html_nodes(xpath = "//div[@class='product-price']") %>% 
          GetHtmlText() 
        
        WHBM.price <- data.frame()
        for(i in 1:length(WHBM.price.text)){
          WHBM.price <- rbind(WHBM.price, data.frame(PriceProcess(WHBM.price.text[i])[1], PriceProcess(WHBM.price.text[i])[2], PriceProcess(WHBM.price.text[i])[3]))
        }
        names(WHBM.price) <-  c("price", "priceNow", "priceWas")
        
        WHBM.data <-
          cbind(WHBM.styleid)%>%
          cbind(WHBM.name)%>%
          cbind(WHBM.rating) %>%
          cbind(WHBM.rating.count) %>%
          cbind(WHBM.price) %>%
          rbind(WHBM.data)
        
        count = count + 1
      }
      
      # print and write
      print(paste(Sys.time(), " - ", "No.", count/length(WHBM.linklist) ))
      
      #save individual file
      strTime = str_replace_all((Sys.time()), "[ :-]", "_") 
      indiFileName = str_replace(paste0("WHBM ", strTime, ".csv"), " ", "_")
      write.table(WHBM.data, paste0(mainDir, subDir, indiFileName), row.names = FALSE, sep = ",", append = FALSE)
      
      #save integral table per 10 mins ver. in .csv 
      if(count > 0)
        write.table(WHBM.data, file = paste0(mainDir, fName), row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
      else
        write.table(WHBM.data, file = paste0(mainDir, fName), row.names = FALSE, sep = ",", append = TRUE)
    }
  }
  },
  error = function(err){
    print(err)
    print(paste(">>>>>Process occur err at ",Sys.time()))
  },
  finally = {})
}else{
  print(">>>>>File or Path already exists, please check existing files and restart to avoid overwriteing!")
  print(paste(">>>>>Process suspend at ",Sys.time()))
} 
