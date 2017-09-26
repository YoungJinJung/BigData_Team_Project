
#ÆÐÅ°Áö ÀÎ½ºÅç ÄÚµå
#È¯°æ ±¸ÃàÀ» À§ÇÑ ¼±Çà ÄÚµåµé
#install.packages("KoNLP")
#install.packages("tm")
#install.packages("topicmodels")
#install.packages("SnowballC") # Suggests: filehash, proxy, Rcampdf, Rgraphviz, Rpoppler, SnowballC, XML tmÆÐÅ°Áö¸¦ À§ÇØ ±ò¾Æ¾ßÇÏ´Â°Í
#install.packages("Rmpfr")
#install.packages("XML")
#install.packages("RMySQL")

#Ãß°¡ ¶óÀÌºê·¯¸®µé
library(KoNLP)
library(tm)

library("topicmodels")
library("slam")
library(XML)

library("Rmpfr")
library("RMySQL")

getNaverNews <- function()
{
  print("Fast mode is not consider ip ban.")
  print("Safe mode is consider ip ban to collect slowly.")
  collectMode <- readline(prompt="Choose mode(1: Fast mode 2: Safe mode): ")
  
  if(collectMode==1)
  {
    CTime=0
  }
  else
  {
    CTime=0.5
  }
  
  startDate <- as.Date(readline(prompt="Enter start date(yyyy-mm-dd): "))
  while(!grepl("^(19|20)..[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",startDate))
    # end while
  {
    startDate <- as.Date(readline(prompt="Please ;) enter start date(yyyy-mm-dd): "))
  }
  # end if
  
  endDate <- as.Date(readline(prompt="Enter end date(yyyy-mm-dd): "))
  while(!grepl("^(19|20)..[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",endDate))
  {
    endDate <- as.Date(readline(prompt="Please ;) enter end date(yyyy-mm-dd): "))
  }
  # end while
  while(startDate>endDate)
  {
    endDate <- as.Date(readline(prompt="Please ;) enter end date later than start date(yyyy-mm-dd): "))
  }
  # end while
  
  print("1: politics 2: economy 3: society 4: culture 5: world news")
  print("6: science 7: IT 8: sports 9: entertainment 10: weather")
  print("If you don't enter 1 or 2, it will be done with nothing. Thank you.")
  selectAll <- readline(prompt="Do you want colect all categories?(yes:1, no:2): ")
  if(selectAll!=1)
  {
    selectPolitics <- readline(prompt="Do you want politics category?(yes:1, no:2): ")
    selectEconomy <- readline(prompt="Do you want economy category?(yes:1, no:2): ")
    selectSociety <- readline(prompt="Do you want society category?(yes:1, no:2): ")
    selectCulture <- readline(prompt="Do you want culture category?(yes:1, no:2): ")
    selectWorld <- readline(prompt="Do you want world news category?(yes:1, no:2): ")
    selectScience <- readline(prompt="Do you want science category?(yes:1, no:2): ")
    selectIT <- readline(prompt="Do you want IT category?(yes:1, no:2): ")
    selectSports <- readline(prompt="Do you want sports category?(yes:1, no:2): ")
    selectEntertain <- readline(prompt="Do you want entertainment category?(yes:1, no:2): ")
    selectWeather <- readline(prompt="Do you want weather category?(yes:1, no:2): ")
    selectFinance <- readline(prompt="Do you want finance category?(yes:1, no:2): ")
  }
  else
  {
    selectPolitics<-1
    selectEconomy<-1
    selectSociety<-1
    selectCulture<-1
    selectWorld<-1
    selectScience<-1
    selectIT<-1
    selectSports<-1
    selectEntertain<-1
    selectWeather<-1
    selectFinance<-1
  }
  # end if
  
  print(paste("Please wait. Results will save at",getwd()))
  print("Start step 1. Scraping URL.")
  
  
  end<-as.integer(endDate-startDate)
  
  listAll<-list()
  list01<-list()
  list02<-list()
  list03<-list()
  list04<-list()
  list05<-list()
  list06<-list()
  list07<-list()
  list08<-list()
  list09<-list()
  list10<-list()
  
  if(selectPolitics==1)
  {
    
    print("Start Politics part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=269&sid1=100&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=269&sid1=100&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list01<-c(list01,listif)
        
        Sys.sleep(CTime)
        list01<-unlist(list01)
        list01<-unique(list01)
        lennow<-length(list01)
        print(paste("I'm scraping politics part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list01<-gsub('<a href=\"','',list01)
    list01<-gsub('\">','',list01)
    list01<-substr(list01,1,108)
    if(length(grep("hot",list01))>0)
    {
      list01<-list01[-grep("hot",list01)]
    }
    # end if
    if(length(grep("endic",list01))>0)
    {
      list01<-list01[-grep("endic",list01)]
    }
    # end if
    if(length(grep("target",list01))>0)
    {
      list01<-list01[-grep("target",list01)]
    }
    # end if
    if(length(grep("class",list01))>0)
    {
      list01<-list01[-grep("class",list01)]
    }
    # end if
  }
  # end if
  
  if(selectEconomy==1)
  {
    
    print("Start economy part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=263&sid1=101&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=263&sid1=101&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list02<-c(list02,listif)
        
        Sys.sleep(CTime)
        list02<-unlist(list02)
        list02<-unique(list02)
        lennow<-length(list02)
        print(paste("I'm scraping economy part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list02<-gsub('<a href=\"','',list02)
    list02<-gsub('\">','',list02)
    list02<-substr(list02,1,108)
    if(length(grep("hot",list02))>0)
    {
      list02<-list02[-grep("hot",list02)]
    }
    # end if
    if(length(grep("endic",list02))>0)
    {
      list02<-list02[-grep("endic",list02)]
    }
    # end if
    if(length(grep("target",list02))>0)
    {
      list02<-list02[-grep("target",list02)]
    }
    # end if
    if(length(grep("class",list02))>0)
    {
      list02<-list02[-grep("class",list02)]
    }
    # end if
  }
  # end if
  
  if(selectSociety==1)
  {
    
    print("Start society part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=257&sid1=102&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=257&sid1=102&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list03<-c(list03,listif)
        
        Sys.sleep(CTime)
        list03<-unlist(list03)
        list03<-unique(list03)
        lennow<-length(list03)
        print(paste("I'm scraping society part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list03<-gsub('<a href=\"','',list03)
    list03<-gsub('\">','',list03)
    list03<-substr(list03,1,108)
    if(length(grep("hot",list03))>0)
    {
      list03<-list03[-grep("hot",list03)]
    }
    # end if
    if(length(grep("endic",list03))>0)
    {
      list03<-list03[-grep("endic",list03)]
    }
    # end if
    if(length(grep("target",list03))>0)
    {
      list03<-list03[-grep("target",list03)]
    }
    # end if
    if(length(grep("class",list03))>0)
    {
      list03<-list03[-grep("class",list03)]
    }
    # end if
  }
  # end if
  
  if(selectCulture==1)
  {
    
    print("Start culture part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=245&sid1=103&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=245&sid1=103&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list04<-c(list04,listif)
        
        Sys.sleep(CTime)
        list04<-unlist(list04)
        list04<-unique(list04)
        lennow<-length(list04)
        print(paste("I'm scraping culture part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list04<-gsub('<a href=\"','',list04)
    list04<-gsub('\">','',list04)
    list04<-substr(list04,1,108)
    if(length(grep("hot",list04))>0)
    {
      list04<-list04[-grep("hot",list04)]
    }
    # end if
    if(length(grep("endic",list04))>0)
    {
      list04<-list04[-grep("endic",list04)]
    }
    # end if
    if(length(grep("target",list04))>0)
    {
      list04<-list04[-grep("target",list04)]
    }
    # end if
    if(length(grep("class",list04))>0)
    {
      list04<-list04[-grep("class",list04)]
    }
    # end if
  }
  # end if
  
  if(selectWorld==1)
  {
    
    print("Start world news part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=322&sid1=104&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=322&sid1=104&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list05<-c(list05,listif)
        
        Sys.sleep(CTime)
        list05<-unlist(list05)
        list05<-unique(list05)
        lennow<-length(list05)
        print(paste("I'm scraping world news part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list05<-gsub('<a href=\"','',list05)
    list05<-gsub('\">','',list05)
    list05<-substr(list05,1,108)
    if(length(grep("hot",list05))>0)
    {
      list05<-list05[-grep("hot",list05)]
    }
    # end if
    if(length(grep("endic",list05))>0)
    {
      list05<-list05[-grep("endic",list05)]
    }
    # end if
    if(length(grep("target",list05))>0)
    {
      list05<-list05[-grep("target",list05)]
    }
    # end if
    if(length(grep("class",list05))>0)
    {
      list05<-list05[-grep("class",list05)]
    }
    # end if
  }
  # end if
  
  if(selectScience==1)
  {
    
    print("Start science part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=228&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=228&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list06<-c(list06,listif)
        
        Sys.sleep(CTime)
        list06<-unlist(list06)
        list06<-unique(list06)
        lennow<-length(list06)
        print(paste("I'm scraping IT/Science part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list06<-gsub('<a href=\"','',list06)
    list06<-gsub('\">','',list06)
    list06<-substr(list06,1,108)
    if(length(grep("hot",list06))>0)
    {
      list06<-list06[-grep("hot",list06)]
    }
    # end if
    if(length(grep("endic",list06))>0)
    {
      list06<-list06[-grep("endic",list06)]
    }
    # end if
    if(length(grep("target",list06))>0)
    {
      list06<-list06[-grep("target",list06)]
    }
    # end if
    if(length(grep("class",list06))>0)
    {
      list06<-list06[-grep("class",list06)]
    }
    # end if
  }
  # end if
  
  if(selectIT==1)
  {
    
    print("Start IT part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=230&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=230&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[500:1000]
        test<-test[-grep("img",test)]
        listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
        list07<-c(list07,listif)
        
        Sys.sleep(CTime)
        list07<-unlist(list07)
        list07<-unique(list07)
        lennow<-length(list07)
        print(paste("I'm scraping IT/Science part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
    
    list07<-gsub('<a href=\"','',list07)
    list07<-gsub('\">','',list07)
    list07<-substr(list07,1,108)
    if(length(grep("hot",list07))>0)
    {
      list07<-list07[-grep("hot",list07)]
    }
    # end if
    if(length(grep("endic",list07))>0)
    {
      list07<-list07[-grep("endic",list07)]
    }
    # end if
    if(length(grep("target",list07))>0)
    {
      list07<-list07[-grep("target",list07)]
    }
    # end if
    if(length(grep("class",list07))>0)
    {
      list07<-list07[-grep("class",list07)]
    }
    # end if
  }
  # end if
  
  
  
  if(selectSports==1)
  {
    
    print("Start sports part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://sports.news.naver.com/sports/index.nhn?category=sports_general&ctg=news&mod=lst&type=news&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://sports.news.naver.com/sports/index.nhn?category=sports_general&ctg=news&mod=lst&type=news&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[698:1300]
        test<-test[grep("<tr><td><a href=\"",test)]
        listif<-paste0("http://sports.news.naver.com/", substr(test, 35, 128))
        list08<-c(list08,listif)
        
        Sys.sleep(CTime)
        list08<-unlist(list08)
        list08<-unique(list08)
        lennow<-length(list08)
        print(paste("I'm scraping sports part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
  }
  # end if
  
  list08Len<-length(list08)
  list08<-list08[1:list08Len-1]
  
  if(selectEntertain==1)
  {
    
    print("Start entertainment part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-as.character(dd)
      len<-0
      lennow<-1
      i<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        
        options(warn=-1)
        test<-tryCatch(eval(parse(text=paste0("readLines('http://entertain.naver.com/home/mainNews?date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://entertain.naver.com/home/mainNews?date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-test[grep('class="tit"',test)]
        listif<-paste0("http://entertain.naver.com", substr(test, 12, 39))
        list09<-c(list09,listif)
        
        Sys.sleep(CTime)
        list09<-unlist(list09)
        list09<-unique(list09)
        lennow<-length(list09)
        print(paste("I'm scraping entertainment part",lennow,"links. I'm at",kk,"date,",i,"page."))
        i<-i+1
        
      }
      # end while
    }
    # end for
  }
  # end if
  
  list09Len<-length(list09)
  list09<-list09[1:list09Len-1]
  
  if(selectWeather==1)
  {
    
    print("Start weather part.")
    
    for(j in 0:end)
    {
      dd<-as.Date(j, origin = startDate)
      kk<-as.character(dd)
      lennow<-1
      
      ncnt<-1
      options(warn=-1)
      test<-tryCatch(eval(parse(text=paste0("readLines('http://weather.naver.com/news/wetrNewsList.nhn?ymd=",kk,"',warn=F,encoding='UTF-8')"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
      if(grepl("Read error",test))
      {
        while(ncnt>3)
        {
          Sys.sleep(1)
          test<-tryCatch(eval(parse(text=paste0("readLines('http://weather.naver.com/news/wetrNewsList.nhn?ymd=",kk,"',warn=F,,encoding='UTF-8')"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
          ncnt<-ncnt+1
        }
        # end while
      }
      # end if
      options(warn=1)
      
      test<-test[-grep("img",test)]
      test<-test[grep("¸ÞÀÎ´º½º",test):grep("´º½º¸®½ºÆ®",test)[2]]
      test<-unique(gsub("\t","",test[grep("href",test)]))
      test<-gsub('<a href=\"',"",test)
      test<-gsub('\">',"",test)
      test<-paste("http://weather.naver.com/news/",test)
      listif<-gsub(' ',"",test)
      list10<-c(list10,listif)
      
      Sys.sleep(CTime)
      list10<-unlist(list10)
      list10<-unique(list10)
      lennow<-length(list10)
      print(paste("I'm scraping weather part",lennow,"links. I'm at",kk,"date."))
      
      
    }
    # end for
  }
  # end if
  
  
  if(selectFinance==100)
  {
    
    print("Start Finance part.")
    
    for(j in 0:end)
    {
      flist<-c(401,402,403,404,406,429)
      dd<-as.Date(j, origin = startDate)
      kk<-gsub('-',"",as.character(dd))
      len<-0
      lennow<-1
      i<-1
      for(l in 1:6)
      {   fnum<-flist[l]
      len<-0
      lennow<-1
      while(len!=lennow)
      {
        
        len<-lennow
        ncnt<-1
        options(warn=-1)
        
        test<-tryCatch(eval(parse(text=paste0("readLines('http://finance.naver.com/news/news_list.nhn?mode=LSS3D&section_id=101&section_id2=258&section_id3=",fnum,"&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",test))
        {
          while(ncnt>3)
          {
            Sys.sleep(1)
            test<-tryCatch(eval(parse(text=paste0("readLines('http://finance.naver.com/news/news_list.nhn?mode=LSS3D&section_id=101&section_id2=258&section_id3=",fnum,"&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        test<-c(test[grep('<dt class="articleSubject">',test)+1],test[grep('<dd class="articleSubject">',test)+1])
        test<-unique(gsub("\t","",test))
        listif<-unique(substr(test,10,150))
        list10<-c(list10,listif)
        
        Sys.sleep(CTime)
        list10<-unlist(list10)
        list10<-unique(list10)
        lennow<-length(list10)
        print(paste("I'm scraping finance part",lennow,"links. I'm at",kk,"date,",i,"page.",fnum))
        i<-i+1
        
      }
      # end while
      }
      # end for
    }
    
  }
  
  listAll<-c(list01,list02,list03,list04,list05,list06,list07,list08,list09,list10)
  write.csv(listAll,"listURL.csv",row.names=F)
  print(paste("We get",length(listAll),"links!"))
  print(paste("Get url list is Done! It is saved at",getwd(),"named listURL.csv."))
  listLen<-length(listAll)
  listAll<<-c(list01,list02,list03,list04,list05,list06,list07)
  
  ##########################################################################################################
  ########################                                                     #############################
  ########################                 step 2 getting contents             #############################
  ########################                                                     #############################
  ##########################################################################################################
  
  print("Start step 2. Crawling contents. This is last step.")
  dataAll<-data.frame(category=8,title="test",author="test",postTime="time",chgTime="time",contents="test")
  errorURL<-list()
  dd<-length(listAll)
  if(listLen>0)
    # end if
  {
    if(length(listAll)>0)
    {
      for(j in 1:dd)
      {
        tt<-as.character(listAll[j])
        if(nchar(tt)==144|nchar(tt)==138)
        {
          substr(tt,1,108)->tt
        }
        # end if
        
        ncnt<-1
        options(warn=-1)
        tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",tst))
        {
          while(ncnt<3)
          {
            Sys.sleep(1)
            tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        if(ncnt==3)
        {
          errorURL<<-c(errorURL,tt)
        }
        else
        {
          tst<-gsub('&nbsp;',"",tst)
          tst<-gsub('&lt;',"<",tst)
          tst<-gsub('&gt;',">",tst)
          tst<-gsub('&amp;',"&",tst)
          tst<-gsub('&quot;','"',tst)
          tst<-gsub('&#034','"',tst)
          
          cate<-tst[grep(":category2",tst)]
          title<-tst[grep("og:title",tst)]
          author<-tst[grep(":author",tst)]
          postTime<-tst[grep("±â»çÀÔ·Â",tst)]
          chgTime<-tst[grep("ÃÖÁ¾¼öÁ¤",tst)]
          if(length(grep("º»¹® ³»¿ë",tst))>1)
          {
            con<-tst[grep("º»¹® ³»¿ë",tst)[1]:grep("º»¹® ³»¿ë",tst)[2]]
          }
          else
          {
            con<-tst[grep("[(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]",tst)[ grep("[(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]",tst)>grep("º»¹® ³»¿ë",tst)][1]]
          }
          # end if
          
          cate<-gsub("[^(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]","",cate)
          title<-gsub('<meta property=\"og:title\"\t\t\tcontent=\"',"",title)
          title<-gsub('\"/>',"",title)
          author<-gsub('<meta property=\"og:article:author\"\tcontent=\"',"",author)
          author<-gsub('\"/>',"",author)
          author<-gsub(" ³×ÀÌ¹ö ´º½º","",author)
          author<-gsub("[[:punct:]]", "", author)
          author<-gsub(" ", "", author)
          postTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",postTime)
          postTime<-gsub("±â»çÀÔ·Â ","",postTime)
          if (length(chgTime)!=0)
          {
            chgTime<-gsub("\t","",chgTime)
            chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
            chgTime<-gsub("ÃÖÁ¾¼öÁ¤ ","",chgTime)
          }
          else
          {
            chgTime<-postTime
          }
          # end if
          con<-gsub("\t","",con)
          con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
          con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
          con<-paste(con, collapse = "")
          
          options(warn=-1)
          listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
          if(!grepl("Read error",listTem))
          {
            dataAll<-rbind(dataAll,listTem)
            print(paste(cate,j,j/dd*100,"%"))
          }
          
          else
          {
            print(paste("This link is not right.",tt))
            print(paste(cate,j,j/dd*100,"% above"))
            errorURL<<-c(errorURL,tt)
          }
          # end if
          options(warn=1)
          Sys.sleep(CTime)
        }
        # end if
      }
      # end for
    }
    # end if
    if(selectSports==1)
    {
      dd<-length(list07)
      for(j in 1:dd)
      {
        tt<-list07[j]
        ncnt<-1
        options(warn=-1)
        tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",tst))
        {
          while(ncnt<3)
          {
            Sys.sleep(1)
            tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        
        if(ncnt==3)
        {
          errorURL<<-c(errorURL,tt)
        }
        else
        {
          tst<-gsub('&nbsp;',"",tst)
          tst<-gsub('&lt;',"<",tst)
          tst<-gsub('&gt;',">",tst)
          tst<-gsub('&amp;',"&",tst)
          tst<-gsub('&quot;','"',tst)
          tst<-gsub('&#034','"',tst)
          
          title<-tst[grep("og:title",tst)]
          author<-tst[grep(":author\"",tst)]
          postTime<-tst[grep("±â»çÀÔ·Â",tst)+1]
          chgTime<-tst[grep("ÃÖÁ¾¼öÁ¤",tst)]
          if(length(grep("±â»ç ³»¿ë",tst))>1)
          {
            con<-tst[grep("±â»ç ³»¿ë",tst)[1]:grep("±â»ç ³»¿ë",tst)[2]]
          }
          else
          {
            con<-tst[grep("[(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]",tst)[ grep("[(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]",tst)>grep("±â»ç ³»¿ë",tst)][1]]
          }
          # end if
          
          cate<-"½ºÆ÷Ã÷"
          title<-gsub('<meta property=\"og:title\"       content=\"',"",title)
          title<-gsub('\"/>',"",title)
          author<-gsub('<meta property=\"og:article:author\" content=\"',"",author)
          author<-gsub('\"/>',"",author)
          author<-gsub("³×ÀÌ¹ö ½ºÆ÷Ã÷ ","",author)
          author<-gsub("[[:punct:]]", "", author)
          author<-gsub(" ", "", author)
          postTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",postTime)
          postTime<-gsub(" ","",postTime)
          postTime<-paste0(substr(postTime,1,10)," ",substr(postTime,11,15))
          if (length(chgTime)!=0)
          {
            chgTime<-gsub("\t","",chgTime)
            chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
            chgTime<-gsub("[[:punct:]]", "", chgTime)
            chgTime<-gsub("ÃÖÁ¾¼öÁ¤", "", chgTime)
            chgTime<-gsub(" ", "", chgTime)
            chgTime<-paste0(substr(chgTime,1,4),"-",substr(chgTime,5,6),"-",substr(chgTime,7,8)," ",substr(chgTime,9,10),":",substr(chgTime,11,12))
          }
          else
          {
            chgTime<-postTime
          }
          # end if
          con<-gsub("\t","",con)
          con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
          con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
          con<-paste(con, collapse = "")
          
          options(warn=-1)
          listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
          if(!grepl("Read error",listTem))
          {
            
            dataAll<-rbind(dataAll,listTem)
            print(paste(cate,j,j/dd*100,"%"))
          }
          
          else
          {
            print(paste("This link is not right.",tt))
            print(paste(cate,j,j/dd*100,"% above"))
            errorURL<<-c(errorURL,tt)
          }
          # end if
          options(warn=1)
          Sys.sleep(CTime)
        }
        # end if
      }
      # end for
    }
    # end if
    
    if(selectEntertain==1)
    {
      dd<-length(list08)
      for(j in 1:dd)
      {
        tt<-as.character(list08[j])
        if(nchar(tt)==144|nchar(tt)==138)
        {
          substr(tt,1,108)->tt
        }
        # end if
        ncnt<-1
        options(warn=-1)
        tst<-tryCatch(readLines(tt,warn=F,encoding="UTF-8"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",tst))
        {
          while(ncnt<3)
          {
            Sys.sleep(1)
            tst<-tryCatch(readLines(tt,warn=F,encoding="UTF-8"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        charSet<-length(grep("<meta charset=",tst))
        if(charSet!=0)
        {
          options(warn=-1)
          tst<-tryCatch(readLines(tt,warn=F,encoding="EUC-KR"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
          if(grepl("Read error",tst))
          {
            while(ncnt<3)
            {
              Sys.sleep(1)
              tst<-tryCatch(readLines(tt,warn=F,encoding="EUC-KR"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
              ncnt<-ncnt+1
            }
            # end while
          }
          # end if
          options(warn=1)
          
        }
        else
        {
          
          if(ncnt==3)
          {
            errorURL<<-c(errorURL,tt)
          }
          else
          {
            
            
            tst<-gsub('&nbsp;',"",tst)
            tst<-gsub('&lt;',"<",tst)
            tst<-gsub('&gt;',">",tst)
            tst<-gsub('&amp;',"&",tst)
            tst<-gsub('&quot;','"',tst)
            tst<-gsub('&#039;',"'",tst)
            tst<-gsub('&#034','"',tst)
            
            cate<-"TV¿¬¾Ö"
            title<-tst[grep("og:title",tst)]
            author<-tst[grep(":author",tst)]
            postTime<-tst[grep("±â»çÀÔ·Â",tst)]
            chgTime<-tst[grep("ÃÖÁ¾¼öÁ¤",tst)]
            if(length(grep("º»¹® ³»¿ë",tst))>1)
            {
              con<-tst[grep("º»¹® ³»¿ë",tst)[1]:grep("º»¹® ³»¿ë",tst)[2]]
            }
            else
            {
              con<-tst[grep("[(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]",tst)[ grep("[(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]",tst)>grep("º»¹® ³»¿ë",tst)][1]]
            }
            # end if
            
            cate<-gsub("[^(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]","",cate)
            title<-gsub('<meta property=\"og:title\"\t\t\tcontent=\"',"",title)
            title<-gsub('\"/>',"",title)
            author<-gsub('<meta property=\"og:article:author\"\tcontent=\"',"",author)
            author<-gsub('\"/>',"",author)
            author<-gsub("³×ÀÌ¹öTV¿¬¿¹","",author)
            author<-gsub("[[:punct:]]", "", author)
            author<-gsub(" ", "", author)
            
            postTime<-gsub("\t","",postTime)
            postTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",postTime)
            postTime<-gsub("±â»çÀÔ·Â","",postTime)
            if (length(chgTime)!=0)
            {
              chgTime<-gsub("\t","",chgTime)
              chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
              chgTime<-gsub("ÃÖÁ¾¼öÁ¤","",chgTime)
            }
            else
            {
              chgTime<-postTime
            }
            # end if
            con<-gsub("\t","",con)
            con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
            con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
            if(length(grep("script",con))==2)
            {
              con<-con[c(-grep("script",con)[1]:-grep("script",con)[2])]
            }
            # end if
            con<-paste(con, collapse = "")
            con<-gsub("<!--.*-->","",con)
            
            options(warn=-1)
            listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
            if(!grepl("Read error",listTem))
            {
              
              dataAll<-rbind(dataAll,listTem)
              print(paste(cate,j,j/dd*100,"%"))
            }
            else
            {
              print(paste("This link is not right.",tt))
              print(paste(cate,j,j/dd*100,"% above"))
              errorURL<<-c(errorURL,tt)
            }
            # end if
            options(warn=1)
            Sys.sleep(CTime)
          }
          # end if
        }
        # end if
      }
      # end for
    }
    # end if
    
    if(selectWeather==1)
    {
      
      dd<-length(list09)
      for(j in 1:dd)
      {
        tt<-as.character(list09[j])
        ncnt<-1
        options(warn=-1)
        tst<-tryCatch(readLines(tt,warn=F,encoding="UTF-8"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
        if(grepl("Read error",tst))
        {
          while(ncnt<3)
          {
            Sys.sleep(1)
            tst<-tryCatch(readLines(tt,warn=F,encoding="UTF-8"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
            ncnt<-ncnt+1
          }
          # end while
        }
        # end if
        options(warn=1)
        charSet<-length(grep("<meta charset=",tst))
        if(charSet!=0)
        {
          options(warn=-1)
          tst<-tryCatch(readLines(tt,warn=F,encoding="EUC-KR"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
          if(grepl("Read error",tst))
          {
            while(ncnt<3)
            {
              Sys.sleep(1)
              tst<-tryCatch(readLines(tt,warn=F,encoding="EUC-KR"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
              ncnt<-ncnt+1
            }
            # end while
          }
          # end if
          options(warn=1)
          
        }
        else
        {
          
          if(ncnt==3)
          {
            errorURL<<-c(errorURL,tt)
          }
          else
          {
            tst<-gsub('&nbsp;',"",tst)
            tst<-gsub('&lt;',"<",tst)
            tst<-gsub('&gt;',">",tst)
            tst<-gsub('&amp;',"&",tst)
            tst<-gsub('&quot;','"',tst)
            tst<-gsub('&#039;',"'",tst)
            tst<-gsub('&#034','"',tst)
            
            cate<-tst[grep("<title>",tst)]
            title<-tst[grep('<h4 class="new_end_tit">',tst)]
            author<-tst[grep('<dd class="date">',tst)]
            postTime<-tst[grep('<dd class="date">',tst)]
            con<-tst[grep('<div class="data">',tst)+1]
            
            cate<-gsub("[^(°¡-ÆR¤¡-¤¾¤¿-¤Ó)]","",cate)
            cate<-gsub(" ","",cate)
            cate<-gsub("³×ÀÌ¹ö","",cate)
            title<-gsub('\t',"",title)
            title<-gsub("<[^>]*>","",title)
            
            author<-gsub("\t","",author)
            author<-gsub('<[^>]*>',"",author)
            author<-gsub("\\[","",author)
            author<-gsub(" ]","",author)
            author<-gsub("(19|20)..-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[0-1]) (0[0-9]|1[0-9]|2[01234]):(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]):(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9])","",author)
            author<-gsub(" ","",author)
            
            postTime<-unlist(strsplit(postTime, ' '))
            postTime<-paste(postTime[grep("(19|20)..-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[0-1])",postTime)],postTime[grep("(0[0-9]|1[0-9]|2[01234]):(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]):(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9])",postTime)])
            chgTime<-postTime
            
            con<-gsub("\t","",con)
            con<-gsub("<ul>.*</ul>","",con)
            con<-gsub("<h3>.*</h3>","",con)
            con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
            
            options(warn=-1)
            listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
            if(!grepl("Read error",listTem))
            {
              
              dataAll<-rbind(dataAll,listTem)
              print(paste(cate,j,j/dd*100,"%"))
            }
            else
            {
              print(paste("This link is not right.",tt))
              print(paste(cate,j,j/dd*100,"% above"))
              errorURL<<-c(errorURL,tt)
            }
            # end if
            options(warn=1)
            Sys.sleep(CTime)
          }
          # end if
        }
        # end for
      }
    }
    # end if
    
    
    dataAll<<-dataAll[-1,]
    
    print(paste("Get contents is Done! It is saved at",getwd(),"named dataAll.csv."))
  }
  else
  {
    print("No url collected.")
  }
}
getNaverNews()


x = data.frame(listAll)
listAll = t(x)
#rownames(listAll) <- c(2:21)
#colnames(listAll) <- "Link"
#dataAll = cbind(dataAll,listAll)

write.csv(dataAll,"dataAll.csv",row.names=F)


con <- dbConnect(MySQL(), user="root", password="bigdata01", dbname="bigdata", host="165.194.35.165",DBMSencoding="UTF-8")
dbListTables(con)
#Æ¯¼ö¹®ÀÚ Á¦°Å ÄÚµå
#str_replace_all(x, "[[:punct:]]", " ")
#str_replace_all(x, "[^[:alnum:]]", " ")
dataAll <- data.frame(lapply(dataAll, as.character), stringsAsFactors=FALSE)

dbWriteTable(con, value = dataAll, name = "naver_article", append = TRUE, row.names = FALSE)

#
dbDisconnect(con)



#»ç¿ëÀÚ Á¤ÀÇÇÔ¼ö
rm_en<- content_transformer(function(x,pattern) gsub(pattern, "", x))
rm_en2<- content_transformer(function(x,pattern,pattern2) gsub(pattern, pattern2, x))
extract_ko<- content_transformer(function(x) {sapply(x, function(a)paste(extractNoun(a), collapse=" "))})
rm_one<- content_transformer(function(x) Filter(function(x){nchar(x)>8},x)) 
#rm_two<- content_transformer(function(x) Filter(function(x){nchar(x)<9},x)) 
harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
trim.leading <- function (x)  sub("^\\s+", "", x)

#-----------------------------------------------------------------------------------------
#db¿¡¼­ ´º½º µ¥ÀÌÅÍ °¡Á®¿À´Â ÄÚµå


con <- dbConnect(MySQL(), user="root", password="bigdata01", dbname="bigdata", host="165.194.35.165",DBMSencoding="utf-8")
dbListTables(con)

dbGetQuery(con,"SET NAMES euckr")
#³¯Â¥ ¹Ù²ãÁà¾ßµÊ
res <- dbSendQuery(con, "SELECT * FROM `naver_article` WHERE `postTime` LIKE '2015-11-18%' LIMIT 0,50000")

articles<-dbFetch(res,n=-1)

dbDisconnect(con)

#´Ü¾îÃß°¡
useSejongDic()
mergeUserDic(data.frame(c("¸Þ¸£½º","Áßµ¿È£Èí±âÁõÈÄ±º","À¯´Ï¹ö½Ã¾Æµå","ºÏÇÑ","Ã»¿Í´ë","¸ê°­Ãæ"),"ncn"))
mergeUserDic(data.frame(c("»õÁ¤Ä¡","»õ´©¸®´ç","¿ø³»´ëÇ¥","±¹Á¤¿ø","±¹°¡Á¤º¸¿ø","»õÁ¤Ä¡¹ÎÁÖ¿¬ÇÕ","»õÁ¤Ä¡ ¹ÎÁÖ ¿¬ÇÕ","±×¸®½º"),"ncn"))
mergeUserDic(data.frame(c("¾ÈÃ¶¼ö", "¹Ú±ÙÇý","¹®ÀçÀÎ","±è¹«¼º","À¯½Â¹Î","±èÀÏ¼º","±èÁ¤ÀÏ","±èÁ¤Àº","ÀÌÈñÈ£","±èÀåÈÆ","Ä¡ÇÁ¶ó½º","¸Þ¸£ÄÌ","µ¥ÀÌ¼¿ºí·ë"), "nqpc"))

##ÇÑ±¹¾î stopwords?
kr_stop <- read.csv("~/bd/stopwords_kr.csv", header=F, stringsAsFactors = F)
kr_stop<- kr_stop[1:length(kr_stop[,1]),1]
kr_stop

#ºÐ¼® È¯°æ¿¡¼­ÀÇ °øÅë º¯¼ö ¼±¾ð
SEED<-2015
burnin <- 1000
iter <- 1000
keep <- 50

# ÆÄÆ®º° ÅäÇÈ °ËÃâ ÅäÇÈ ¹Ù²ãÁã¾ßµÊ.(Á¤Ä¡, »çÈ¸, °úÇÐ, °æÁ¦, ¼¼°è Á¤µµ)
global<-DataframeSource(as.data.frame(articles$contents[articles$category=="»çÈ¸"],stringsAsFactors=FALSE))
#global_link<-DataframeSource(as.data.frame(articles$V6[articles$category=="Á¤Ä¡"],stringsAsFactors=FALSE))
global_title<-DataframeSource(as.data.frame(articles$title[articles$category=="»çÈ¸"],stringsAsFactors=FALSE))
global<-VCorpus(global)
global<-tm_map(global, removePunctuation)
global<-tm_map(global, removeNumbers)
global<-tm_map(global, rm_en2,"( )[°¡-ÆR]( )", " ")
global<-tm_map(global, rm_en,"^[°¡-ÆR]( )")
global<-tm_map(global, rm_en,"( )[°¡-ÆR]$")
global<-tm_map(global, stripWhitespace)
global<-tm_map(global, rm_en, "?[A-Za-z0-9]?")
global<-tm_map(global, rm_en2, "[^°¡-ÆR]"," ")
global<-tm_map(global, rm_one)
#global<-tm_map(global, rm_two)

global<-tm_map(global,extract_ko)
global<-tm_map(global, removeWords,stopwords("english"))
global<-tm_map(global, removeWords,kr_stop)


global<-tm_map(global, removePunctuation)
global<-tm_map(global, removeNumbers)
global<-tm_map(global, rm_en2,"( )[°¡-ÆR]( )", " ")
global<-tm_map(global, rm_en,"^[°¡-ÆR]( )")
global<-tm_map(global, rm_en,"( )[°¡-ÆR]$")
global<-tm_map(global, stripWhitespace)
global<-tm_map(global, rm_en, "?[A-Za-z0-9]?")
global<-tm_map(global, rm_en2, "[^°¡-ÆR]"," ")




dtm_global <- DocumentTermMatrix(global,control = list(stemming=TRUE, stopwords=TRUE, minWordLength= 3, removeNumbers=TRUE, removePunctuation=TRUE))
dtm_global

#lda¿¡¼­´Â tfidf»ç¿ë ¾ÈÇÔ, µû¶ó¼­ tfidf´Â ¼öµ¿À¸·Î ±¸ÇØ¼­ termÀ» °É·¯³»´Â µ¥¿¡¸¸ ¾´´Ù
#tf·Î¸¸ °è»êµÈ dtm¿¡¼­ tf-idf·Î °É·¯³»´Â ÄÚµå
dtm_global_term_tfidf<-DocumentTermMatrix(global,control=list(weighting=function(x) weightTfIdf(x, normalize = T),stopwords=T))

dtm_global_term_tfidf





#°¢ termÀÇ À¯´ÏÅ©ÇÑ ÃÖ´ë°ªÀ» °í·ÁÇÏ´Â ÄÚµå
#summary(as.numeric(substr(summary(as.matrix(dtm_global_term_tfidf))[6,],9,17)))
#words<-names(summary(as.matrix(dtm_global_term_tfidf))[6,as.numeric(substr(summary(as.matrix(dtm_global_term_tfidf))[6,],9,17))>7.094])
#words<-trim.leading(words)
#length(words)

#°¢ termÀÇ colsumÀ» È°¿ëÇÏ´Â ÄÚµå
third_quantile<-summary(col_sums(dtm_global_term_tfidf))[5]
third_quantile
max_value <- summary(col_sums(dtm_global_term_tfidf))[6]
max_value
dtm_global_term_tfidf <- dtm_global_term_tfidf[,col_sums(dtm_global_term_tfidf)>0.95*third_quantile+0.05*max_value]
dtm_global_term_tfidf

#removesparstermÀº 3¿É¼Å
#dtm_global_term_tfidf <- removeSparseTerms(dtm_global_term_tfidf, sparse = 0.99)
hist(col_sums(dtm_global_term_tfidf),main="Hist of Normalized TF-IDF Score",xlab="Normalized TF-IDF Score")
#hist(col_sums(dtm_global_term_tfidf_noNormal),main="Hist of Non-Normalized TF-IDF Score",xlab="Non-Normalized TF-IDF Score")
summary(col_sums(dtm_global_term_tfidf))
#summary(col_sums(dtm_global_term_tfidf_noNormal))




dtm_global_term_tfidf<-dtm_global_term_tfidf[row_sums(dtm_global_term_tfidf)>0,]#¾Æ¹«°Íµµ ¾È°É¸®´Â ¹®¼­µé ³¯¸®
dtm_global_term_tfidf

words<-colnames(dtm_global_term_tfidf)

dtm_global<-dtm_global[,is.element(colnames(dtm_global),words)]
dtm_global<-dtm_global[row_sums(dtm_global)>0,]#¾Æ¹«°Íµµ ¾È°É¸®´Â ¹®¼­µé ³¯¸®

dim(dtm_global)

sequ<-seq(2,50,1)
fitted_many<-lapply(sequ,function(k)LDA(dtm_global,k=k,method = "Gibbs",control = list(seed=SEED,burnin=burnin,iter=iter,keep=keep)))
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
plot(sequ, hm_many, type = "l",main="Plot of Harmonic Mean",xlab="Number of Topic",ylab="Harmonic mean of Likelihood")
best.k<-sequ[hm_many==max(hm_many)]
best.k

Gibbs=LDA(dtm_global,k=best.k,method = "Gibbs", control = list(seed=SEED,burnin=burnin,iter=iter,keep=keep))
Topic_global<-topics(Gibbs,1)
Topic_global
Terms_global<-terms(Gibbs,3)
Terms_global

###################################################################

m1 = topics(Gibbs,3)
m1 = m1[,1:best.k]
m1 = as.data.frame(m1)
m1 = t(m1)

m2 = terms(Gibbs,3)
m2 = m2[,1:best.k]
m2 = as.data.frame(m2)
m2 = t(m2)

c1 = cbind(m1[,1],m2[,1])
c2 = cbind(m1[,2],m2[,2])
c3 = cbind(m1[,3],m2[,3])

d = data.frame(c(1:best.k))
e = data.frame(c(1:best.k))

colnames(d)<-("date")
colnames(e)<-("category")
for(j in 1:best.k)
{
  d[j,1] = "2015-11-18" #³¯Â¥ ¹Ù²ãÁà¾ßµÊ
  e[j,1] = "»çÈ¸" 	      #ºÐ¾ßµµ ¹Ù²ãÁà¾ßµÊ.	
}

colnames(c1)<-c("tf1","keyword1")
colnames(c2)<-c("tf2","keyword2")
colnames(c3)<-c("tf3","keyword3")

c = cbind(c1,c2,c3)
c = cbind(e,d,c)
con <- dbConnect(MySQL(), user="root", password="bigdata01", dbname="bigdata", host="165.194.35.165",DBMSencoding="UTF-8")
dbListTables(con)

#str_replace_all(x, "[[:punct:]]", " ")
#str_replace_all(x, "[^[:alnum:]]", " ")
c <- as.data.frame(c)

dbWriteTable(con, value = c, name = "count", append = TRUE, row.names = FALSE)

#
dbDisconnect(con)

