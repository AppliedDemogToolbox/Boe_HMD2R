
HMD2R <- function(CNTRY=NULL,username, password, wanteditems=NULL, drop.tadj=TRUE){
  ## fetches all non-aggregated data for the specified country
  ## from the HMD website, using the username and password
  ## of the registered user.  NB: passwords are not sent encrypted

  ## With no arguments, prints a list of available country codes and
  ## returns a list of country code (invisible)

  ## wanteditems is a char vector of the data chunks desired,
  ## e.g. c("E0per","fltper_1x1") if only certain statistics
  ## are desired.
  
  ## This function uses RCurl library.  Install using either
  ##     install.packages("RCurl", dep=TRUE)
  ## or
  ##     source("http://bioconductor.org/biocLite.R")
  ##     biocLite("RCurl")

  ## Carl Boe, Human Mortality Database, 2011
  
  ## There is no warranty for this code
  ## Last updated: June 2011

  require(RCurl)
  urlbase<- "http://www.mortality.org/hmd"

 
  tf <- tempfile();
  on.exit(unlink(tf))

  
  if(missing(CNTRY) || is.null(CNTRY)){
    this.url<- "http://www.mortality.org/countries.csv";
    cat(getURL(this.url),file=tf)
    ctrylist<- read.csv(file=tf,header=TRUE,as.is=TRUE);
    tmp<-data.frame(Country=ctrylist$Country, CCode=ctrylist$Subpop.Code.1);
    cat("** Country code CNTRY required! \nHMD Countries and Country Codes\n\n");
    print(tmp,quote=FALSE);
    print("Usage: HMD2R(CNTRY,hmdusername, hmdpassword [,wanteditems])")
    print("  where 'wanteditems' is a set of desired HMD statistics, e.g. c('Mx_5x5','E0per').")  
    print("  If omitted a structure of all 1x1 data for the country is returned.")
    return(invisible(tmp));
  }

  if(length(CNTRY) > 1)
    stop("Multiple country call not supported currently")
  
  if(missing(username) || missing(password) || is.null(username) || is.null(password))
    stop("username and password required for HMD access")

 
  this.pw <- paste(username,password,sep=":");

  ## reuse handle, reduce connection starts
  handle <- getCurlHandle(userpwd=this.pw)

  f.fetchit<- function(stat=this.stat,this.skip=2){
    cat(paste("  *** Fetching...",stat,"\n"))
    this.stat.txt <- paste(stat,".txt",sep="");
    this.url<- paste(urlbase,CNTRY,"STATS",this.stat.txt,sep="/")
    stat.return<- NULL;
    x<-NULL;
    stat.return<-tryCatch(getURL(this.url,  curl=handle),
                          error = function(e) {
                                   cat("HTTP error: ", e$message, "\n")
                                 }
                          );
    if(! is.null(stat.return)){         #something was returned from server
      cat(stat.return,file=tf);
      x.header<- scan(file=tf,what="character",nlines=1,sep = "\n",quiet=TRUE);
      
      if( grepl('Last modified:',x.header)){ # has HMD table been returned
        x<- read.table(file=tf,header=TRUE,skip=this.skip, as.is=TRUE,na.string=".")
      } else {
        ## either no cohort lifetable data or more general problem
        if(stat %in% c("fltcoh_1x1","mltcoh_1x1","bltcoh_1x1","E0coh")){
          warning(paste(CNTRY,stat,"..No cohort lifetable data, returning NULL"))
        } else {
          warning(paste(CNTRY,stat,"..Problem fetching data"),immediate.=TRUE);
        }
      }

    };
    return(x)
    
  } # end of f.fetchit

  
  this.stat <- "Births"
  assign(this.stat,NULL);
  if (missing(wanteditems)|| this.stat %in% wanteditems) {
    x<- f.fetchit(this.stat)
    assign(this.stat,x)
  } 
      



  ## Statistics with Age need to have 100+ mapped to 100 and converted to int
  ## cohort life tables return NULL if they do not exist
  for(this.stat in c("Deaths_1x1","Deaths_lexis","Population","Exposures_1x1",
                     "Mx_1x1","fltper_1x1","mltper_1x1","bltper_1x1",
                     "cExposures_1x1","cMx_1x1","fltcoh_1x1","mltcoh_1x1","bltcoh_1x1")){
    

    assign(this.stat,NULL);
    if (missing(wanteditems) || this.stat %in% wanteditems) {
      x<- f.fetchit(this.stat)
      if(!is.null(x))
        x$Age<-as.integer(gsub("\\+","",x$Age));
      assign(this.stat,x)
    }

  }


  ## fix for territorial adjustments in population data.  Usually, we just
  ## want to ignore the before/after distinction (keep the '+' variant)
  if(drop.tadj){
    if(!is.null("Population") || mode(Population$Year) =="character" ){
      isel<-grep("-",Population$Year);
      Population<- Population[-isel,];                 # keep all but those with '-'
      Population$Year<- gsub("\\+","",Population$Year); # strip '+'
      Population$Year<- as.integer(Population$Year);
    }
  }
      
  
  this.stat <- "E0per"
  assign(this.stat,NULL);
  if (missing(wanteditems) || this.stat %in% wanteditems) {
    x<- f.fetchit(this.stat)
    assign(this.stat,x)
  }

  this.stat <- "E0coh"  
  assign(this.stat,NULL);
  if (missing(wanteditems) || this.stat %in% wanteditems) {
    x<- f.fetchit(this.stat)
    assign(this.stat,x)
  }

  result<-list(Births=Births,
              E0per=E0per,
              Deaths_1x1=Deaths_1x1,
              Deaths_lexis =Deaths_lexis ,
              Population = Population  ,
              Exposures_1x1 =  Exposures_1x1,
              Mx_1x1 = Mx_1x1,
              fltper_1x1 = fltper_1x1,
              mltper_1x1 =mltper_1x1  ,
              bltper_1x1 = bltper_1x1 ,
              cExposures_1x1 = cExposures_1x1,
              cMx_1x1 = cMx_1x1 ,
              fltcoh_1x1 = fltcoh_1x1 ,
              mltcoh_1x1 =  mltcoh_1x1,
              bltcoh_1x1 = bltcoh_1x1,
              E0coh = E0coh);

  ## drop unwanted items (which are empty)
  if(! missing(wanteditems)){
    isel<- names(result) %in% wanteditems;
    result<- result[isel];
  }
  
  return(result);

} # end HMD2R()


#####
#####


## Examples:

args(HMD2R)

function (CNTRY = NULL, username, password, wanteditems = NULL,
   drop.tadj = TRUE)

countries <- HMD2R()   				## with no args, returns list of current HMD
                      				## countries.  No username or password required

myname<- "your HMD username"	 		## your registered HMD username
mypass<- "your HMD password"          		## mortality.org / HMD account password

## get only what you want, saves download time
USA<-HMD2R("USA",username=myname,password=mypass,wanted=c("E0per","Population"))

USA<-HMD2R("USA",username=myname,password=mypass,wanted=c("E0per","Population"))

str(USA)

## get everything for a country
USA<-HMD2R("USA",username=myname,password=mypass)

## loop example
flifetables <-vector("list",length(countries))
names(flifetables)<- countries

for( i in seq(flifetables)){
   t.ans <- HMD2R(names(flifetables)[i], myname, mypass, "fltper_1x1")
   flifetables[[i]] <- t.ans$fltper_1x1
}

## plyr to convert from list structure back to data.frame
library(plyr)
flifetables.df <- ldply(flifetables)
flifetables.df <- rename(flifetables.df, c(".id" = "Country")
