ISO1999 <- function(CDC2017NQA){
  CDC2017NQA<- within(CDC2017NQA, {
    age_adjust<- NA
    age_adjust[age < 30] <- 1
    age_adjust[age >= 30 & age < 40] <- 2
    age_adjust[age >= 40 & age < 50] <- 3
    age_adjust[age >= 50 & age < 60] <- 4
    age_adjust[age >= 60 & age < 70] <- 5
    age_adjust[age > 70] <- 6
  })
  LA500=CDC2017NQA$L500
  LA1000=CDC2017NQA$L1000
  LA2000=CDC2017NQA$L2000
  LA3000=CDC2017NQA$L3000
  LA4000=CDC2017NQA$L4000
  LA6000=CDC2017NQA$L6000
  LA8000=CDC2017NQA$L8000
  RA500=CDC2017NQA$R500
  RA1000=CDC2017NQA$R1000
  RA2000=CDC2017NQA$R2000
  RA3000=CDC2017NQA$R3000
  RA4000=CDC2017NQA$R4000
  RA6000=CDC2017NQA$R6000
  RA8000=CDC2017NQA$R8000
  CDC2017NQA=data.frame(CDC2017NQA,LA500,LA1000,LA2000,LA3000,LA4000,
                        LA6000,LA8000,RA500,RA1000,RA2000,RA3000,
                        RA4000,RA6000,RA8000)
  #500
  CDC2017NQA$LA500<-with(CDC2017NQA,LA500-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA500<-with(CDC2017NQA,LA500-ifelse(age_adjust==2,1,0))
  CDC2017NQA$LA500<-with(CDC2017NQA,LA500-ifelse(age_adjust==3,2,0))
  CDC2017NQA$LA500<-with(CDC2017NQA,LA500-ifelse(age_adjust==4,4,0))
  CDC2017NQA$LA500<-with(CDC2017NQA,LA500-ifelse(age_adjust==5,6,0))
  CDC2017NQA$LA500<-with(CDC2017NQA,LA500-ifelse(age_adjust==6,9,0))
  
  CDC2017NQA$RA500<-with(CDC2017NQA,RA500-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA500<-with(CDC2017NQA,RA500-ifelse(age_adjust==2,1,0))
  CDC2017NQA$RA500<-with(CDC2017NQA,RA500-ifelse(age_adjust==3,2,0))
  CDC2017NQA$RA500<-with(CDC2017NQA,RA500-ifelse(age_adjust==4,4,0))
  CDC2017NQA$RA500<-with(CDC2017NQA,RA500-ifelse(age_adjust==5,6,0))
  CDC2017NQA$RA500<-with(CDC2017NQA,RA500-ifelse(age_adjust==6,9,0))
  d=data.frame(CDC2017NQA$gender,CDC2017NQA$age_adjust,CDC2017NQA$age,CDC2017NQA$L500,CDC2017NQA$LA500);d
  rm(d)
  #1000
  CDC2017NQA$LA1000<-with(CDC2017NQA,LA1000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA1000<-with(CDC2017NQA,LA1000-ifelse(age_adjust==2,1,0))
  CDC2017NQA$LA1000<-with(CDC2017NQA,LA1000-ifelse(age_adjust==3,2,0))
  CDC2017NQA$LA1000<-with(CDC2017NQA,LA1000-ifelse(age_adjust==4,4,0))
  CDC2017NQA$LA1000<-with(CDC2017NQA,LA1000-ifelse(age_adjust==5,7,0))
  CDC2017NQA$LA1000<-with(CDC2017NQA,LA1000-ifelse(age_adjust==6,11,0))
  
  CDC2017NQA$RA1000<-with(CDC2017NQA,RA1000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA1000<-with(CDC2017NQA,RA1000-ifelse(age_adjust==2,1,0))
  CDC2017NQA$RA1000<-with(CDC2017NQA,RA1000-ifelse(age_adjust==3,2,0))
  CDC2017NQA$RA1000<-with(CDC2017NQA,RA1000-ifelse(age_adjust==4,4,0))
  CDC2017NQA$RA1000<-with(CDC2017NQA,RA1000-ifelse(age_adjust==5,7,0))
  CDC2017NQA$RA1000<-with(CDC2017NQA,RA1000-ifelse(age_adjust==6,11,0))
  d=data.frame(CDC2017NQA$gender,CDC2017NQA$age_adjust,CDC2017NQA$age,CDC2017NQA$L1000,CDC2017NQA$LA1000,CDC2017NQA$R1000,CDC2017NQA$RA1000);d
  rm(d)
  #2000
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(age_adjust==2,1,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(age_adjust==3,3,0))
  
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(gender==1 & age_adjust==4,7,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(gender==1 & age_adjust==5,12,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(gender==1 & age_adjust==6,19,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(gender==2 & age_adjust==4,6,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(gender==2 & age_adjust==5,11,0))
  CDC2017NQA$LA2000<-with(CDC2017NQA,LA2000-ifelse(gender==2 & age_adjust==6,16,0))
  
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(age_adjust==2,1,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(age_adjust==3,3,0))
  
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(gender==1 & age_adjust==4,7,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(gender==1 & age_adjust==5,12,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(gender==1 & age_adjust==6,19,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(gender==2 & age_adjust==4,6,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(gender==2 & age_adjust==5,11,0))
  CDC2017NQA$RA2000<-with(CDC2017NQA,RA2000-ifelse(gender==2 & age_adjust==6,16,0))
  
  #3000
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==1 & age_adjust==2,2,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==1 & age_adjust==3,6,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==1 & age_adjust==4,12,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==1 & age_adjust==5,20,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==1 & age_adjust==6,31,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==2 & age_adjust==2,1,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==2 & age_adjust==3,4,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==2 & age_adjust==4,8,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==2 & age_adjust==5,13,0))
  CDC2017NQA$LA3000<-with(CDC2017NQA,LA3000-ifelse(gender==2 & age_adjust==6,20,0))
  
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==1 & age_adjust==2,2,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==1 & age_adjust==3,6,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==1 & age_adjust==4,12,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==1 & age_adjust==5,20,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==1 & age_adjust==6,31,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==2 & age_adjust==2,1,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==2 & age_adjust==3,4,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==2 & age_adjust==4,8,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==2 & age_adjust==5,13,0))
  CDC2017NQA$RA3000<-with(CDC2017NQA,RA3000-ifelse(gender==2 & age_adjust==6,20,0))
  
  #4000
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==1 & age_adjust==2,2,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==1 & age_adjust==3,8,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==1 & age_adjust==4,16,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==1 & age_adjust==5,28,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==1 & age_adjust==6,43,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==2 & age_adjust==2,1,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==2 & age_adjust==3,4,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==2 & age_adjust==4,9,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==2 & age_adjust==5,16,0))
  CDC2017NQA$LA4000<-with(CDC2017NQA,LA4000-ifelse(gender==2 & age_adjust==6,24,0))
  
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==1 & age_adjust==2,2,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==1 & age_adjust==3,8,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==1 & age_adjust==4,16,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==1 & age_adjust==5,28,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==1 & age_adjust==6,43,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==2 & age_adjust==2,1,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==2 & age_adjust==3,4,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==2 & age_adjust==4,9,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==2 & age_adjust==5,16,0))
  CDC2017NQA$RA4000<-with(CDC2017NQA,RA4000-ifelse(gender==2 & age_adjust==6,24,0))
  
  #6000
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==1 & age_adjust==2,3,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==1 & age_adjust==3,9,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==1 & age_adjust==4,18,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==1 & age_adjust==5,32,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==1 & age_adjust==6,49,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==2 & age_adjust==2,2,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==2 & age_adjust==3,6,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==2 & age_adjust==4,12,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==2 & age_adjust==5,21,0))
  CDC2017NQA$LA6000<-with(CDC2017NQA,LA6000-ifelse(gender==2 & age_adjust==6,32,0))
  
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==1 & age_adjust==2,3,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==1 & age_adjust==3,9,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==1 & age_adjust==4,18,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==1 & age_adjust==5,32,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==1 & age_adjust==6,49,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==2 & age_adjust==2,2,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==2 & age_adjust==3,6,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==2 & age_adjust==4,12,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==2 & age_adjust==5,21,0))
  CDC2017NQA$RA6000<-with(CDC2017NQA,RA6000-ifelse(gender==2 & age_adjust==6,32,0))
  
  #8000
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==1 & age_adjust==2,3,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==1 & age_adjust==3,11,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==1 & age_adjust==4,23,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==1 & age_adjust==5,39,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==1 & age_adjust==6,60,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==2 & age_adjust==2,2,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==2 & age_adjust==3,7,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==2 & age_adjust==4,15,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==2 & age_adjust==5,27,0))
  CDC2017NQA$LA8000<-with(CDC2017NQA,LA8000-ifelse(gender==2 & age_adjust==6,41,0))
  
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(age_adjust==1,0,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==1 & age_adjust==2,3,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==1 & age_adjust==3,11,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==1 & age_adjust==4,23,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==1 & age_adjust==5,39,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==1 & age_adjust==6,60,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==2 & age_adjust==2,2,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==2 & age_adjust==3,7,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==2 & age_adjust==4,15,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==2 & age_adjust==5,27,0))
  CDC2017NQA$RA8000<-with(CDC2017NQA,RA8000-ifelse(gender==2 & age_adjust==6,41,0))
  return(CDC2017NQA)
}
