mynormalize<-function(m,type="znorm"){
  if (type=="znorm"){ ##standard z-norm procedure,mean 0, sd = 1
    (m-mean(m,na.rm=TRUE))/sd(m,na.rm=TRUE)
  }
  else if (type=="normalrange") { ##normalize and scaled between -1 to 1, max value set to 1
    (m-mean(m,na.rm=TRUE))/max(abs(m-mean(m,na.rm=TRUE)),na.rm=TRUE)
  }
  else if (type=="unitize"){ ##normalize and linearly scaled between -1 and 1
    (m-mean(m,na.rm=TRUE))/(max(m,na.rm=TRUE)-min(m,na.rm=TRUE))
  }
  else if (type=="scale"){ ##this is a linear scaling between max and min
    (m-min(m,na.rm=TRUE))/(max(m,na.rm=TRUE)-min(m,na.rm=TRUE))
  }
  
}