`dtrgg` <-
function(m,a1,a2,h1,h2,y){T=rep(0,length(y));

if ((a1==0)&(a2==0))
 {
    {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]==m)   
        T[j]= 1  # Dirac distribution at m 
         
       else{
           T[j]=0
            }
      } 
    }
  }
  

else if ((h1==0)&(h2==0))
 {
    {for (j in 1:length(y))             
    
      {if (y[j]==m)  
        T[j]= 1  # Dirac distribution at m 
         
       else{
           T[j]=0
            }
      } 
    }
  }



else if ((h1==Inf)&(h2==Inf))
{
   {for (j in 1:length(y))             
   
     {if (y[j]>=(m-a1) & y[j]<=(m+a2) & y[j]==as.integer(y[j]))   #  Support {m-a1,...,m,...m+a2}
        T[j]= 1/(a1+a2+1)  # Discrete uniform distribution  
         
       else{
           T[j]=0
           }
      } 
    }
  }
 

else if ((h1==Inf)&(h2==0))
{
   {for (j in 1:length(y))             
   
     {if (y[j]>=(m-a1) & y[j]<=m & y[j]==as.integer(y[j]))   #  Support {m-a1,...,m-1,m}
        T[j]= 1/(a1+1)  # Discrete uniform distribution  
         
       else{
           T[j]=0
           }
      } 
    }
  }

else if ((h1==0)&(h2==Inf))
{
   {for (j in 1:length(y))             
   
     {if (y[j]>=m & y[j]<=(m+a2) & y[j]==as.integer(y[j]))   #  Support {m,m+1,...,m+a2}
        T[j]= 1/(a2+1)  # Discrete uniform distribution  
         
       else{
           T[j]=0
           }
      } 
    }
  }

else
{
 u1=0;
 u2=0;  
 
 
 {for (k in 1:a1)
 
   { 
    u1=u1+k^h1
    }
    
  } 
 
 {for (k in 1:a2)
 
   { 
    u2=u2+k^h2
    }
    
  } 
 
 
  D=(a1+a2+1)-(a1+1)^(-h1)*u1 -(a2+1)^(-h2)*u2               # Normalizing constant 
  
 
 
  {for (j in 1:length(y))             
    
      {if (y[j]>=(m-a1) & y[j]<=(m-1)& y[j]==as.integer(y[j]))   #  Support {m-a1,...,m-2,m-1}
        T[j]= (1 - ((m-y[j])/(a1+1))^h1)/D  # Discrete triangular distribution a1, h1
         
        
      else if (y[j]>=m & y[j]<=(m+a2)& y[j]==as.integer(y[j]))   #  Support {m,m+1,...m+a2}
        T[j]= (1 - ((y[j]-m)/(a2+1))^h2)/D  # Discrete triangular distribution a2, h2
         	
      
       else{
           T[j]=0
            }
      } 
   }
}

return(T)
}

