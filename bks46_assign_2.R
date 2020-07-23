getwd()
setwd("/Users/baljindersmagh/Desktop/Data Mining/HW-2")
car_data <-read.csv("car.data",header = FALSE)
names(car_data)<-c("Buying","Maintenance","Doors","Person","Lug_boot","Safety","Class Values")
car_matrix<-as.matrix(car_data)

##asigning ranking ##
for (i in 1:1728){
  for (j in 1:7){
    if (j == 1 || j ==2){
      if(car_matrix[i,j] == "vhigh"){
        car_matrix[i,j] = 4
      }else{
        if(car_matrix[i,j] == "high"){
          car_matrix[i,j] = 3
        }else{
          if(car_matrix[i,j] == "med"){
            car_matrix[i,j] = 2
          }else{
            if(car_matrix[i,j] == "low")
              car_matrix[i,j] = 1
          }
        }
      }
    }else{
      if(j == 3){
        if(car_matrix[i,j] == "5more"){
          car_matrix[i,j] = 4
        }else{
          if(car_matrix[i,j] == "4"){
            car_matrix[i,j] = 3
          }else{
            if(car_matrix[i,j] == "3"){
              car_matrix[i,j] = 2
            }else{
              if(car_matrix[i,j] == "2"){
                car_matrix[i,j] = 1
              }
            }
          }
        }
      }else{
        if(j == 4){
          if(car_matrix[i,j] == "more"){
            car_matrix[i,j] = 3
          }else{
            if(car_matrix[i,j] == "4"){
              car_matrix[i,j] = 2
            }else{
              if(car_matrix[i,j] == "2"){
                car_matrix[i,j] = 1
              }
            }
          }
        }else{
          if(j == 5){
            if(car_matrix[i,j] == "big"){
              car_matrix[i,j] = 3
            }else{
              if(car_matrix[i,j] == "med"){
                car_matrix[i,j] = 2
              }else{
                if(car_matrix[i,j] == "small"){
                  car_matrix[i,j] = 1
                }
              }
            }
          }else{
            if(j == 6){
              if(car_matrix[i,j] == "high"){
                car_matrix[i,j] = 3
              }else{
                if(car_matrix[i,j] == "med"){
                  car_matrix[i,j] = 2
                }else{
                  if(car_matrix[i,j] == "low"){
                    car_matrix[i,j] = 1
                  }
                }
              }
            }else{
              if(j == 7){
                if(car_matrix[i,j] == "vgood"){
                  car_matrix[i,j] = 4
                }else{
                  if(car_matrix[i,j] == "good"){
                    car_matrix[i,j] = 3
                  }else{
                    if(car_matrix[i,j] == "acc"){
                      car_matrix[i,j] = 2
                    }else{
                      if(car_matrix[i,j] == "unacc"){
                        car_matrix[i,j] = 1
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
  }
}


##Normalizing##

for (i in 1:1728){
  for (j in 1:7){
    if(j == 1 || j == 2 || j == 7){
      car_matrix[i,j] = (as.integer(car_matrix[i,j]) - 1)/3
    }else{
      car_matrix[i,j] = (as.integer(car_matrix[i,j]) - 1)/2
    }
    
  }
  
}
dissim_matrix<-matrix(nrow=1728,ncol=1728)
sim_matrix<-matrix(nrow=1728,ncol=1728)

#calculation of dissimilarity and similarity matrix#

for(i in 1:1728){
  for(j in 1:1728){
    dissim_matrix[i,j]=0
    sim_matrix[i,j]=0
    if(i==j){
      dissim_matrix[i,j]=0
    }else{
      for(k in 1:7){
        dissim_matrix[i,j]=dissim_matrix[i,j]+((as.numeric(car_matrix[i,k])-as.numeric(car_matrix[j,k]))^2)
      }
    }
    dissim_matrix[i,j]=(dissim_matrix[i,j])^(1/2)
    sim_matrix[i,j]=1-dissim_matrix[i,j]
  }
}

#Q1#
#calculating highest similarity#
high_sim=0
x=0
y=0
for(i in 1:1728){
  for(j in 1:1728){
    if(i==j){
      next
    }else{
      if(sim_matrix[i,j]>high_sim){
        high_sim=sim_matrix[i,j]
        x=i
        y=j
      }
    }
  }
}

#Q2#
#calculating highest dissimilarity
high_dis=1
a=0
b=0
for(i in 1:1728){
  for(j in 1:1728){
    if(i==j){
      next
    }else{
      if(dissim_matrix[i,j]>high_dis){
        high_dis=dissim_matrix[i,j]
        a=i
        b=j
      }
    }
  }
}

#Q3#

#finding highest positive and negative correlation among categorical attributes using correlation and chisquare test##

cor_matrix<-data.matrix(car_data)
cor_matrix
#significance level 0.05#
cor.test(cor_matrix[1,],cor_matrix[2,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[1,],cor_matrix[3,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[1,],cor_matrix[4,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[1,],cor_matrix[5,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[1,],cor_matrix[6,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[1,],cor_matrix[7,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[2,],cor_matrix[3,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[2,],cor_matrix[4,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[2,],cor_matrix[5,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[2,],cor_matrix[6,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[2,],cor_matrix[7,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[3,],cor_matrix[4,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[3,],cor_matrix[5,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[3,],cor_matrix[6,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[3,],cor_matrix[7,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[4,],cor_matrix[5,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[4,],cor_matrix[6,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[4,],cor_matrix[7,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[5,],cor_matrix[6,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[5,],cor_matrix[7,],method="pearson",conf.level = 0.95)
cor.test(cor_matrix[6,],cor_matrix[7,],method="pearson",conf.level = 0.95)

#significance level 0.001#
cor.test(cor_matrix[1,],cor_matrix[2,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[1,],cor_matrix[3,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[1,],cor_matrix[4,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[1,],cor_matrix[5,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[1,],cor_matrix[6,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[1,],cor_matrix[7,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[2,],cor_matrix[3,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[2,],cor_matrix[4,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[2,],cor_matrix[5,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[2,],cor_matrix[6,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[2,],cor_matrix[7,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[3,],cor_matrix[4,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[3,],cor_matrix[5,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[3,],cor_matrix[6,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[3,],cor_matrix[7,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[4,],cor_matrix[5,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[4,],cor_matrix[6,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[4,],cor_matrix[7,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[5,],cor_matrix[6,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[5,],cor_matrix[7,],method="pearson",conf.level = 0.999)
cor.test(cor_matrix[6,],cor_matrix[7,],method="pearson",conf.level = 0.999)


#Significance level 0.01#
 
cor.test(cor_matrix[1,],cor_matrix[3,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[1,],cor_matrix[4,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[1,],cor_matrix[5,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[1,],cor_matrix[6,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[1,],cor_matrix[7,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[2,],cor_matrix[3,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[2,],cor_matrix[4,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[2,],cor_matrix[5,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[2,],cor_matrix[6,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[2,],cor_matrix[7,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[3,],cor_matrix[4,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[3,],cor_matrix[5,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[3,],cor_matrix[6,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[3,],cor_matrix[7,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[4,],cor_matrix[5,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[4,],cor_matrix[6,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[4,],cor_matrix[7,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[5,],cor_matrix[6,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[5,],cor_matrix[7,],method="pearson",conf.level = 0.99)
cor.test(cor_matrix[6,],cor_matrix[7,],method="pearson",conf.level = 0.99)



#scatter plot#
plot(car_data$Buying,car_data$`Class Values`)
plot(car_data$Doors,car_data$`Class Values`)


#Present the dissimilarity matrix for the very good cars#
car_data_vgood<-car_data[car_data$`Class Values`=="vgood",]

car_matrix_vgood<-as.matrix(car_data_vgood)

##asigning ranking ##
for (i in 1:65){
  for (j in 1:7){
    if (j == 1 || j ==2){
      if(car_matrix_vgood[i,j] == "vhigh"){
        car_matrix_vgood[i,j] = 4
      }else{
        if(car_matrix_vgood[i,j] == "high"){
          car_matrix_vgood[i,j] = 3
        }else{
          if(car_matrix_vgood[i,j] == "med"){
            car_matrix_vgood[i,j] = 2
          }else{
            if(car_matrix_vgood[i,j] == "low")
              car_matrix_vgood[i,j] = 1
          }
        }
      }
    }else{
      if(j == 3){
        if(car_matrix_vgood[i,j] == "5more"){
          car_matrix_vgood[i,j] = 4
        }else{
          if(car_matrix_vgood[i,j] == "4"){
            car_matrix_vgood[i,j] = 3
          }else{
            if(car_matrix_vgood[i,j] == "3"){
              car_matrix_vgood[i,j] = 2
            }else{
              if(car_matrix_vgood[i,j] == "2"){
                car_matrix_vgood[i,j] = 1
              }
            }
          }
        }
      }else{
        if(j == 4){
          if(car_matrix_vgood[i,j] == "more"){
            car_matrix_vgood[i,j] = 3
          }else{
            if(car_matrix_vgood[i,j] == "4"){
              car_matrix_vgood[i,j] = 2
            }else{
              if(car_matrix_vgood[i,j] == "2"){
                car_matrix_vgood[i,j] = 1
              }
            }
          }
        }else{
          if(j == 5){
            if(car_matrix_vgood[i,j] == "big"){
              car_matrix_vgood[i,j] = 3
            }else{
              if(car_matrix_vgood[i,j] == "med"){
                car_matrix_vgood[i,j] = 2
              }else{
                if(car_matrix_vgood[i,j] == "small"){
                  car_matrix_vgood[i,j] = 1
                }
              }
            }
          }else{
            if(j == 6){
              if(car_matrix_vgood[i,j] == "high"){
                car_matrix_vgood[i,j] = 3
              }else{
                if(car_matrix_vgood[i,j] == "med"){
                  car_matrix_vgood[i,j] = 2
                }else{
                  if(car_matrix_vgood[i,j] == "low"){
                    car_matrix_vgood[i,j] = 1
                  }
                }
              }
            }else{
              if(j == 7){
                if(car_matrix_vgood[i,j] == "vgood"){
                  car_matrix_vgood[i,j] = 4
                }else{
                  if(car_matrix_vgood[i,j] == "good"){
                    car_matrix_vgood[i,j] = 3
                  }else{
                    if(car_matrix_vgood[i,j] == "acc"){
                      car_matrix_vgood[i,j] = 2
                    }else{
                      if(car_matrix_vgood[i,j] == "unacc"){
                        car_matrix_vgood[i,j] = 1
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
  }
}


##Normalizing##

for (i in 1:65){
  for (j in 1:7){
    if(j == 1 || j == 2 || j == 7){
      car_matrix_vgood[i,j] = (as.integer(car_matrix_vgood[i,j]) - 1)/3
    }else{
      car_matrix_vgood[i,j] = (as.integer(car_matrix_vgood[i,j]) - 1)/2
    }
    
  }
  
}


dissim_matrix_vgood<-matrix(nrow=65,ncol=65)
for(i in 1:65){
  for(j in 1:65){
    dissim_matrix_vgood[i,j]=0
    if(i==j){
      dissim_matrix_vgood[i,j]=0
    }else{
      for(k in 1:7){
        dissim_matrix_vgood[i,j]=dissim_matrix_vgood[i,j]+((as.numeric(car_matrix_vgood[i,k])-as.numeric(car_matrix_vgood[j,k]))^2)
      }
    }
    dissim_matrix_vgood[i,j]=(dissim_matrix_vgood[i,j])^(1/2)
  }
}






