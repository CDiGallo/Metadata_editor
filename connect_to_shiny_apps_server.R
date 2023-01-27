
install.packages("rsconnect")

rsconnect::setAccountInfo(name='barbarossa', token='A2A4DE64555D79952962115D0C65B691', secret='FoZDhgRG+9yuhICqArkTUr7U/AZ5o3GG1oq2Cp3G')

library(rsconnect)
rsconnect::deployApp('/cloud/project')
Y

