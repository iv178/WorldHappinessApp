library(rsconnect)

# Prijavi se na shinyapps.io (ako već nisi)
library(rsconnect)
rsconnect::setAccountInfo(name='ivmg-analysis',
                          token='21DA572B6CFC8FCDB1D92FFFCA598C01',
                          secret='5sd85Xz8HKueo6spZNLzXQxfYnXVuQAPfrHa2xpH')

# Postavi aplikaciju
rsconnect::deployApp(appDir = "C:/Users/irena/OneDrive/Desktop/vjezba_nova/ShinyApp")

