

library(readxl)

# effacer toutes les variables sauvegardees dans l environnement 
rm(list=ls())


setwd("C:/Users/mantunes/Desktop/Alternance/Sujet mémoire/Données/Infrastructure")

# Importer ci-dessous le fichier des cours, et mettre le working directory dans le repertoire ou il est localise

lecture <- function(data) { read_excel(data)}

#excel <- "FR0007038674.xlsx" 


#excel <- "MSCI World Net Total Return.xlsx"

excel <- "Infralux.xlsx"



data <- lecture(excel)

date <- data$date
close <- data$close


#data[order(data$date),]     au cas ou les cours ne soient pas deja tries du plus ancien au plus recent



# Tests prealables pour voir si les resultats descriptifs semblent coherents avec ceux obtenus sur excel 


mean(close)
sd(close)

quantile(close,0.05)



# Introduction du vecteur des rendements  Rt = ln (Cours T / Cours T-1)

(returns <- diff(log(close)))

# Definition de la moyenne et de l ecart type



(mu <- mean((returns)))
(sigma <- sd(returns))



# Definition du nombre total d observations

n <- length(returns)

# Definition du niveau de confiance alpha

alpha <- 0.05

# Definition du Za associe au quantile alpha de la loi normale N(0,1), Za = 1.645 environ pour un seuil de confiance a 95%

Za <- qnorm(alpha,0,1)




# I.  Inference de la VAR par l approche historique


# Calcul de la VAR a un jour
(VAR_historique_j <- mu + sigma * Za)

# Calcul de la VAR a un an, 4 étant le nombre de trimestres sur une année, multiplie par 100 pour ramener a un pourcentage

(VAR_historique_y <- sqrt(4)*VAR_historique_j*100)






# II.  Inference de la VAR par l approche du Bootstrap



# Nombre de replications pour la methode du bootstrap
m <- 100000

# Nombre de trimestres dans l horizon de risque (52 semaines par an)
h <- 4

# Valeur de confiance (ex: 0.05)
alpha <- 0.05



quantile <- matrix(nrow = 1, ncol = m)

sample_returns <- matrix(nrow = m, ncol = h)


for (i in 1:m) {
  sample_returns[i,] <- sample(returns, h, replace = TRUE)
  quantile[1,i] <- quantile(sample_returns[i,], alpha)
  
}


# Retransformation pour obtenir la VAR un jour sous forme de pourcentage

(depreciation = 100 * (exp(mean(quantile)) - 1))

# Calcul de la VAR un an

(VAR_bootstrap_y <- depreciation * sqrt(4))







# III.   Inference de la VAR par la methode analytique 


# introduire le vecteur volatilite pour calcul de la      volatilité historique = Racine (n * (somme ( Rt^2 / (n-1) ) ) )

volatilite <- matrix(0, nrow=n, ncol=1)

for (i in 1:n) { 
  
  volatilite[i,1] <- (returns[i]^2 / (n-1)) }




sum(volatilite)
(volatilite_historique <- sqrt(n * sum(volatilite)))




# calcul de la volatilite journaliere en divisant la volatilite historique par la racine du nombre d'observations sur la periode

(volatilite_journaliere <- volatilite_historique / sqrt(n))


# calcul de la VAR a 95% pour un horizon de un jour 

(VAR_j <-  volatilite_journaliere * qnorm(alpha, 0,1))

# calcul de la VAR a 95% pour un horizon d'un an ; ou 52 correspond au nombre de jours d'ouverture de la bourse ; multiplication par 100 pour le pourcentage


(VAR_analytique_y <- VAR_j * sqrt(4) * 100)








# IV. Inference de la VAR par la methode de Cornish Fisher


# Graphique pour avoir une idee de la distribution des donnees
# a priori, s il y a trop de pics alors l approche de Cornish Fisher ne sera pas concluante


library(ggplot2)
datasim1 <- data.frame(close)
ggplot(datasim1, aes(x = close), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Distribution'))) + 
  ylab(expression(bold('Density')))

# Distribution des rendements

datasim2 <- data.frame(returns)
ggplot(datasim2, aes(x = returns), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Distribution'))) + 
  ylab(expression(bold('Density')))




# Calcul des coefficients de skewness et Kurtosis, les moments d'ordre 3 et 4


#skewness <- 1 /n * sum(((returns-mu) / sigma)^3)

#kurtosis <- 1 /n * sum(((returns-mu) / sigma)^4)

library(moments)

#calculate skewness
skewness <- skewness(returns)

#calculate excess kurtosis
kurtosis <- kurtosis(returns)-3



skewness
kurtosis


# calcul du coefficient de Cornish Fisher, lequel sera multiplie a la place du Za (meme facon que methode historique)

Wa <- Za + 1/6 * ( Za^2 -1) * skewness + 1/24 * (Za^3 - 3* Za) * kurtosis - 1/36 * (2*Za^3 - 5* Za) * skewness^2


(var_Cornish_Fisher_j <- mu + sigma * Wa)

(var_Cornish_Fisher_y <- (var_Cornish_Fisher_j) * sqrt(4) * 100)



# pour etre valable, apparemment cette fonction doit etre bijective ; ce qui ne se verifie que si la formule ci-dessous donne un resultat negatif ou nul


bijective <-   skewness^2 /9 -4*(kurtosis/8 - skewness^2/6) * (1-kurtosis/8 +5*skewness^2/36)

bijective





# V.   Inference de la VAR par la methode Exponentially weighted moving average (EWMA)





# Etape 1 - tri des donnees par ordre decroissant de la date, car la ponderation est plus grande pour les valeurs recentes


data_EWMA <- data[order(data$date, decreasing=TRUE),] 
data_EWMA



# Etape 2 - definir le vecteur des rendements (Cours date T / Cours date T-1) -1

returns_EWMA <- matrix(1, nrow=n, ncol=1)


for (i in 1:n-1) {
  temp1 <- data_EWMA$close[i]
  temp2 <- data_EWMA$close[i+1]
  returns_EWMA[i,1] <- temp1 / temp2 - 1
  
  
  
}



returns_EWMA


# Etape 3 - definition du vecteur des rendements au carres 


squared_returns_EWMA <- matrix(1, nrow=n, ncol=1)



squared_returns_EWMA <- returns_EWMA^2


# Etape 4 - definition du parametre de ponderation alpha et du vecteur associe : le poids du premier terme est (1-alpha), puis le second prend la valeur precedente multipliee par alpha


alpha_EWMA <- 0.70


EWMA_vecteur<- matrix(0, nrow=n, ncol=1)

EWMA_vecteur[1,1] <- (1-alpha_EWMA)

for (i in 2:n) {
  EWMA_vecteur[i,] <- EWMA_vecteur[i-1,]*alpha_EWMA
  
}

EWMA_vecteur


# Etape 5 - multiplication des termes de ce vecteurs avec les rendements carres associes, puis calcul de leur somme pour obtenir la variance EWMA


EWMA_variance <- sum(EWMA_vecteur * squared_returns_EWMA)

EWMA_variance




#   Etape 6 - calcul de la volatilite, definie par la racine carree de la variance EWMA


(volatilite_EWMA <- sqrt(EWMA_variance))


#   Etape 6 - calcul de la volatilite EWMA journali?re, definie par la racine carree de la variance EWMA


(volatilite_EWMA <- sqrt(EWMA_variance))


# daily VAR =   volatilite EWMA * -1,645

(VAR_EWMA_j <- volatilite_EWMA*qnorm(alpha,0,1))



# calcul de la VAR a 95% pour un horizon d'un an ; ou 52 correspond au nombre de jours d'ouverture de la bourse

(VAR_EWMA_y <- VAR_EWMA_j * sqrt(4) *100)




# Compilation de tous les resultats
skewness
kurtosis
bijective

VAR_historique_y
VAR_bootstrap_y
VAR_analytique_y
var_Cornish_Fisher_y
VAR_EWMA_y



Sub VAR_Calculation()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Sheets("Data") ' Assurez-vous que vos données sont dans cette feuille

    Dim n As Long, h As Long
    Dim mu As Double, sigma As Double
    Dim alpha As Double
    Dim Za As Double
    Dim VAR_historique_j As Double, VAR_historique_y As Double
    Dim VAR_bootstrap_y As Double
    Dim VAR_analytique_y As Double
    Dim VAR_EWMA_y As Double
    Dim returns() As Double
    Dim close As Range
    Dim i As Long

    ' Importer les données
    Set close = ws.Range("B2:B" & ws.Cells(ws.Rows.Count, 2).End(xlUp).Row) ' Supposons que les prix de clôture sont en colonne B
    n = close.Rows.Count
    
    ' Calcul des rendements
    ReDim returns(1 To n - 1)
    For i = 2 To n
        returns(i - 1) = WorksheetFunction.Ln(close.Cells(i, 1).Value / close.Cells(i - 1, 1).Value)
    Next i

    ' Calcul des statistiques descriptives
    mu = WorksheetFunction.Average(returns)
    sigma = WorksheetFunction.StDev(returns)

    ' Niveau de confiance
    alpha = 0.05
    Za = WorksheetFunction.Norm_S_Inv(alpha)

    ' VAR historique à un jour
    VAR_historique_j = mu + sigma * Za
    VAR_historique_y = Sqr(4) * VAR_historique_j * 100

    ' VAR bootstrap
    Dim m As Long
    m = 100000
    Dim sample_returns() As Double
    ReDim sample_returns(1 To m, 1 To h)
    Dim quantile() As Double
    ReDim quantile(1 To m)

    For i = 1 To m
        For j = 1 To h
            sample_returns(i, j) = returns(Application.WorksheetFunction.RandBetween(1, n - 1))
        Next j
        quantile(i) = WorksheetFunction.Percentile(sample_returns, alpha)
    Next i

    VAR_bootstrap_y = 100 * (WorksheetFunction.Exp(WorksheetFunction.Average(quantile)) - 1) * Sqr(4)

    ' VAR analytique
    Dim volatilite_journaliere As Double
    Dim volatilite_historique As Double
    Dim volatilite() As Double
    ReDim volatilite(1 To n)

    For i = 1 To n
        volatilite(i) = (returns(i) ^ 2 / (n - 1))
    Next i

    volatilite_historique = Sqr(n * WorksheetFunction.Sum(volatilite))
    volatilite_journaliere = volatilite_historique / Sqr(n)
    VAR_analytique_y = volatilite_journaliere * WorksheetFunction.Norm_S_Inv(alpha) * 100

    ' VAR EWMA
    Dim alpha_EWMA As Double
    alpha_EWMA = 0.70
    Dim EWMA_variance As Double
    Dim EWMA_vecteur() As Double
    ReDim EWMA_vecteur(1 To n)

    ' Initialiser EWMA_vecteur
    EWMA_vecteur(1) = 1 - alpha_EWMA

    For i = 2 To n
        EWMA_vecteur(i) = EWMA_vecteur(i - 1) * alpha_EWMA
    Next i

    EWMA_variance = WorksheetFunction.SumProduct(EWMA_vecteur, Application.WorksheetFunction.Transpose(returns) ^ 2)
    Dim volatilite_EWMA As Double
    volatilite_EWMA = Sqr(EWMA_variance)
    VAR_EWMA_y = volatilite_EWMA * WorksheetFunction.Norm_S_Inv(alpha) * 100

    ' Afficher les résultats
    ws.Range("D1").Value = "VAR Historique (1 an)"
    ws.Range("D2").Value = VAR_historique_y
    ws.Range("E1").Value = "VAR Bootstrap (1 an)"
    ws.Range("E2").Value = VAR_bootstrap_y
    ws.Range("F1").Value = "VAR Analytique (1 an)"
    ws.Range("F2").Value = VAR_analytique_y
    ws.Range("G1").Value = "VAR EWMA (1 an)"
    ws.Range("G2").Value = VAR_EWMA_y

End Sub





























