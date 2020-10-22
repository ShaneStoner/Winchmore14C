registerDoParallel(8)

years = seq(1950, 2010)

ks <- c(seq(1,20), seq(25, 100, by = 5), seq(120, 500, by = 20))

# kFrame <- data.frame(matrix(ncol = length(years)+1, nrow = length(p1)))
# kFrame$`1/k` <- ks

plist = list(ks = ks)


for(z in years){

p <- unlist(
  foreach (x = ks, .combine = 'c') %dopar% {
    p = steadyMod(1/x, z, plotOut = FALSE)$preBomb14C[6]
    p <- as.list(p)
    list(p)
  })

yearName <- as.character(z)
plist[[yearName]] <- list(p)

print(paste('Finished for', z))
}

savename = './pListKturnoverW14C.Rdata'
save(saveName, plist, file = 'pListKturnoverW14C.Rdata')
