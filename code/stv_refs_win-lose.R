##################################################
# PLOT STV REFERENDUMS (WINNING/LOSING) BY YEAR AND/OR DECADE
# JACK SANTUCCI
# CREATED 2024-11-25 IN R 4.3.2 FOR MAC 
##################################################

d <- read.delim(file.choose()) # because an unknown bug prevents use of file path. Data can be gotten at https://osf.io/kf3cp.

###### Variable creation

d$year <- as.numeric(substr(d$ref.date, 1, 4))

d$win <- ifelse(as.numeric(d$for.pct > 0.5), 1, 0)

###### Tablulate outcomes by year

wintab <- table(d$win, d$year)

dimnames(wintab)[[1]] <- c("lose", "win")

###### Munge data for creation of barplot

wintab.df <- data.frame(cbind("year"=dimnames(wintab)[[2]], "lose"=wintab['lose',], "win"=wintab['win',]))

year <- seq(min(d$year, na.rm=T), max(d$year, na.rm=T))

year.df <- as.data.frame(year)

full <- merge(year.df, wintab.df, all.x=T)

full$win <- as.numeric(full$win)

full$lose <- as.numeric(full$lose)

full[is.na(full)] <- 0

full2 <- t(full)

##### Barplot

pdf('../output/stv_adopt_referendums.pdf')

bp <- barplot(as.matrix(full2[2:3,]), beside=F, col=c('gray', 'black'), axes=F, ylim=c(0, 10), ylab="Count of referendums", main="Winning and losing referendums\non single transferable vote adoption in U.S. cities")

## x-axis construction

for.axis <- cbind(bp, year)

legend('top', legend=c("Losses (less than half of yes + no)", "Wins (more than half of yes + no)"), fill=c('gray', 'black'), bty='n')

axis(2, tick=F, las=2)

axis(1, tick=T, las=2, at=for.axis[,1], labels=for.axis[,2])

dev.off()