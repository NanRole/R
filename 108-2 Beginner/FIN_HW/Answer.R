KB <- read.csv('KB.csv', header=T, sep=',')
KB_EFF <- ((KB$PTS+KB$TRB+KB$AST+KB$STL+KB$BLK)
           -((KB$FGA-KB$FG)+(KB$FTA-KB$FT)+KB$TOV)) / KB$G
# KB_EFF <- sum(KB_EFF) / length(KB_EFF)
KB_EFF <- mean(KB_EFF)


LJ <- read.csv('LJ.csv', header=T, sep=',')
LJ_EFF <- ((LJ$PTS+LJ$TRB+LJ$AST+LJ$STL+LJ$BLK)
           -((LJ$FGA-LJ$FG)+(LJ$FTA-LJ$FT)+LJ$TOV)) / LJ$G
LJ_EFF <- mean(LJ_EFF)

MJ <- read.csv('MJ.csv', header=T, sep=',')
MJ_EFF <- ((MJ$PTS+MJ$TRB+MJ$AST+MJ$STL+MJ$BLK)
           -((MJ$FGA-MJ$FG)+(MJ$FTA-MJ$FT)+MJ$TOV)) / MJ$G
MJ_EFF <- mean(MJ_EFF)

EFF <- c(MJ_EFF, KB_EFF, LJ_EFF)

xx <- barplot(EFF, main = 'NBA Career Performance',
        names.arg = c('Michael Jordan', 'Kobe Bryant', 'LeBron James'),
        ylab = 'Efficiency', xlab = 'Player', ylim = c(0, 1), col = rainbow(3))
text(x = xx, y = EFF, labels = round(EFF, 2), pos = 3, cex = 2)
