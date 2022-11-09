path<-'E:/Data/TCR-seq/Project1/PT081/VDJtoolsTop'
fileNames <- dir(path) 
filePath <- sapply(fileNames, function(x){ 
    paste(path,x,sep='/')})  
filePath<-filePath[which(str_sub(filePath,-8,-1)=='j.wt.txt')]
df <- lapply(filePath, function(x){
    read.table(x,na.strings = c("NA"), sep="\t", comment="")})
require(circlize); require(RColorBrewer)

TRBV<-as.vector(t(df[[1]][1,]))
for (i in 1:length(df)){
    TRBV<-append(TRBV,as.vector(t(df[[i]][1,])))
    }
TRBV<-sort(unique(TRBV))[-1]
for(i in seq_len(length(TRBV))) {
      TRBV[i] <- paste(TRBV[i], paste(rep(" ", 10 - nchar(TRBV[i])), collapse = ''))
}
cb_palette <- c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#666666","#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#E5C494","#FBB4AE","#B3CDE3","#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC","#F2F2F2","#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9","#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8","#4DAF4A")
VCCol<-cbind(TRBV,cb_palette)

# write.table(col_vector, sep="\t",row.names = FALSE,"clipboard-256")

TRBJ<-as.vector(df[[1]][,1])
for (i in 1:length(df)){
    TRBJ<-append(TRBJ,as.vector(df[[i]][,1]))
    }
TRBJ<-sort(unique(TRBJ))[-1]
for(i in seq_len(length(TRBJ))) {
      TRBJ[i] <- paste(TRBJ[i], paste(rep(" ", 10 - nchar(TRBJ[i])), collapse = ''))
}
cb_palette <-c("#B3B3B3","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")
JRCol<-cbind(TRBJ,cb_palette)



for (l in 1:length(df)){
names(df)[l]<-substring(names(df[l]),1,6)
# l=11
  df[[l]]->temp
  n <- nrow(temp)
  m <- ncol(temp)
  rn = as.character(temp[2:n,1])
  cn = apply(temp[1,2:m], 2 , as.character)
  mat <- matrix(apply(temp[2:n, 2:m], 1:2, as.numeric), n - 1, m-1) * 100

  n <- nrow(temp)
  m <- ncol(temp)
  # Here columns and rows correspond to V and J segments respectively
  # Also replace possible duplicates (undef, '.', ...)
  duplicates <- intersect(rn, cn)
  rownames(mat) <- replace(rn, rn==duplicates, paste("V", duplicates, sep=""))
  colnames(mat) <- replace(cn, cn==duplicates, paste("J", duplicates, sep=""))

# sort

col_sum = apply(mat, 2, sum)
row_sum = apply(mat, 1, sum)

mat <- mat[order(row_sum), order(col_sum)]

# equal number of characters for visualizaiton

rn <- rownames(mat)
cn <- colnames(mat)

maxrn <- max(nchar(rn))
maxcn <- max(nchar(cn))

for(i in seq_len(length(rn))) {
      rn[i] <- paste(rn[i], paste(rep(" ", 10 - nchar(rn[i])), collapse = ''))
}

for(i in seq_len(length(cn))) {
      cn[i] <- paste(cn[i], paste(rep(" ", 10 - nchar(cn[i])), collapse = ''))
}

rownames(mat) <- rn
colnames(mat) <- cn

# viz using circlize
png(filename = paste("E:/Analysis/TCR-seq/Graph/VDJtoolsTop/wt-PT",names(df)[l],'.png',sep=""), 
                 width     = 3.25,
                 height    = 3.25,
                 units     = "in",
                 res       = 1200,
                 pointsize = 4)

circos.par(gap.degree = c(rep(1, nrow(mat)-1), 10, rep(1, ncol(mat)-1), 10), start.degree = 5)


Vname<-as.matrix(sort(colnames(mat)))
colnames(Vname)[1] = 'TRBV'
Vedge = merge(VCCol,Vname,by = 'TRBV')
ccols<-Vedge[,2][1:nrow(Vedge)]

Jname<-as.matrix(sort(rownames(mat)))
colnames(Jname)[1] = 'TRBJ'
Jedge = merge(JRCol,Jname,by = 'TRBJ')
rcols<-Jedge[,2][1:nrow(Jedge)]

# rcols <- rep(brewer.pal(12, "Paired"), nrow(mat)/12 + 1)[1:nrow(mat)]
# ccols <- rep(brewer.pal(12, "Paired"), ncol(mat)/12 + 1)[1:ncol(mat)]

names(rcols) <- Jedge[,1][1:nrow(Jedge)]
names(ccols) <- Vedge[,1][1:nrow(Vedge)]

chordDiagram(mat, annotationTrack = "grid",
             grid.col = c(rcols, ccols),
             preAllocateTracks = list(track.height = 0.2), transparency = 0.5, reduce = 0)

circos.trackPlotRegion(track.index = 1, bg.border = NA,
       panel.fun = function(x, y) {
                   sector.name = get.cell.meta.data("sector.index")
                   xlim = get.cell.meta.data("xlim")
                   ylim = get.cell.meta.data("ylim")
                   circos.text(mean(xlim), ylim[1], cex = 0.5, sector.name, facing = "clockwise", adj = c(0, 0.5))
                   }
       )

circos.clear()
dev.off()

}

