hyper_1="1_IVF-ET_vs_CTRL/Gain.bed"
hyper_2="2_ICSI-ET_vs_CTRL/Gain.bed"
hyper_3="3_IVF-FET_vs_CTRL/Gain.bed"
hyper_4="4_ICSI-FET_vs_CTRL/Gain.bed"
hyper_5="5_ICSI-ET_vs_IVF-ET/Gain.bed"
hyper_6="6_IVF-FET_vs_IVF-ET/Gain.bed"
hyper_7="7_ICSI-FET_vs_IVF-ET/Gain.bed"

hypo_1="1_IVF-ET_vs_CTRL/Loss.bed"
hypo_2="2_ICSI-ET_vs_CTRL/Loss.bed"
hypo_3="3_IVF-FET_vs_CTRL/Loss.bed"
hypo_4="4_ICSI-FET_vs_CTRL/Loss.bed"
hypo_5="5_ICSI-ET_vs_IVF-ET/Loss.bed"
hypo_6="6_IVF-FET_vs_IVF-ET/Loss.bed"
hypo_7="7_ICSI-FET_vs_IVF-ET/Loss.bed"




################################## 4 groups

outpath1="For_ICSI-ET_IVF-FET/HOMER_hyper"
mkdir -p $outpath1
mergePeaks  -d  given    -prefix $outpath1/region  -matrix $outpath1/matrix   -venn $outpath1/venn.txt    \
$hyper_2    $hyper_3     $hyper_5      $hyper_6         > $outpath1/runLog.txt 2>&1
 

outpath1="For_ICSI-ET_IVF-FET/HOMER_hypo"
mkdir -p $outpath1
mergePeaks  -d  given    -prefix $outpath1/region  -matrix $outpath1/matrix   -venn $outpath1/venn.txt    \
 $hypo_2   $hypo_3     $hypo_5   $hypo_6        > $outpath1/runLog.txt 2>&1

 





################################## 2 groups
##################################
outpath3="For_ICSI/HOMER_hyper"
mkdir -p $outpath3
mergePeaks  -d  given    -prefix $outpath3/region  -matrix $outpath3/matrix   -venn $outpath3/venn.txt    \
$hyper_2     $hyper_5       > $outpath3/runLog.txt 2>&1
 

outpath3="For_ICSI/HOMER_hypo"
mkdir -p $outpath3
mergePeaks  -d  given    -prefix $outpath3/region  -matrix $outpath3/matrix   -venn $outpath3/venn.txt    \
$hypo_2     $hypo_5       > $outpath3/runLog.txt 2>&1

 
##################################
outpath3="For_FET/HOMER_hyper"
mkdir -p $outpath3
mergePeaks  -d  given    -prefix $outpath3/region  -matrix $outpath3/matrix   -venn $outpath3/venn.txt    \
$hyper_3     $hyper_6       > $outpath3/runLog.txt 2>&1
 

outpath3="For_FET/HOMER_hypo"
mkdir -p $outpath3
mergePeaks  -d  given    -prefix $outpath3/region  -matrix $outpath3/matrix   -venn $outpath3/venn.txt    \
$hypo_3     $hypo_6       > $outpath3/runLog.txt 2>&1



##################################
outpath3="For_ICSI-FET/HOMER_hyper"
mkdir -p $outpath3
mergePeaks  -d  given    -prefix $outpath3/region  -matrix $outpath3/matrix   -venn $outpath3/venn.txt    \
$hyper_4     $hyper_7       > $outpath3/runLog.txt 2>&1
 

outpath3="For_ICSI-FET/HOMER_hypo"
mkdir -p $outpath3
mergePeaks  -d  given    -prefix $outpath3/region  -matrix $outpath3/matrix   -venn $outpath3/venn.txt    \
$hypo_4     $hypo_7       > $outpath3/runLog.txt 2>&1

 




