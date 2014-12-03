    library(survival)

    ## Loading required package: splines

    require(RVAideMemoire)

    ## Loading required package: RVAideMemoire
    ## *** Package RVAideMemoire v 0.9-40 ***

    require(multcomp)

    ## Loading required package: multcomp
    ## Loading required package: mvtnorm
    ## Loading required package: TH.data

    kmdab=read.csv("KMdataDabob.csv")
    names(kmdab)    

    ## [1] "Animal"     "Population" "Death"      "Status"

    with(kmdab, tapply(Death[Status==1],Population[Status==1],mean))

    ##     H     N     S 
    ## 4.385 4.522 4.715

    with(kmdab, tapply(Death[Status==1],Population[Status==1],var))

    ##      H      N      S 
    ## 0.4524 0.9786 1.6039

    fit1=with(kmdab,survfit(Surv(Death,Status)~Population))
    summary(fit1)

    ## Call: survfit(formula = Surv(Death, Status) ~ Population)
    ## 
    ##                 Population=H 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480     113    0.765  0.0194        0.728        0.803
    ##     5    307      53    0.633  0.0230        0.589        0.679
    ##     8    214       3    0.624  0.0232        0.580        0.671
    ## 
    ##                 Population=N 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480     229    0.523  0.0228        0.480        0.570
    ##     5    208      97    0.279  0.0218        0.239        0.325
    ##     8     88      21    0.212  0.0209        0.175        0.258
    ## 
    ##                 Population=S 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480     180    0.625  0.0221        0.583        0.670
    ##     5    249      71    0.447  0.0239        0.402        0.496
    ##     8    144      33    0.344  0.0241        0.300        0.395

    plot(fit1,xlim=c(0,11), col=c("#3366CC","#CC66CC","#FF9900"), xlab="Survival Time from Outplant in Months", ylab="% Surviving", lwd=2)
    legend("bottomleft", title="Population", c("Dabob","Fidalgo","Oyster Bay"), fill=c("#3366CC","#CC66CC","#FF9900"))

![plot of chunk
unnamed-chunk-1](./KMgraphsWorking_files/figure-markdown_strict/unnamed-chunk-11.png)

    kmman=read.csv("KMdataMan.csv")
    names(kmman)

    ## [1] "Animal"     "Population" "Death"      "Status"

    with(kmman, tapply(Death[Status==1],Population[Status==1],mean))

    ##      H      N      S 
    ## 10.540  9.368  9.197

    with(kmman, tapply(Death[Status==1],Population[Status==1],var))

    ##     H     N     S 
    ## 1.519 6.468 6.321

    fit2=with(kmman, survfit(Surv(Death,Status)~Population))
    summary(fit2)

    ## Call: survfit(formula = Surv(Death, Status) ~ Population)
    ## 
    ##                 Population=H 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480       1    0.998 0.00208        0.994        1.000
    ##     6    446       1    0.996 0.00305        0.990        1.000
    ##    10    445      11    0.971 0.00791        0.956        0.987
    ##    11    434      37    0.888 0.01489        0.860        0.918
    ## 
    ##                 Population=N 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      13    0.973 0.00741        0.959        0.988
    ##     6    435       5    0.962 0.00885        0.945        0.979
    ##    10    430      26    0.904 0.01383        0.877        0.931
    ##    11    404      43    0.807 0.01857        0.772        0.845
    ## 
    ##                 Population=S 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      12    0.975 0.00713        0.961        0.989
    ##     6    436       4    0.966 0.00835        0.950        0.983
    ##    10    432      33    0.892 0.01456        0.864        0.921
    ##    11    399      27    0.832 0.01761        0.798        0.867

    plot(fit2, col=c("#3366CC","#CC66CC","#FF9900"), xlab="Survival Time from Outplant in Months", ylab="% Surviving", lwd=2)
    legend("bottomleft", title="Population", c("Dabob","Fidalgo","Oyster Bay"), fill=c("#3366CC","#CC66CC","#FF9900"))

![plot of chunk
unnamed-chunk-1](./KMgraphsWorking_files/figure-markdown_strict/unnamed-chunk-12.png)

    kmfid=read.csv("KMdataFid.csv")
    with(kmfid, tapply(Death[Status==1],Population[Status==1],mean))

    ##     H     N     S 
    ## 5.921 5.754 6.897

    with(kmfid, tapply(Death[Status==1],Population[Status==1],var))

    ##     H     N     S 
    ## 2.554 2.189 3.989

    fit3=with(kmfid, survfit(Surv(Death,Status)~Population))
    summary(fit3)

    ## Call: survfit(formula = Surv(Death, Status) ~ Population)
    ## 
    ##                 Population=H 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      21    0.956 0.00934        0.938        0.975
    ##     6    426      43    0.860 0.01629        0.828        0.892
    ##     9    383      12    0.833 0.01753        0.799        0.868
    ## 
    ##                 Population=N 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      18    0.963 0.00867        0.946        0.980
    ##     6    430      36    0.882 0.01511        0.853        0.912
    ##     9    394       7    0.866 0.01596        0.836        0.898
    ## 
    ##                 Population=S 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      16    0.967 0.00819        0.951        0.983
    ##     6    431      28    0.904 0.01380        0.877        0.931
    ##     9    403      34    0.828 0.01778        0.793        0.863

    plot(fit3, col=c("#3366CC","#CC66CC","#FF9900"), xlab="Survival Time from Outplant in Months", ylab="% Surviving", lwd=2)
    legend("bottomleft", title="Population", c("Dabob","Fidalgo","Oyster Bay"), fill=c("#3366CC","#CC66CC","#FF9900"))

![plot of chunk
unnamed-chunk-1](./KMgraphsWorking_files/figure-markdown_strict/unnamed-chunk-13.png)

    kmoys=read.csv("KMdataOys.csv")
    with(kmoys, tapply(Death[Status==1],Population[Status==1],mean))

    ##     H     N     S 
    ## 7.550 8.069 8.396

    with(kmoys, tapply(Death[Status==1],Population[Status==1],var))

    ##     H     N     S 
    ## 7.774 5.805 5.814

    fit4=with(kmoys, survfit(Surv(Death,Status)~Population))
    summary(fit4)

    ## Call: survfit(formula = Surv(Death, Status) ~ Population)
    ## 
    ##                 Population=H 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      50    0.896  0.0139        0.869        0.924
    ##     6    397       3    0.889  0.0144        0.861        0.918
    ##     9    394      52    0.772  0.0196        0.734        0.811
    ##    10    342      14    0.740  0.0206        0.701        0.782
    ##    11    328      21    0.693  0.0217        0.652        0.737
    ## 
    ##                 Population=N 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      47    0.902  0.0136        0.876        0.929
    ##     6    403      16    0.866  0.0157        0.836        0.898
    ##     9    387      94    0.656  0.0223        0.614        0.701
    ##    10    293      39    0.569  0.0233        0.525        0.616
    ##    11    254      21    0.522  0.0235        0.477        0.570
    ## 
    ##                 Population=S 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4    480      52    0.892  0.0142        0.864        0.920
    ##     6    395      25    0.835  0.0172        0.802        0.870
    ##     9    370      69    0.679  0.0220        0.638        0.724
    ##    10    301     110    0.431  0.0234        0.388        0.480
    ##    11    191      27    0.370  0.0229        0.328        0.418

    plot(fit4, col=c("#3366CC","#CC66CC","#FF9900"), xlab="Survival Time from Outplant in Months", ylab="% Surviving", lwd=2)
    legend("bottomleft", title="Population", c("Dabob","Fidalgo","Oyster Bay"), fill=c("#3366CC","#CC66CC","#FF9900"))

![plot of chunk
unnamed-chunk-1](./KMgraphsWorking_files/figure-markdown_strict/unnamed-chunk-14.png)

    mansurv<-survdiff(Surv(Death,Status)~Population,data=kmman,rho=1)
    print(mansurv)

    ## Call:
    ## survdiff(formula = Surv(Death, Status) ~ Population, data = kmman, 
    ##     rho = 1)
    ## 
    ##                N Observed Expected (O-E)^2/E (O-E)^2/V
    ## Population=H 480     46.8     69.8     7.584     12.95
    ## Population=N 480     82.9     66.6     4.001      6.66
    ## Population=S 480     73.0     66.3     0.676      1.12
    ## 
    ##  Chisq= 13.7  on 2 degrees of freedom, p= 0.00105

    dabsurv<-survdiff(Surv(Death,Status)~Population,data=kmdab)
    print(dabsurv)

    ## Call:
    ## survdiff(formula = Surv(Death, Status) ~ Population, data = kmdab)
    ## 
    ##                N Observed Expected (O-E)^2/E (O-E)^2/V
    ## Population=H 480      169      290     50.59    118.14
    ## Population=N 480      347      245     42.05     91.28
    ## Population=S 480      284      264      1.45      3.21
    ## 
    ##  Chisq= 141  on 2 degrees of freedom, p= 0

    fidsurv<-survdiff(Surv(Death,Status)~Population,data=kmfid)
    print(fidsurv)

    ## Call:
    ## survdiff(formula = Surv(Death, Status) ~ Population, data = kmfid)
    ## 
    ##                N Observed Expected (O-E)^2/E (O-E)^2/V
    ## Population=H 480       76     71.0     0.359     0.571
    ## Population=N 480       61     71.8     1.619     2.590
    ## Population=S 480       78     72.3     0.455     0.730
    ## 
    ##  Chisq= 2.6  on 2 degrees of freedom, p= 0.274

    oyssurv<-survdiff(Surv(Death,Status)~Population,data=kmoys)
    print(oyssurv)

    ## Call:
    ## survdiff(formula = Surv(Death, Status) ~ Population, data = kmoys)
    ## 
    ##                N Observed Expected (O-E)^2/E (O-E)^2/V
    ## Population=H 480      140      227    33.168     60.10
    ## Population=N 480      217      210     0.201      0.35
    ## Population=S 480      283      203    31.724     54.35
    ## 
    ##  Chisq= 76.3  on 2 degrees of freedom, p= 0
