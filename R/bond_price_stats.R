#zero coupon price for daily yield

bond_price <- function(y, c, m, p, f, d){
    #helper function for bond_stats, which passes arguments to calculate 
    #bond price for other calculations in bond_stats. 
    
    #y = yield, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield
    
    if(f > 1){
        stop("coupon frequency must be annual (1) or less (0.5 = semi annual)")
    }
    
    
    bond <<- data_frame(period = seq(f, m, f), 
                        payment = c(rep(c*p*f, m/f-1), 
                                    (c*p*f + p)),
                        discount_factor = 1/((1+y)^(period)), 
                        pv = discount_factor*payment)
    
    price <<- sum(bond$pv)
    
    print(price)
    
    
}
bond_stats <- function(yield = 0.05, coupon.rate = 0.05, ytm = 10, par.value = 1000, coupon.freq = 1, chg.yield = 0){
    
    bond_price(y = yield, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield)
    
    #to show duration and convexity adjustments
    if(chg.yield != 0){
        eff.duration <<- (bond_price(y = yield-0.01, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield) - 
                              bond_price(y = yield+0.01, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield))/ 
            (2*bond_price(y = yield, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield)*.01)
        
        eff.convexity <<- ((bond_price(y = yield-0.01, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield) + 
                                bond_price(y = yield+0.01, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield)) - 
                               2*bond_price(y = yield, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield)) / 
            (bond_price(y = yield, c = coupon.rate, m = ytm, p = par.value, f = coupon.freq, d = chg.yield)*(.01^2))
        
        bond.pct.chg <<- -eff.duration*chg.yield + 0.5*eff.convexity*chg.yield^2
        
        dol.value.chg <<- bond.pct.chg*price
    }
    
}