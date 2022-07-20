set i 'plants'/1*50/;
set k 'markets'/1*50/;

parameter cap(i) Maximum capacity of plant i
$call GDXXRW gamsedited.xlsx par=cap rng=Sheet27!a2:b51  cdim=0 rdim=1
$GDXIN gamsedited.gdx
$Load cap
$GDXIN

parameter d(k) demand of finished goods at market k
$call GDXXRW gamsedited.xlsx par=d rng=Sheet27!a54:b103  cdim=0 rdim=1
$GDXIN gamsedited.gdx
$Load d
$GDXIN

parameter f(i) location cost at plant
$call GDXXRW gamsedited.xlsx par=f rng=Sheet27!a106:b155  cdim=0 rdim=1
$GDXIN gamsedited.gdx
$Load f
$GDXIN

parameter C(i,k) Cost of moving unit goods from plant i to market k
$call GDXXRW gamsedited.xlsx par=C rng=Sheet27!a158:ay208  cdim=1 rdim=1
$GDXIN gamsedited.gdx
$Load C
$GDXIN

variable

Y(i)       Binary variable to define the location of plant
Z(i,k)     Binary variable to define the supply between i and k
X(i,k)     qty of goods transported from ‘i’ to ‘k’
cost       total cost;

binary variable Y,Z ;
positive variable X ;
Equations

Objfn Objective function
quant_trans1       quantity of good transport 1
quant_trans2       quantity of good transport 2
loc                location of plant
sup                supply between i and k
dem_at_mkt         demand at a market
supplyfp           supply from plant
supply_at_mkt      supply at a market
supplyfop          supply from one plant
capacityfp         capacity of plant
strong_constr      strong constraint
;

Objfn..             cost =e= sum((i,k), X(i,k)*c(i,k)) + sum((i), Y(i)*f(i));
quant_trans1(i,k)..            X(i,k)  $ (d(k) > cap(i)) =e= d(k);
quant_trans2(i,k)..           X(i,k) $ (d(k) < cap(i)) =e= cap(i);
loc(i)..                    Y(i) = 1 $ f(i) =g= 0;
sup(i,k)..                    Z(i,k) = 1 $ X(i,k) =g= 0;

dem_at_mkt(i,k)..             X(i,k) =l= d(k)*cap(i) ;
supplyfp(k)..               sum((i), Z(i,k)) =e= 3;
supply_at_mkt(i)..          sum((k), Z(i,k)) =e= 3;
supplyfop(i)..              sum((k), Z(i,k))/3 =e= Y(i);
capacityfp..             sum((i), cap(i)*Y(i)) =g= sum((k), D(k));
strong_constr(k)..       sum((i), X(i,k)) =g= D(k);



model project1 /Objfn, dem_at_mkt, supplyfp, supply_at_mkt, supplyfop,
                capacityfp, strong_constr /;

solve project1 using mip minimizing cost;

display cost.l;
