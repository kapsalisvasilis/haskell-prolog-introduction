--Lab1.hs
--Vasileios Kapsalis cs04080@uoi.gr
--AM 4080
--askisi 1
-- turnin Haskell-1@myy401 Lab1.hs

-- -----------------------------------------------------

-- ASKHSH 1  example amount :: Int->Float->Float amount n p = -2.022  
-- ASKHSH 1 
amount :: Int->Float->Float

amount n p 
	= if (n<=0 || p<=0) then 0
	else if (n<5) then sumItUp n p -- >= 5 ena dwro
	else if (n<9) && (n>=5) then sumItUp (n-1) p -- >= 5 && <9 to dwro
	else if (n<12) && (n>=9) then sumItUp (n-2) p -- <-12 && >=9 2o dwro
	else sumItUp (n-2 - ((n-9)`div` 3)) p

sumItUp :: Int->Float->Float

sumItUp amount1 objectvalue
	= if (fromIntegral amount1)*objectvalue>100 then (fromIntegral amount1)*objectvalue*(1-0.10) -- overall ekptwsh synolou 10% fromIntegral giati petage error int float
	else (fromIntegral amount1)*objectvalue

-- ---------------------------------------------------------------------------------------
     
-- ASKHSH 2 cost :: (Int,Int,Int)->(Int,Int,Int)->Float -- cost (h1,m1,s1) (h2,m2,s2) = -2.022  
-- ASKHSH 2 


cost :: (Int,Int,Int)->(Int,Int,Int)->Float
--float p dinetai einai sunoliko kostos 
cost (h1,m1,s1) (h2,m2,s2)
	|h1<=h2 
		= telosKinhths ((h2-h1) * 3600 + (m2-m1) * 60 + (s2-s1))
	|True == True 
		= telosKinhths ((12+12+h2-h1) * 3600+(m2-m1) * 60+(s2-s1)) -- 2 12wra gia diorthwsh arnhtikou

telosKinhths :: Int->Float

telosKinhths lepta 
	= if (lepta ==0) then 0
	else if (lepta <=180) then 0.58 
	else 0.58+(fromIntegral(lepta -180))*0.003 --same me fromintegral prepei na parexei float