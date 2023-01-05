--INTRODUCTION HASKELL 
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



--search :: Integer->Integer->Integer->Integer

--search a k m = -2022    
-- ASKHSH 1


search :: Integer->Integer->Integer->Integer

search a k m = leastPossibleNumber a k m 1

leastPossibleNumber :: Integer->Integer->Integer->Integer->Integer
leastPossibleNumber a k m n
     |((n+a)^k) < m^n = n 
     |otherwise = leastPossibleNumber a k m (n+1)    



-----------------------------------------------------------------------------------------
     -- ASKHSH 2

--compress :: Integer->Integer

--compress n = -2022   
-- ASKHSH 2

compress :: Integer->Integer
compress n 
     |n`div`10==0 = n
     |otherwise = compress (plsmosdigit n)

plsmosdigit :: Integer->Integer
plsmosdigit n
     |leftn>0 && rightnum>0 = rightnum*(plsmosdigit leftn) --left kommati arithmoi loopa me ton dexia
     |leftn==0 && rightnum>0 = rightnum  --dexia arithmos gia to telos
     |otherwise = plsmosdigit leftn --sunexeia loopas 
     where leftn = n`div`10
           rightnum = n`mod`10






xsum :: [Integer]->Integer --pairnei lista dinei apotelesma

xsum [] = 0 --lista 
xsum (integL : []) = 0 --arxikopoihsh
xsum (integL : t) = sumadderV2 (integL : t) 0 --anagastika paw sthn voithitikh gia na krataw timh



sumzeroV1 :: [Integer] -> Integer
sumzeroV1 (integL : t) = integL + sumzeroV1 t --kalw 2h voithitikh
sumzeroV1 [] = 0                                       

sumadderV2 :: [Integer]->Integer->Integer --theloume anagastika lista me int int gia thn ylopoihsh
--epeidh den mporoume na to valoyme sthn main to exoume se voithitikh
sumadderV2 (integL : []) n = 0 -- =0 
sumadderV2 (integL : t) n = (n+integL) * (sumzeroV1 t) + sumadderV2 t (n+integL) --sumadderv2 kaleite anadromikh
--mesa san loop gia na parei oles tis times kai ** me sumzerovoithitikh1


-------------------------------------------------------------------


compress :: Integer->Integer

compress n 
     |n`div`10 == 0 = n
     |otherwise= compress p
     where p = pollaplasiasmos n

pollaplasiasmos :: Integer->Integer
pollaplasiasmos n 
     |x==0 = pollaplasiasmos y
     |x>0 && y==0 = x
     |x>0 && y>0 = x*(pollaplasiasmos y) 
     where x = n`mod`10
           y = n`div`10




 -----------------------------------------



 trace :: [(Int,Int)]->[(Int,Int)]
trace [] = []
trace s = (trace2 (0,0) s) ++ [(0,0)]

trace2 :: (Int,Int)->[(Int,Int)]->[(Int,Int)]
trace2 (x,y) [] = trace3 (x,y) (0,0)
trace2 (x,y) (h:t) = trace3 (x,y) h ++ (trace2 h t)

-----------------------------------------------------------------------------------------

askhsh2 
minIntList :: [Int]->Int
minIntList (h:[])=h
minIntList (h:t)=min h(minIntList t)

h::[Int]
h=[]

temp::[Int]->Int->Int
temp [] b =0
temp a b =abs((head a) - b)

checklist::[Int]->Int->[Int]
checklist a b  = if tail a == [] then  temp a b:h 
else  temp a b:h ++ checklist (tail a) b 

result::[Int]->Int->Int
result a b = if minIntList (checklist a b)  == head (checklist a b) then 1 
else 1 + result (tail a) b


nearest :: [Int]->Int->Int
nearest s n
    |length s ==0 = error "nearest :wrong syntax" 
    |length s ==1 = 1
    |otherwise    = result s n  

-----------------
split::String->[String]
split [] = [""]
split(h:t)
    |h==' ' ="" :rest
    |otherwise=(h:head rest) :tail rest
               where rest=split t

flipWords::[String]->String

flipWords a
    |length a ==1 = head a
    |a==[] ="" 
    |(tail a)==[[]]="" 
    |(tail(tail a))==[[]] =head(tail a)++" "++ head a
    |otherwise= head(tail a)++" "++ head a++" "++flipWords(tail(tail a)) 


swap :: String->String
swap s=flipWords(split s)

---