--SET 2 arxes glwsswn programmatismou
--VASILEIOS KAPSALIS AM 4080
--5/4/22
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

-- ASKHSH 1


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