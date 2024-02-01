-----------------------------------------------------------------------------------------
--VASILEIOS KAPSALIS AM 4080
--LAB 4 
-- ASKHSH 1

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



-----------------------------------------------------------------------------------------


-- ASKHSH 2

apply :: Ord u => [v->u]->[v]->[u]

apply p s = []        
--ekana mia ylopoihsh me 3 voithikes alla den katafera ton algorithmo oute na bgazei to teliko apotelesma