--VASILEIOS KAPSALIS
--4080
-----------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------

-- ASKHSH 1

--wordList :: String->[String]

--wordList s = ["2022"]                                         


wordList :: String->[String]
wordList [] = [] --list
wordList (g:f)
      | s == []   = []
      | otherwise = s : wordList (l) --xanakalw me 
           where s = stringFinder ( findLetterLeft (g:f) )
                 l = previousDelete ( findLetterLeft (g:f) ) s


stringFinder :: String->String
stringFinder [] = []
stringFinder (g:h)
      | (g>='a' && g<='z') || (g>='A' && g<='Z') = g : (stringFinder h)
      | otherwise                                = []


previousDelete :: String->String->String --kanw reset to string gia na dexetai ta nea orismata 
previousDelete stringF [] = stringF -- call of list in 
previousDelete (part1:part2) (part3:part4) = previousDelete part2 part4


findLetterLeft :: String->String --kanw delete ola ektos apo grammata me value letters 
findLetterLeft [] = []
findLetterLeft (g:h)
      | (g>='a' && g<='z') || (g>='A' && g<='Z') = (g:h)
      | otherwise                                = findLetterLeft h