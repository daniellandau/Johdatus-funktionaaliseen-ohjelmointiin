module W3 where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

-- ATTENZION! Palauta vain tiedosto joka _kääntyy_. Tämä tarkoittaa
-- sitä että komennon "runhaskell W3Test.hs" pitää toimia. Yksittäiset
-- testit siis saavat olla menemättä läpi mutta testien ajamisen tulee
-- toimia.
--
-- Ei myöskään ole suositeltavaa poistaa tässä pohjassa olevia
-- tyyppiannotaatioita. Ne kertovat mikä funktion tyypin _pitää_ olla.
-- Jos saat tyyppivirheitä, vika on toteutuksessasi, ei tehtäväpohjan
-- mukana tulevissa tyypeissä.

-- Tehtävä 1: Määrittele operaatio hei, joka tulostaa kaksi riviä,
-- joista ensimmäinen on "HEI" ja toinen on "MAAILMA".

hei :: IO ()
hei = do
  putStrLn "HEI"
  putStrLn "MAAILMA"

-- Tehtävä 2: Määrittele operaatio tervehdi siten, että tervehdi nimi
-- tulostaa "HEI nimi"

tervehdi :: String -> IO ()
tervehdi s = do
  putStrLn $ "HEI " ++ s

-- Tehtävä 3: Määrittele operaatio tervehdi', joka lukee nimen
-- näppäimistöltä ja sitten tervehtii kuten edellisessä tehtävässä.

tervehdi' :: IO ()
tervehdi' = do
  nimi <- getLine
  tervehdi nimi

-- Tehtävä 4: Määrittele operaatio lueSanat n joka lukee käyttäjältä n
-- sanaa (yksi per rivi) ja palauttaa ne aakkosjärjestyksessä

lueSanat :: Int -> IO [String]
lueSanat n = do
  if (n > 0)
    then lueSanat' n []
    else return []
  where
    lueSanat' 0 ss = return $ sort ss
    lueSanat' n ss = do
      sana <- getLine
      lueSanat' (n-1) (sana:ss)
      
-- Tehtävä 5: Määrittele operaatio lueKunnes f, joka lukee käyttäjältä
-- merkkijonoja ja palauttaa ne listana. Lukeminen lopetetaan kun f
-- palauttaa luetulle alkiolle True. (Sitä alkiota jolle f palauttaa
-- True ei liitetä listaan).

lueKunnes :: (String -> Bool) -> IO [String]
lueKunnes f = lueKunnes' f []
  where
    lueKunnes' f ss = do
      sana <- getLine
      if (f sana)
        then return $ reverse ss
        else lueKunnes' f (sana:ss)

-- Tehtävä 6: Määrittele operaatio printFibs n, joka tulostaa n
-- ensimmäistä fibonaccin lukua, yhden per rivi

-- kanoninen impelementaatio, paitsi alku 1:1, eikä 0:1. Lähde:
-- http://www.haskell.org/haskellwiki/The_Fibonacci_sequence#Canonical_zipWith_implementation
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

printFibs :: Int -> IO ()
printFibs n = mapM_ putStrLn $ map show (take n fibs)

-- Tehtävä 7: Määrittele operaatio isums n, joka lukee käyttäjältä n
-- lukua ja palauttaa niitten summan. Lisäksi jokaisen luvun jälkeen
-- tulostetaan siihenastinen summa luvuista.
          
isums :: Int -> IO Int
isums 0 = return 0
isums n = isums' n 0

isums' :: Int -> Int -> IO Int
isums' 0 sum = return sum
isums' n sum = do
  line <- getLine
  let num  = read line
      sum' = sum + num
  putStrLn $ show sum'
  isums' (n-1) sum'

-- Tehtävä 8: when on hyödyllinen funktio, mutta sen ensimmäien
-- argumentti on tyyppiä Bool. Toteuta whenM joka toimii samoin mutta
-- ehto on tyyppiä IO Bool.

whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
  cond' <- cond
  when cond' op

-- Tehtävä 9: Toteuta funktio while ehto operaatio, joka suorittaa
-- operaatiota niin kauan kun ehto palauttaa True.
-- 
-- Esimerkkejä:
-- while (return False) (putStrLn "MAHDOTONTA")  -- ei tulosta mitään
-- 
-- let kysy :: IO Bool
--     kysy = do putStrLn "K/E?"
--               line <- getLine
--               return $ line == "K"
-- in while kysy (putStrLn "JEE!") 
--
-- Tämä tulostaa JEE niin kauan kuin käyttäjä vastaa K

while :: IO Bool -> IO () -> IO ()
while ehto op = whenM ehto $ (do op
                                 while ehto op)

-- Tehtävä 10: Toteuta funktio debug, joka ottaa merkkijonon s ja
-- IO-operaation op, ja palauttaa IO-operaation joka tulostaa annetun
-- s, kutsuu op, ja tulostaa jälleen s. Lopuksi operaation pitäisi
-- palauttaa op:n palautusarvo.

debug :: String -> IO a -> IO a
debug s op = do
  putStrLn s
  op
  putStrLn s
  op

-- Tehtävä 11: Toteuta itse funktio mapM_. Saat käyttää (puhtaita)
-- listafunktioita ja listojen hahmontunnistusta

mymapM_ :: (a -> IO b) -> [a] -> IO ()
mymapM_ f xs = do mySequence_ $ map f xs
                  return ()
                  
mySequence_ :: [IO b] -> IO ()
mySequence_ [] = return ()
mySequence_ (x:xs) = do x
                        mySequence_ xs
                    

-- Tehtävä 12: Toteuta itse funktio forM. Saat käyttää (puhtaita)
-- listafunktioita ja listojen hahmontunnistusta

myforM :: [a] -> (a -> IO b) -> IO [b]
myforM as f = do mySequence $ map f as

mySequence :: [IO b] -> IO [b]
mySequence [] = return []
mySequence bs = mySequence' bs []
  where mySequence' :: [IO b] -> [b] -> IO [b]
        mySequence' [] bsCum = return $ reverse bsCum
        mySequence' (b:bs) bsCum = do
          b' <- b
          mySequence' bs (b':bsCum)

-- Tehtävä 13: Joskus törmää IO-operaatioihin jotka palauttavat
-- IO-operaatiota. Esimerkiksi IO-operaatio joka palauttaa
-- IO-operaation joka palauttaa Intin on tyypiltään IO (IO Int).
--
-- Toteuta funktio tuplaKutsu, joka ottaa IO-operaation joka palauttaa
-- IO operaation. tuplaKutsu op palauttaa IO-operaation joka
--   1. kutsuu op
--   2. kutsuu op:n palauttamaa operaatiota
--   3. palauttaa tämän palauttaman arvon
--
-- Esimerkkejä: 
--   - tuplaKutsu (return (return 3)) on sama kuin return 3
--
--   - let op :: IO (IO [String])
--         op = do l <- readLn
--                 return $ replicateM l getLine
--     in tuplaKutsu op
--
--     toimii kuten
--
--     do l <- readLn
--        replicateM l getLine
--
-- Tämä tehtävä on siitä mielenkiintoinen että jos saat sen menemään
-- tyypintarkastuksesta läpi, se on lähes välttämättä oikein.

tuplaKutsu :: IO (IO a) -> IO a
tuplaKutsu op = do op' <- op
                   op'

-- Tehtävä 14: Monesti IO-operaatioita halutaan ketjuttaa. Toteuta
-- funktio yhdista joka toimii hieman kuten operaattori (.)
-- funktioille. yhdista siis ottaa operaation op1 tyyppiä
--     a -> IO b
-- ja operaation op2 tyyppiä
--     c -> IO a
-- ja arvon tyyppiä
--     c
-- ja palauttaa operaation op3 tyyppiä
--     IO b
-- op3 tekee tietenkin seuraavaa:
--   1. ottaa argumenttinsa (tyyppiä c) ja syöttää sen op2:lle
--   2. ottaa tämän lopputuloksen (tyyppiä a) ja syöttää sen op1:lle
--   3. palauttaa lopputuloksen (tyyppiä b)
--
-- Tämä tehtävä on siitä mielenkiintoinen että jos saat sen menemään
-- tyypintarkastuksesta läpi, se on lähes välttämättä oikein.

yhdista :: (a -> IO b) -> (c -> IO a) -> c -> IO b
yhdista op1 op2 c = op2 c >>= op1
--yhdista op1 op2 c = do a <- op2 c
--                       op1 a
                       
-- Tehtävä 15: Tutustu modulin Data.IORef dokumentaatioon
-- <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html>
--
-- Toteuta funktio mkCounter, joka palauttaa operaatiot inc :: IO ()
-- ja get :: IO Int. Näitten operaatioitten tulee toimia yhteen seuraavasti:
--
-- 1. jos operaatiota inc ei ole ajettu kertaakaan, palauttaa get arvon 0
-- 2. operaation inc ajaminen kasvattaa seuraavien get-kutsujen palautusarvoa
--
-- Kyseessä on siis yksinkertainen tilallinen laskuri

mkCounter :: IO (IO (), IO Int)
mkCounter = do
  r <- newIORef 0
  let inc = do
        val <- readIORef r
        writeIORef r (val+1)
        return ()
      get = readIORef r
    in return (inc,get)

-- Tehtävä 16: Toteuta operaatio hFetchLines, joka hakee annetusta
-- tiedostoskahvasta rivit, joitten rivinumerot (rivinumerointi alkaa
-- 1:stä) ovat annetussa listassa. Voit olettaa että rivinumerolista
-- on nousevassa järjestyksessä.
--
-- Modulin System.IO dokumentaatio auttanee.

hFetchLines :: Handle -> [Int] -> IO [String]
hFetchLines h nums = do ls <- hGetContents h
                        let lsWithNums = zip [1..] $ lines ls
                        getLines' lsWithNums nums []
  where getLines' :: [(Int,String)] -> [Int] -> [String] -> IO [String]
        getLines' ls [] lsCum = return $ reverse lsCum
        getLines' (l:ls) (n:nums) lsCum = 
          let (lineNum,line) = l
          in if (lineNum == n)
             then getLines' ls nums (line:lsCum)
             else getLines' ls (n:nums) lsCum


-- Tehtävä 17: CSV on tiedostoformaatti, jossa taulukollinen arvoja on
-- tallenettu tiedostoon niin, että tiedoston yksi rivi vastaa
-- taulukon yhtä riviä, ja rivin alkiot on eroteltu ,-merkeillä.
--
-- Tee funktio readCSV joka lukee CSV-tiedoston listaksi listoja.
--
-- Huom! Funktiosi ei tarvitse osata käsitellä lainausmerkkejä,
-- kenoviivoja, eikä muitakaan erinäisten CSV-formaattien hienouksia.
-- Voit olettaa että jokainen kerkki , syötteessä on kentän raja.
--
-- Huom! Eri riveillä voi olla eri määrä kenttiä

readCSV :: FilePath -> IO [[String]]
readCSV path = do
  h <- openFile path ReadMode
  contents <- hGetContents h
  let ls = lines contents
  return $ map (splitC ',') ls

splitC :: Char -> String -> [String]
splitC c [] = [[]]
splitC c ss = splitC' c ss []
  where splitC' :: Char -> String -> [String] -> [String]
        splitC' c [] cums = reverse $ map reverse cums -- lopuksi
        splitC' c (s:ss) [] = splitC' c ss ([s]:[])    -- aluksi
        splitC' c (s:ss) (cum:cums)
          | c == s    = splitC' c ss ([]: cum : cums)  -- lisää sana
          | otherwise = splitC' c ss ((s:cum) : cums)  -- lisää kirjain sanaan
        
-- Tehtävä 18: Toteuta operaatio compareFiles, joka saa kaksi
-- tiedostonimeä, a ja b. Tiedostojen sisältöjen haluttaisiin olevan
-- samat, mutta niissä on jotakin eroja. Siispä kun tiedostojen a ja b
-- rivit nro i poikkeavat toisistaan, tulostaa ohjelma:
--
-- < tiedoston a versio rivistä
-- > tiedoston b versio rivistä 
--
-- Esimerkki:
--
-- Tiedoston a sisältö:
-- a
-- aa
-- x
-- aa
-- bb
-- cc
--
-- Tiedoston b sisältö:
-- a
-- aa
-- bb
-- aa
-- cc
-- dd
-- 
-- Tulostus:
-- < x  
-- > bb
-- < bb 
-- > cc
-- < cc
-- > dd
--
-- Huom! Voit olettaa että tiedostoissa on sama määrä rivejä.
--
-- Vihje! Eroavien rivien löytäminen on hyödyllistä erottaa omaksi
-- puhtaaksi funktiokseen (jonka tyyppi voi olla vaikkapa [String] ->
-- [String] -> [String]).

compareFiles :: FilePath -> FilePath -> IO ()
compareFiles a b = do
  ha <- openFile a ReadMode
  hb <- openFile b ReadMode
  contentsA <- hGetContents ha
  contentsB <- hGetContents hb
  let linesA = lines contentsA
      linesB = lines contentsB
  putStr $ unlines $ diff linesA linesB

diff :: [String] -> [String] -> [String]
diff linesA linesB = diff' linesA linesB []
  where diff' :: [String] -> [String] -> [String] -> [String]
        diff' [] [] diffs = reverse diffs
        diff' (lineA:linesA) (lineB:linesB) diffs 
          | lineA == lineB = diff' linesA linesB diffs
          | otherwise = diff' linesA linesB (("> " ++ lineB) : ("< " ++ lineA) : diffs)
          
-- Tehtävä 19: Tässä tehtävässä näet miten funktionaalisessa
-- ohjelmassa logiikan voi toteuttaa puhtaana funktiona, jota ympäröi
-- yksinkertainen IO-"ajuri".
--
-- Toteuta funktio interact', joka ottaa puhtaan funktion f tyyppiä
--   (String,a) -> (Bool,String,a)
-- ja alkutilan tyyppiä a ja palauttaa IO-operaation tyyppiä IO a
-- 
-- interact':n tulisi toimia niin että se lukee käyttäjältä rivin,
-- syöttää rivin ja tämänhetkisen tilan f:lle. f palauttaa booleanin,
-- tulosteen ja uuden tilan. f:n palauttama tuloste tulostetaan
-- ruudulle, ja jos palautettu boolean on True, jatketaan f:n
-- suorittamista uudella tilalla. Jos palautettu boolean on False,
-- loppuu suoritus ja operaatio palauttaa lopputilan.
--
-- Esimerkki:
--
-- let f :: (String,Integer) -> (Bool,String,Integer)
--     f ("inc",n)   = (True,"",n+1)
--     f ("print",n) = (True,show n,n)
--     f ("quit",n)  = (False,"bye bye",n)
-- in interact' f 0
--

interact' :: ((String,a) -> (Bool,String,a)) -> a -> IO a
interact' f state = do
  str <- getLine
  let triplet = f (str,state)
      (cont, msg, state') = triplet
  putStr msg
  if (cont)
    then interact' f state'
    else return state'
    
