import Control.Monad.RWS (MonadState(put))
-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,

osszeg :: Num a => a -> a -> a
osszeg a b = a + b 

kulonbseg :: Double -> Double -> Double
kulonbseg a b = a - b

szorzat :: Int -> Int -> Int
szorzat a b = a * b

hanyados :: Fractional a => a -> a -> a
hanyados a b = a / b

hanyados2 :: Integral a => a -> a -> a
hanyados2 a b = div a b


osztasi_maradek :: Integral a => a -> a -> a
osztasi_maradek a b= mod a b

-- - egy első fokú egyenlet gyökét,
elsoF a b = (-b)/a 

-- - egy szám abszulút értékét, ez az abs fg 
abszolut a
    | a < 0 = -a
    | otherwise = a

abszolut2 a = if a < 0 then -a else a


-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozititv" else "nulla"


elojel2 n
  | n < 0 = "negativ"
  | n > 0 = "pozititv"
  | otherwise = "nulla"

-- - két argumentuma közül a maximumot,

maxi a b = if a > b then a else b


-- - két argumentuma közül a minimumot,
mini a b = if a < b then a else b
-- - egy másodfokú egyenlet gyökeit,
masodF a b c
  | delta < 0 = error "komplex szamok"
  | delta == 0 = [gy1]
  | otherwise = [gy1, gy2]
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b - sqrt delta) / (2 * a)
    gy2 = (-b + sqrt delta) / (2 * a)

masodF2 a b c
  | delta < 0 = error "komplex szamok"
  | otherwise = (gy1, gy2)
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b - sqrt delta) / (2 * a)
    gy2 = (-b + sqrt delta) / (2 * a)

masodF3 a b c = if delta < 0 then error "komplex szamok" else (gy1, gy2)
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b - sqrt delta) / (2 * a)
    gy2 = (-b + sqrt delta) / (2 * a)
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetGyokN n=[ sqrt | i <- [ 1..n]]
-- - az első n négyzetszámot,
negyzetN :: (Enum a, Floating a) => a -> [a]
negyzetN n=[i **i | i<-[1..n]]
-- - az első n természetes szám köbét,
kobN :: (Num a, Enum a) => a -> [a]
kobN n=[i ^ 3 | i<- [1..n]]
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzetN :: (Enum a, Eq a, Floating a) => a -> [a]
nemNegyzetN n=[i| i <- [1..n],(sqrt i* sqrt i)/=i]



-- - x hatványait adott n-ig,
xHatvanyN :: (Num a, Integral b) => a -> b -> [a]
xHatvanyN x n = [x^i|i<-[1..n]]

-- - egy szám páros osztóinak listáját,

osztokN :: Integral a => a -> [a]
osztokN n=[i| i<-[1..n], n `mod` i == 0, i `mod` 2 ==0]

osztokN2 :: Integral a => a -> [a]
osztokN2 n=[i|i<-[2,4..n], mod n i ==0]
-- - n-ig a prímszámok listáját,
osztok n=[i| i<- [1..n], mod n i ==0]
primszam :: Integral a => a -> Bool
primszam n= osztok n==[1,n]
primszamokN n=[i| i<-[2..n], primszam i]

primszamokN2 n=[i | i <- [2..n], primszamL i]
  where 
    primszamL n = osztokL n==[1,n]
    osztokL n = [i | i<- [1..n], mod n i ==0]


-- - n-ig az összetett számok listáját,
osszetettN n=[i | i<-[1..n], primszam i /= True]  -- not (primsmzam i)


-- - n-ig a páratlan összetett számok listáját,
osszetettParatlanN n=[i | i<-[3,5..n], not (primszam i)]  --od i
 

-- - az n-nél kisebb Pitágorászi számhármasokat, 
pitagorsz n= [(a,b,c) | c<-[1..n], b <-[1..c], a <- [1..b], a ** 2 +b ** 2== c**2]

-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuszam = zip[ 'a'..'z'][0..25]
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok1 = zip [0..5][5,4..0]
szamok2 n = zip [0..n][n,n-1..0]

szamok3 n= [(i,n-i)| i<- [0..n]]
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.

tfLs n = take n ls 
  where 
    ls= [True,False] ++ ls


main :: IO()
main=do
  putStrLn("x Hatvany n")
  print(xHatvanyN 5 3)
  putStrLn "Paros osztok 48"
  print (osztokN 48)
  putStrLn "Pitagoraszi szam harmasok "
  print (pitagorsz 100)
  putStrLn("Szamparok 15" ++ show (szamok3 15))