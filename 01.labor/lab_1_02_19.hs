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
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
-- - az első n négyzetszámot,
-- - az első n természetes szám köbét,
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
-- - x hatványait adott n-ig,
-- - egy szám páros osztóinak listáját,
-- - n-ig a prímszámok listáját,
-- - n-ig az összetett számok listáját,
-- - n-ig a páratlan összetett számok listáját,
-- - az n-nél kisebb Pitágorászi számhármasokat,
-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.

