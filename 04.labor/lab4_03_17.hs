import Control.Monad.Trans.Cont (reset)
-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
negyzet n=take n [i^2| i<- [0,2..]]

negyzet2 :: (Num a, Enum a) => Int -> [a]
negyzet2 n = take n $ map(\i -> i ^2)[2,4..]

negyzet3 :: Int -> IO ()
negyzet3 n =  mapM_ (\(szam, negyzete) -> putStrLn ( show szam ++ " negyzete " ++ show negyzete)) ls 
    where 
        ls= take n $ map (\i -> (i,i^2)) [2,4..]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamokLs 1 = replicate 1 1
szamokLs n = szamokLs (n-1) ++ replicate n n


szamok2Ls n i  
    |i /= n = replicate i i ++ szamok2Ls n (i+1)
    |otherwise= replicate i i


-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,

szamokLsParos n i j
    | i /= n = replicate i (j +2) ++ szamokLsParos n (i+1) (j+2)
    |otherwise = replicate i i 


szamokLsParos2 n i 
    | i/=n = replicate i (i*2) ++ szamokLsParos2 n (i+1)
    | otherwise = replicate i (i+2)
-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,

szamokLs5 n = [n,n-1 .. 1]++[1..n]
-- - váltakozva tartalmazzon True és False értékeket,

valtakozo n = take n $  ls
    where
        ls=[True,False] ++ ls

-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

valtakozo2 n = take n $  ls
    where
        ls=[0,1,-1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok x =length [i| i<-[1..x], mod x i==0]

osztok2 x=myLength [i| i<-[1..x], mod x i==0]
    where 
        myLength []=0
        myLength (_ : ls) = 1 + myLength ls

osztok3 x= foldl(\res i -> if mod x i == 0 then res+1 else res) 0 [1 .. x]

osztok4 x= foldl(\res i -> if mod x i == 0 then res+1 else res) 0 [1 .. div x 2]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOszto n = last [i | i<- [1,3..n], mod n i ==0]

maxParatlanOsztok3 n = myMaximum [i | i <- [1 .. n], mod n i == 0, odd i]
    where
        myMaximum [x]=x
        myMaximum (x1:x2:xs)
            | x1>x2 =myMaximum(x1:xs)
            | otherwise= myMaximum(x2:xs)

paratlanOsztok5 n 
    |odd n =n
    |otherwise = foldl(\acc x -> if mod n x == 0 then x else acc ) 1 [1,3..n];


maxParatlanOsztok2 n = last [i | i <- [1 .. n], mod n i == 0, odd i]


-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP x p
    |x<p=[x]
    |otherwise = decP (div x p) p ++ [mod x p]


decPszam x p = length $ decP x p;

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decpMax x p = maximum $ decP x p
-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

fibo a b = dropWhile (<a) $ fiboSg 0 1 0
    where 
        fiboSg a1 b1 res
            | res < b = fiboSg b1 res (res+b1)
            |otherwise = [res]

fibo2 = fibosg 0 1 0
    where
        fibosg a b res = res : fibosg b res (b + res)

fiboAB a b  = dropWhile (> a) $ takeWhile (<b) fiboAB 0 1 0

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
-- - tükrözi egy lista elemeit,
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.


-- main :: IO ()
-- main = do 
--negyzet3 4
---print(negyzet2 4)