module BinaryNumber where

import Data.List
import Data.Tuple (swap)

{-
    Reprezentarea unui număr binar ca listă finită sau infinită de biți.
    Primul bit este cel mai puțin semnificativ.

    De exemplu, 6 poate fi reprezentat ca [0, 1, 1, 0, 0, ...].

    type introduce un sinonim de tip, similar cu typedef din C.
-}

type BinaryNumber = [Int]

{-
    *** TODO ***

    Transformă un număr din reprezentarea binară finită în cea zecimală.
    
    Constrângeri: funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând o funcțională (fără recursivitate explicită).

    Exemple:
    
    > toDecimal [0,1,1]
    6
-}

toDecimal :: BinaryNumber -> Int
toDecimal = foldl (\acc x -> acc * 2 + x) 0 . reverse

--reverse asupra listei de biți binari pentru a putea parcurge lista de la cifra cea mai semnificativă la cea mai puțin semnificativă.
--foldl primește o funcție lambda și un acumulator inițial. Lambda primește două argumente: acumulatorul și
--elementul curent din listă. Funcția lambda calculează valoarea decimala înmulțind acumulatorul cu 2 și adăugând elementul curent (0 sau 1).

{-
    *** TODO ***

    Transformă un număr din reprezentarea zecimală în cea binară infinită.

    Constrângeri: pt bonus 10p, funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând funcționala unfoldr (fără recursivitate explicită).

    Spre deosebire de foldr, care împăturește o listă la o singură valoare,
    unfoldr desfășoară procesul invers, construind o listă pe baza unei valori
    inițiale.
    
    Hint: divMod.

    Exemple:
    
    > take 10 $ toBinary 6
    [0,1,1,0,0,0,0,0,0,0]
-}

toBinary :: Int -> BinaryNumber
toBinary = unfoldr (\x -> Just (snd (divMod x 2), fst (divMod x 2)))

--unfoldr primește o funcție lambda și un parametru de start. În cazul de față, funcția lambda primește un număr întreg n
--și utilizează funcția divMod pentru a calcula restul și câtul împărțirii lui n la 2. Restul reprezintă ultimul bit din reprezentarea binară,
--iar câtul este utilizat pentru a calcula următorul bit din reprezentare.
--Aceste valori (restul, câtul) sunt apoi ambalate într-o valoare Just și returnate de funcția lambda. Valoarea de continuare a secvenței
--unfoldr este setată pe câtul calculat anterior. Procesul se repetă pentru acest nou număr, până când valoarea de continuare devine 0

{-
    *** TODO ***

    Incrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la transport!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > inc [0,1,1] 
    [1,1,1]

    > inc [1,1,1]
    [0,0,0,1]
-}

inc :: BinaryNumber -> BinaryNumber
inc [] = []
inc [0] = [1]
inc (0:xs) = 1 : xs
inc (1:xs) = 0 : inc xs

{-
   *** TODO ***

    Decrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la împrumut!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > dec [1,1,1]
    [0,1,1]

    > dec [0,0,0,1]
    [1,1,1]
-}

dec :: BinaryNumber -> BinaryNumber
dec [] = []
dec [0] = [0]
dec [1] = [0]
dec (0:xs) = 1 : dec xs
dec (1:xs) = 0 : xs

{-
    *** TODO ***

    Adună două numere binare, asumând reprezentări infinite, pentru ușurința
    aplicării funcționalelor.

    Constrângeri: utilizați
    * where sau let
    * pt bonus 10p, funcționala mapAccumL (fără recursivitate explicită).

    mapAccumL are tipul (a -> b -> (a, c)) -> a -> [b] -> (a, [c]).
    Așa cum sugerează numele, combină comportamentele funcționalelor:
    * map, transformând element cu element [b] în [c]
    * foldl, utilizând în același timp un acumulator de tipul a.

    Exemple:

    > take 10 $ add (toBinary 74) (toBinary 123)
    [1,0,1,0,0,0,1,1,0,0]
    
    > toDecimal $ take 10 $ add (toBinary 74) (toBinary 123)
    197
-}

diivMood :: Int -> (Int, Int) -> (Int, Int)
diivMood acc (x, y) =
  let (d, m) = divMod (x + y + acc) 2
  in (fst (d, m), snd (d, m))

add :: BinaryNumber -> BinaryNumber -> BinaryNumber
add bits1 bits2 = snd $ mapAccumL diivMood 0 (zip bits1 bits2)

--funcția mapAccumL pentru a aplica funcția diivMood pe perechile de biți corespunzătoare din cele două liste și pentru
--a calcula transportul și bitul rezultat pentru fiecare pereche
--snd extrage doar lista de rezultate din perechea întoarsă de mapAccumL

{-
    *** TODO ***

    În pregătirea operației de înmulțire a două numere binare, cu reprezentări
    infinite, stivuiește copii deplasate la dreapta ale lui bits1, înmulțite
    cu bit-ul curent din bits2. Deplasarea se face adăugând la stânga lui bits1
    un număr de 0-uri dat de numărul liniei curente. Întoarce o listă infinită
    de liste infinite.

    Vizual:

    0 1 1 0 ... *   <- bits1
    1 0 1 0 ...     <- bits2
    -----------
   |0 1 1 0 ...        înmulțire bits1 cu 1 și deplasare 0 poziții la dreapta
    0|0 0 0 0 ...      înmulțire bits1 cu 0 și deplasare 1 poziție la dreapta
    0 0|0 1 1 0 ...    înmulțire bits1 cu 1 și deplasare 2 poziții la dreapta

    Constrângeri:
    * Corpul funcției trebuie să fie un list comprehension.
    * Nu utilizați recursivitate explicită.

    Hint: iterate pt generarea secvențelor de deplasare spre dreapta cu biți 0.

    Exemple:

    (exemplul vizual)
    > take 3 $ map (take 6) $ stack (toBinary 6) (toBinary 5)
    [[0,1,1,0,0,0],[0,0,0,0,0,0],[0,0,0,1,1,0]]
-}

aux :: BinaryNumber -> Int -> BinaryNumber
aux bits b = [x * b | x <- bits]

stack :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stack bits1 bits2 = [replicate a 0 ++ aux bits1 b | (a, b) <- zip [0,1..] bits2]

--aux primește un număr binar și un întreg, și întoarce numărul binar rezultat prin înmulțirea fiecărui element al listei cu acel întreg
--stack primește două numere binare și întoarce o listă de numere binare, unde fiecare număr binar este rezultatul înmulțirii primului număr
--binar cu un bit din al doilea număr binar și deplasarea la dreapta a primului număr binar cu un număr de poziții egal cu poziția bitului din
--al doilea număr binar
--stack întoarce o listă de numere binare, unde fiecare număr binar este rezultatul înmulțirii primului număr binar cu un bit din al doilea

{-
    *** TODO ***

    Întoarce o listă infinită de numere binare, care pe poziția i >= 0 conține
    rezultatul înmulțirii lui bits1 cu numărul format din primii i biți
    ai lui bits2, i.e. suma primelor i linii întoarse de stack.

    Constrângeri:
    * Utilizați funcționala scanl (fără recursivitate explicită).

    Spre deosebire de foldl, care întoarce acumulatorul final, scanl întoarce
    o listă cu toate acumulatoarele intermediare.

    Exemple:
    
    > take 5 $ map (toDecimal . take 10) $ multiply (toBinary 6) (toBinary 5) 
    [0,6,6,30,30]
-}

multiply :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
multiply bits1 bits2 = scanl add [0,0..] (stack bits1 bits2)

{-"multiply" primește două numere binare, "bits1" și "bits2", și returnează o listă de numere binare care reprezintă fiecare etapă
a înmulțirii bit cu bit a celor două numere.
funcția mai întâi "stochează" fiecare bit din numărul "bits2", apoi utilizează funcția "scanl" pentru a aplica funcția "add" asupra
listei din stack și listei "0,0,..", care va produce o listă de numere binare ce reprezintă fiecare etapă a înmulțirii bit cu bit.-}







--Rezultatul final este lista de numere binare reprezentând toate valorile intermediare obținute prin aplicarea funcției "add"
--asupra tuturor liniilor matricei construite de stack. Ultimul element din această listă reprezintă produsul final al celor două numere
--binare primite inițial ca argumente.

{-
    *** TODO ***

    Întrebare de reflecție, la care veți răspunde la prezentarea temei.

    Având în vedere că liniile întoarse de stack care conțin exclusiv biți 0
    nu contribuie la rezultatul unei înmulțiri, să presupunem că modificăm
    definiția funcției stack astfel încât să construiască doar liniile utile;
    de exemplu, folosind filter sau pattern matching în list comprehension
    pt a păstra doar biții 1 din bits2. Ce probleme ar putea crea această
    abordare?
-}
