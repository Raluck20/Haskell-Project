module BinomialHeap where

import Data.Function (on)
import Data.List

{-
    Reprezentarea unui arbore binomial, având priorități de tipul p și chei
    de tipul k. Conform proprietății definitorii a acestor arbori, fiecare
    copil i dintre cei r copii ai unui nod de rang r, cu 1 <= i <= r, trebuie
    să aibă exact r-i copii la rândul său, adică r-1, r-2, ..., 1, 0 copii,
    exact în această ordine descrescătoare. Dacă rădăcina are r copii, atunci,
    în conjuncție cu proprietate anterioară, întregul arbore are 2^r noduri.
-}
data BinomialTree p k
    = EmptyTree
    | Node { prio :: p, key :: k, children :: [BinomialTree p k] }
    deriving (Show, Eq)

{-
    Reprezentarea unui heap binomial, ca listă de arbori binomiali ce respectă
    proprietatea de heap. Mai precis, considerăm că, în fiecare arbore binomial,
    rădăcina are cea mai mică prioritate; analog pt subarbori. Câmpul size
    desemnează numărul de elemente din heap (nu numărul de arbori). Mai precis,
    dimensiunea este egală cu suma dimensiunilor arborilor din listă. Cum
    dimensiunea unui arbore de rang r este 2^r, dimensiunea este o sumă
    de puteri ale lui 2, unde exponenții sunt dimensiunile câmpurilor children
    (rangurile) din rădăcinile arborilor nevizi.
-}
data BinomialHeap p k = BinomialHeap { size :: Int, trees :: [BinomialTree p k] }
    deriving (Show, Eq)

{-
    *** TODO ***

    Construiește recursiv un arbore binomial de rang r din doi arbori binomiali
    de rang r-1, atașând unul dintre arbori drept prim copil al rădăcinii
    celuilalt. Maniera în care se realizează atașarea trebuie să țină cont
    de faptul că cei doi arbori respectă proprietatea de heap, și că arborele
    rezultant trebuie de asemenea să o respecte. Astfel, arborele cu cheia mai
    mică din rădăcină trebuie să încorporeze arborele cu cheia mai mare.

    Atenție! Cei doi arbori primiți ca parametru au întotdeauna același rang,
    conform principiului de construcție. Funcția nu necesită parcurgeri
    recursive, putând opera direct la nivelul rădăcinilor.

    Constrângeri: utilizați gărzi.

    Hint: pt pattern matching, pot fi utile alias-urile (@).

    Exemple:

    > attach (Node 0 'a' []) (Node 1 'b' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}

    > attach (Node 1 'b' []) (Node 0 'a' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}
-}

attach :: Ord p => BinomialTree p k -> BinomialTree p k -> BinomialTree p k
attach EmptyTree t = t
attach t EmptyTree = t
attach t1@(Node p1 k1 c1) t2@(Node p2 k2 c2)
  | p1 < p2   = Node p1 k1 (t2 : c1)
  | otherwise = Node p2 k2 (t1 : c2)
{-Funcția attach verifică dacă prioritatea primului arbore (t1) este mai mică decât prioritatea
celui de-al doilea arbore (t2). În acest caz, se creează un nou nod cu prioritatea și cheia primului
arbore și se adaugă al doilea arbore la lista de subarbori ai primului arbore. În caz contrar,
se face invers - se creează un nou nod cu prioritatea și cheia celui de-al doilea arbore și se adaugă
primul arbore la lista de subarbori ai celui de-al doilea arbore.-}

{-
    *** TODO ***

    Introduce un arbore binomial nevid într-o listă cu alți arbori binomiali,
    toți arborii respectând proprietatea de heap. Cum nu pot exista simultan
    în listă doi arbori binomiali cu același rang, la întâlnirea unui arbore
    cu același rang cu cel care se dorește introdus, este necesară atșarea
    unuia la celălalt, cu crearea unui transport.

    Operația o oglindește pe cea de incrementare a unui număr binar din etapa 1.
    Astfel, elementele EmptyTree sunt analoagele biților 0, iar elementele Node,
    biților 1. O diferență este că, în această etapă, biții 1 „nu mai arată toți
    la fel”, ci elementele Node au rangul dat de poziția în listă. Spre exemplu:
    * un element Node de pe poziția 0 din listă trebuie să aibă rangul 0
      (dimensiunea 2^0 = 1)
    * un element Node de pe poziția 1 din listă trebuie să aibă rangul 1
      (dimensiunea 2^1 = 2)
    * un element Node de pe poziția 2 din listă trebuie să aibă rangul 2
      (dimensiunea 2^2 = 4)
    etc.

    Gestiunea transportului apărut în incrementare corespunde operației attach.
    Modul în care va fi utilizată mai departe funcția insertTree garantează
    respectarea presupunerilor funcției attach, cum că arborii primiți ca
    parametru au întotdeauna același rang.

    Constrângeri: utilizați
    * construcția case
    * funcția attach.

    Exemple:

    > insertTree (Node 1 'a' []) []
    [Node {prio = 1, key = 'a', children = []}]

    > insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > insertTree (Node 3 'c' []) $ insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

rank :: BinomialTree p k -> Int
rank EmptyTree = 0
rank (Node _ _ children) = length children

insertTree :: Ord p => BinomialTree p k -> [BinomialTree p k] -> [BinomialTree p k]
insertTree tree [] = [tree]
insertTree tree (t:ts) = case t of
  EmptyTree -> tree : ts
  _ -> if rank tree > rank t then t : insertTree tree ts
       else EmptyTree : insertTree (attach tree t) ts

{-rank: Această funcție primește un arbore binomial (BinomialTree) și calculează rangul său,
adică numărul de subarbori ai săi. Dacă arborele este vid (EmptyTree), atunci rangul său este 0.

Funcția insertTree funcționează astfel: dacă lista de arbori este vidă, atunci se returnează lista
cu arborele primit ca unic element. Dacă lista nu este vidă, atunci se compară rangul arborelui primit
cu rangul primului arbore din listă. Dacă rangul arborelui primit este mai mare decât rangul primului
arbore din listă, atunci arborele este adăugat înaintea primului arbore din listă (deoarece arborii cu
rang mai mic au prioritate mai mare). În caz contrar, se creează un nou arbore prin atașarea celor doi
arbori și se inserează acest nou arbore în restul listei (ts) prin apelul recursiv al funcției insertTree.-}

{-
    *** TODO ***

    Heap-ul vid.
-}
emptyHeap :: BinomialHeap p k
emptyHeap = BinomialHeap { size = 0, trees = [] }

{-
    *** TODO ***

    Introduce o cheie cu prioritatea aferentă într-un heap binomial.

    Constrângeri: utilizați funcția insertTree.

    Exemple:

    > insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 1
        , trees = [Node {prio = 1, key = 'a', children = []}]
        }

    > insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 3
        , trees = [ Node {prio = 3, key = 'c', children = []}
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }
-}

insert :: Ord p => p -> k -> BinomialHeap p k -> BinomialHeap p k
insert p k heap = heap { size = size heap + 1, trees = insertTree (Node p k []) (trees heap) }

{- funcția construiește un nou arbore binomial cu prioritatea și cheia primite, dar cu o listă de
subarbori vidă. Acest arbore este apoi inserat în lista de arbori a heap-ului primit prin apelul funcției insertTree-}

{-
    *** TODO ***

    Dacă heap-ul nu este vid, întoarce perechea formată din prioritatea minimă
    și cheia aferentă; în caz contrar, întoarce Nothing. Cum toți arborii din
    listă respectă proprietatea de heap, este suficient să parcurgeți doar
    rădăcinile pt a o determina pe cea cu prioritate minimă, fără a fi necesară
    explorarea nivelurilor inferioare ale arborilor.

    Constrângeri: pt selectarea arborilor nevizi din listă (ignorând elementele
    EmptyTree), utilizați list comprehension cu pattern matching.

    Hint: pt determinarea elementului minim dintr-o listă pe baza valorii
    calculate de o funcție numită criteriu, utilizați o expresie de forma:
    minimumBy (compare `on` criteriu) lista.

    Exemple:

    > findMin emptyHeap
    Nothing

    > findMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    Just (1,'a')
-}

findMin :: Ord p => BinomialHeap p k -> Maybe (p, k)
findMin (BinomialHeap _ []) = Nothing
findMin (BinomialHeap _ trees) =
  let nonEmptyTrees = [t | t@(Node _ _ _) <- trees]
      (Node p k _) = minimumBy (compare `on` prio) nonEmptyTrees
  in Just (p, k)

{-cazul când heap-ul este vid (nu conține niciun arbore binomial), în care funcția returnează valoarea Nothing
cazul când heap-ul conține cel puțin un arbore binomial. Variabila trees va conține lista de arbori binomiali din heap.
filtrează doar arborii binomiali ne-vidați din lista de arbori din heap. Se folosește o list comprehension pentru a face
acest lucru, iar variabila nonEmptyTrees va fi o listă cu acești arbori
găsește arborele binomial cu cea mai mică prioritate din lista nonEmptyTrees
compare on prio, care compară prioritatea a doi arbori binomiali și returnează ordinea lor. Funcția on este o funcție din
biblioteca standard Haskell, ce permite compunerea a două funcții, în cazul de față compare și prio
returnează perechea de tipul (p, k) cu cea mai mică prioritate găsită în arborele binomial cu ajutorul funcției minimumBy-}

{-
    Funcția zipExtend este similară funcției predefinite zip. Scopul ei este
    de a compensa limitarea funcției zip, care se oprește când atinge sfârșitul
    listei mai scurte. Primii doi parametri reprezintă valori cu care se extind
    prima, respectiv a doua listă, în funcție de care listă este mai scurtă.
    O puteți folosi în cele ce urmează.

    Exemple:

    > zipExtend 0 'z' [1,2] "abcd"
    [(1,'a'),(2,'b'),(0,'c'),(0,'d')]

    > zipExtend 0 'z' [1,2,3,4] "ab"
    [(1,'a'),(2,'b'),(3,'z'),(4,'z')]
-}
zipExtend :: a -> b -> [a] -> [b] -> [(a, b)]
zipExtend a' _  [] bs = zip (repeat a') bs
zipExtend _  b' as [] = zip as (repeat b')
zipExtend a' b' (a : as) (b : bs) = (a, b) : zipExtend a' b' as bs

{-
    *** TODO ***

    Combină două liste de arbori binomiali care respectă proprietatea de heap.
    Observațiile din comentariile funcției insertTree, legate de necesitatea
    atașării arborilor cu același rang, rămân valabile.

    Operația o oglindește pe cea de adunare a două numere binare din etapa 1.

    Constrângeri:
    * evitați recursivitatea explicită
    * utilizați funcția zipExtend pt a facilita aplicarea unor funcționale.

    Exemple:

    > mergeTrees [Node 1 'a' []] []
    [Node {prio = 1, key = 'a', children = []}]

    > mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > mergeTrees [Node 3 'c' []] $ mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
mergeTrees trees1 trees2 = 
  let combinedTrees = zipExtend EmptyTree EmptyTree trees1 trees2
      f (t1, t2) =
        if abs (rank t1 - rank t2) > 1
        then replicate (abs (rank t1 - rank t2) - 1) EmptyTree ++ [t1, t2]
        else [t1, t2]
  in concatMap f combinedTrees
{-Mai întâi, funcția creează o listă de perechi de arbori, fiecare din lista inițială fiind extinsă cu EmptyTree-uri
până când lungimea lor este egală. Această listă de perechi este apoi mapată printr-o funcție f care primește două arbori
și decide cum trebuie să fie îmbinate în funcție de rangul lor. Dacă diferența de rang este mai mare de 1,
atunci se inserează suficiente EmptyTree-uri în lista de arbori pentru a îndepărta această diferență și apoi se adaugă cei doi arbori.
În caz contrar, cei doi arbori sunt adăugați direct la lista de arbori rezultată.-}
{-
    *** TODO ***

    Combină două heap-uri binomiale.

    Constrângeri: utilizați funcția mergeTrees.

    Exemple: similare cu cele de la mergeTrees.
-}

merge :: (Ord p) => BinomialHeap p k -> BinomialHeap p k -> BinomialHeap p k
merge heap1 heap2 = BinomialHeap (size heap1 + size heap2) (mergeTrees (trees heap1) (trees heap2))
{-Mai întâi, dimensiunea celor două heap-uri este adunată pentru a obține dimensiunea heap-ului rezultat.
Apoi, lista de arbori a fiecărui heap este combinată folosind funcția mergeTrees descrisă anterior.-}