{- ********************
   COM2001 Assignment 1
   Bags
   ******************** -}

module Bags where
  import Data.List

  type ItemType = Int
  type Item a = (a,Int)
  -- data Item = (a,Int) deriving Eq (Eq,Int)
  type Bag a = [Item a]

  -- takes a list of items in sequence and puts them into a list of tuples in sequence
  listToBag :: [ItemType] -> Bag Int
  listToBag itemList = listToBagCleanup (listToBagA itemList)

  listToBagA :: [ItemType] -> Bag Int
  listToBagA [] = []
  listToBagA (first:rest) = ((first,1):listToBagA rest)

  -- goes through list of tuples and puts duplicates togther - i.e. (5,1),(5,1),(3,1)(5,1) becomes (5,3),(3,1)
  listToBagCleanup :: Bag Int -> Bag Int
  listToBagCleanup [] = []
  -- call listToBagCleanupA with the bag to clean up and an empty 'working' bag
  listToBagCleanup bag = listToBagCleanupA bag []

  -- takes a bag to clean up, a working bag and returns a new bag
  -- the 'working' bag is a new bag, initially empty, used to build up the final bag which will have been stored more efficiently
  listToBagCleanupA bag workingBag
    -- got to end of working bag, so just insert the item into it
    | null remainingBag = bagInsert currentItemType workingBag
    -- otherwise insert item in place
    | otherwise = listToBagCleanupA remainingBag (bagInsert currentItemType workingBag)
    where ((currentItemType,currentItemQuantity):remainingBag) = bag

  itemCount :: ItemType -> Bag Int -> Int
  itemCount itemType bag
    -- if the current item in list is of the type being queried, return its quantity
    | null remainingBag = if itemType == currentItemType then currentItemQuantity else 0
    | itemType == currentItemType = currentItemQuantity + (itemCount itemType remainingBag)
    | otherwise = itemCount itemType remainingBag
    where ((currentItemType,currentItemQuantity):remainingBag) = bag

  bagInsert :: ItemType -> Bag Int-> Bag Int
  bagInsert itemType [] = (itemType,1):[]
  bagInsert itemType bag
    -- if item doesn't exist, create new pair which counts 1 item of the type
    | null remainingBag =
        if itemType == currentItemType
          then (currentItemType,currentItemQuantity+1):[]
        else (currentItemType,currentItemQuantity):(itemType,1):[]
    -- if item is already in list, create tuple with item type and count increased by 1 and cons it
    | itemType == currentItemType = (currentItemType,currentItemQuantity+1):(remainingBag)
    -- else keep going through bag seeing if item already exists
    | otherwise = ((currentItemType,currentItemQuantity):(bagInsert itemType remainingBag))
    where ((currentItemType,currentItemQuantity):remainingBag) = bag

  -- sort both bags and then see if each (item,quantity) tuple is equal
  bagEqual :: Bag Int -> Bag Int -> Bool
  -- two empty bags equal, but if just one of either is then they cannot be
  bagEqual [] [] = True
  bagEqual _ [] = False
  bagEqual [] _ = False
  -- call bagEqualA on the sorted bags
  bagEqual bag1 bag2 = bagEqualA (sort bag1) (sort bag2)

  bagEqualA :: Bag Int -> Bag Int -> Bool
  bagEqualA [] [] = True
  bagEqualA _ [] = False
  bagEqualA [] _ = False

  bagEqualA (h1:t1) (h2:t2)
    | h1 == h2 = bagEqualA t1 t2
    | otherwise = False
