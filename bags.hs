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

  bagInsertN :: ItemType -> Int -> Bag Int -> Bag Int
  bagInsertN itemType n [] = (itemType,n):[]
  bagInsertN itemType n bag
    -- if item doesn't exist, create new pair which counts 1 item of the type
    | null remainingBag =
        if itemType == currentItemType
          then (currentItemType,currentItemQuantity+n):[]
        else (currentItemType,currentItemQuantity):(itemType,n):[]
    -- if item is already in list, create tuple with item type and count increased by 1 and cons it
    | itemType == currentItemType = (currentItemType,currentItemQuantity+n):(remainingBag)
    -- else keep going through bag seeing if item already exists
    | otherwise = ((currentItemType,currentItemQuantity):(bagInsertN itemType n remainingBag))
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
  -- heads are equal, check rest of bag is
    | h1 == h2 = bagEqualA t1 t2
  -- return false if any aren't
    | otherwise = False

  bagIntersection :: Bag Int -> Bag Int -> Bag Int
  bagIntersection bag1 bag2
    | null bag1 || null bag2 = []
    -- checks if the item being looked at in bag 1 is present in bag 2
    | bag2ItemCount > 0 =
    -- checks whether the item occurs more times in bag1 or bag2
      if (bag1ItemCount) <= bag2ItemCount
        then (bag1ItemType,bag1ItemCount):(bagIntersection remainingBag1 bag2)
      else
        (bag1ItemType,bag2ItemCount):(bagIntersection remainingBag1 bag2)
    -- if item isn't present in bag2 then check rest of items in bag1
    | otherwise = bagIntersection remainingBag1 bag2
    -- checks if the item being looked at in bag 1 is present in bag 2
    | bag2ItemCount > 0 =
    -- checks whether the item occurs more times in bag1 or bag2
      if (bag1ItemCount) <= bag2ItemCount
        then (bag1ItemType,bag1ItemCount):(bagIntersection remainingBag1 bag2)
      else
        (bag1ItemType,bag2ItemCount):(bagIntersection remainingBag1 bag2)
    -- if item isn't present in bag2 then check rest of items in bag1
    | otherwise = bagIntersection remainingBag1 bag2
    where ((bag1ItemType,bag1ItemCount):remainingBag1) = bag1
    -- here, bag2ItemCount is the number of times the current item being looked at in bag 1, bag1ItemType, occurs in bag 2
          bag2ItemCount = itemCount bag1ItemType bag2

  bagSum :: Bag Int -> Bag Int -> Bag Int
  bagSum bag1 bag2 = bagSumA bag1 bag2 []

  bagSumA :: Bag Int -> Bag Int -> Bag Int -> Bag Int
  bagSumA bag1 bag2 workingBag
    | null bag1 && null bag2 = []
    | null bag1 = bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumIndividual remainingBag2 workingBag)
    | null bag2 = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagSumIndividual remainingBag1 workingBag)
    | null remainingBag1 = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumIndividual remainingBag2 workingBag))
    | null remainingBag2 = bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagSumIndividual remainingBag1 workingBag))
    | otherwise = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumA remainingBag1 remainingBag2 workingBag))
    where ((bag1CurrentItemType,bag1CurrentItemQuantity):remainingBag1) = bag1
          ((bag2CurrentItemType,bag2CurrentItemQuantity):remainingBag2) = bag2

  bagSumIndividual :: Bag Int -> Bag Int -> Bag Int
  bagSumIndividual bag workingBag
    | null bag = []
    | otherwise = bagInsertN currentItemType currentItemQuantity workingBag
    where (currentItemType,currentItemQuantity):remainingBag = bag
