{- ********************
   COM2001 Assignment 1
   Bags
   ******************** -}

module Bags where
  import Data.List

  type Item a = (a,Int)
  -- data Item = (a,Int) deriving Eq (Eq,Int)
  type Bag a = [Item a]

  -- takes a list of items in sequence and puts them into a list of tuples in sequence
  listToBag :: (Ord a) => [a] -> Bag a
  listToBag itemList = listToBagA itemList []

  listToBagA :: (Ord a) => [a] -> Bag a -> Bag a
  listToBagA [] _ = []
  listToBagA itemList workingBag
    | null remainingList = bagInsert currentItem workingBag
    | otherwise = listToBagA remainingList (bagInsert currentItem workingBag)
    where (currentItem:remainingList) = itemList

  itemCount :: (Ord a) => a -> Bag a -> Int
  itemCount itemType bag
    -- if the current item in list is of the type being queried, return its quantity
    | itemType == currentItemType = currentItemQuantity
    -- current item in list is not of type being queried and end of list has been reached so return 0
    | null remainingBag = 0
    | otherwise = itemCount itemType remainingBag
    where ((currentItemType,currentItemQuantity):remainingBag) = bag

  bagInsert :: (Ord a) => a -> Bag a-> Bag a
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

  bagInsertN :: (Ord a) => a -> Int -> Bag a -> Bag a
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
  bagEqual :: (Ord a) => Bag a -> Bag a -> Bool
  -- two empty bags equal, but if just one of either is then they cannot be
  bagEqual [] [] = True
  bagEqual _ [] = False
  bagEqual [] _ = False
  -- call bagEqualA on the sorted bags
  bagEqual bag1 bag2 = bagEqualA (sort bag1) (sort bag2)

  bagEqualA :: (Ord a) => Bag a -> Bag a -> Bool
  bagEqualA [] [] = True
  bagEqualA _ [] = False
  bagEqualA [] _ = False

  bagEqualA (h1:t1) (h2:t2)
  -- heads are equal, check rest of bag is
    | h1 == h2 = bagEqualA t1 t2
  -- return false if any aren't
    | otherwise = False

  bagIntersection :: (Ord a) => Bag a -> Bag a -> Bag a
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

  bagSum :: (Ord a) => Bag a -> Bag a -> Bag a
  bagSum bag1 bag2 = bagSumA bag1 bag2 []

  bagSumA :: (Ord a) => Bag a -> Bag a -> Bag a -> Bag a
  bagSumA bag1 bag2 workingBag
    | null bag1 && null bag2 = []
    | null bag1 = bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumIndividual remainingBag2 workingBag)
    | null bag2 = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagSumIndividual remainingBag1 workingBag)
    | null remainingBag1 = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumIndividual remainingBag2 workingBag))
    | null remainingBag2 = bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagSumIndividual remainingBag1 workingBag))
    | otherwise = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumA remainingBag1 remainingBag2 workingBag))
    where ((bag1CurrentItemType,bag1CurrentItemQuantity):remainingBag1) = bag1
          ((bag2CurrentItemType,bag2CurrentItemQuantity):remainingBag2) = bag2

  bagSumIndividual :: (Ord a) => Bag a -> Bag a -> Bag a
  bagSumIndividual bag workingBag
    | null bag = []
    | otherwise = bagInsertN currentItemType currentItemQuantity workingBag
    where (currentItemType,currentItemQuantity):remainingBag = bag
