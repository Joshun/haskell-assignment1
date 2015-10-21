{- ********************
   COM2001 Assignment 1
   Bags
   ******************** -}

module Bags where
  import Data.List

  -- polymorphic type for single item in bag
  type Item a = (a,Int)
  -- bag of polymorphic item types
  type Bag a = [Item a]

  -- takes a list of items in sequence and puts them into a list of tuples in sequence
  listToBag :: (Eq a) => [a] -> Bag a
  listToBag itemList = listToBagA itemList []

  listToBagA :: (Eq a) => [a] -> Bag a -> Bag a
  listToBagA [] _ = []
  listToBagA itemList workingBag
    -- if there is none left in remainingList, insert currentItem and finish
    | null remainingList = bagInsert currentItem workingBag
    -- otherwise keep adding items from the list to the bag
    | otherwise = listToBagA remainingList (bagInsert currentItem workingBag)
    where (currentItem:remainingList) = itemList

  -- returns the quantity of a given item in a bag
  itemCount :: (Eq a) => a -> Bag a -> Int
  itemCount itemType bag
    -- if the current item in list is of the type being queried, return its quantity
    | itemType == currentItemType = currentItemQuantity
    -- current item in list is not of type being queried and end of list has been reached so return 0
    | null remainingBag = 0
    | otherwise = itemCount itemType remainingBag
    where ((currentItemType,currentItemQuantity):remainingBag) = bag

  bagInsert :: (Eq a) => a -> Bag a-> Bag a
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

  bagInsertN :: (Eq a) => a -> Int -> Bag a -> Bag a
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
  bagEqual :: (Eq a) => Bag a -> Bag a -> Bool
  -- two empty bags equal, but if just one of either is then they cannot be
  bagEqual [] [] = True
  bagEqual _ [] = False
  bagEqual [] _ = False
  -- call bagEqualA on the sorted bags
  bagEqual bag1 bag2
    -- only one element left in both so if bags are equal heads must be equal
    | null remainingBag1 && null remainingBag2 = bag1Element == bag2Element
    -- otherwise see if current element of bag1 exists in bag2 and if so remove from bag2 and compare rest
    | bag1Element `elem` bag2 = bagEqual remainingBag1 (bagRemove bag1Element bag2)
    -- otherwise check rest of bag1 and bag2
    | otherwise = bagEqual remainingBag1 bag2
    where (bag1Element:remainingBag1) = bag1
          (bag2Element:remainingBag2) = bag2

  -- helper function to return a bag with an element removed
  bagRemove :: (Eq a) => Item a -> Bag a -> Bag a
  bagRemove item bag
    | null bag = []
    | bagElement == item = remainingBag
    | otherwise = bagElement:(bagRemove item remainingBag)
    where (bagElement:remainingBag) = bag

  bagIntersection :: (Eq a) => Bag a -> Bag a -> Bag a
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

  bagSum :: (Eq a) => Bag a -> Bag a -> Bag a
  bagSum bag1 bag2 = bagSumA bag1 bag2 []

  bagSumA :: (Eq a) => Bag a -> Bag a -> Bag a -> Bag a
  bagSumA bag1 bag2 workingBag
    -- if both bags are empty then sum is empty list
    | null bag1 && null bag2 = []
    -- if bag1 is empty then add the rest of bag2
    | null bag1 = bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumIndividual remainingBag2 workingBag)
    -- if bag2 is empty then add the rest of bag1
    | null bag2 = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagSumIndividual remainingBag1 workingBag)
    -- if there is no more of bag1 remaining, add its head and the rest of bag2
    | null remainingBag1 = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumIndividual remainingBag2 workingBag))
    -- if there is no more of bag2 remaining, add its head and the rest of bag1
    | null remainingBag2 = bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagSumIndividual remainingBag1 workingBag))
    -- otherwise, keep the contents of both bag1 and bag2
    | otherwise = bagInsertN bag1CurrentItemType bag1CurrentItemQuantity (bagInsertN bag2CurrentItemType bag2CurrentItemQuantity (bagSumA remainingBag1 remainingBag2 workingBag))
    where ((bag1CurrentItemType,bag1CurrentItemQuantity):remainingBag1) = bag1
          ((bag2CurrentItemType,bag2CurrentItemQuantity):remainingBag2) = bag2

  -- helper function for adding the rest of a bag when the other bag is empty
  bagSumIndividual :: (Eq a) => Bag a -> Bag a -> Bag a
  bagSumIndividual bag workingBag
    | null bag = []
    | otherwise = bagInsertN currentItemType currentItemQuantity workingBag
    where (currentItemType,currentItemQuantity):remainingBag = bag
