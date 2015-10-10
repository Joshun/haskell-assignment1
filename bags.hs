{- ********************
   COM2001 Assignment 1
   Bags
   ******************** -}

module Bags where
  type ItemType = Int
  type Item a = (a,Int)
  -- data Item = (a,Int) deriving Eq (Eq,Int)
  type Bag a = [Item a]

  listToBag :: [ItemType] -> Bag Int
  listToBag [] = []
  listToBag (first:rest) = (first,1):listToBag rest

  listToBagCleanup :: Bag Int -> Bag Int
  listToBagCleanup [] = []
  listToBagCleanup bag = listToBagCleanupA bag []

  -- takes a bag to clean up, a working bag and returns a new bag
  listToBagCleanupA :: Bag Int -> Bag Int -> Bag Int
  listToBagCleanupA bag workingBag
    | null rest = bagInsert nextItemType workingBag
    | otherwise = listToBagCleanupA rest (bagInsert nextItemType workingBag)
    where ((nextItemType,nextItemCount):rest) = bag

  itemCount :: ItemType -> Bag Int -> Int
  itemCount itemType bag
    | null rest = if itemType == nextItemType then nextItemCount else 0
    | itemType == nextItemType = nextItemCount + (itemCount itemType rest)
    | otherwise = itemCount itemType rest
    where ((nextItemType,nextItemCount):rest) = bag

  bagInsert :: ItemType -> Bag Int-> Bag Int
  bagInsert itemType [] = (itemType,1):[]
  bagInsert itemType bag
    -- if item doesn't exist, create new pair which counts 1 item of the type
    | null remainingBag = if itemType == nextItemType then (nextItemType,nextItemCount+1):[] else (nextItemType,nextItemCount):(itemType,1):[]
    -- if item is already in list, create tuple with item type and count increased by 1 and cons it
    | itemType == nextItemType = (nextItemType,nextItemCount+1):(remainingBag)
    -- else keep going through bag seeing if item already exists
    | otherwise = ((nextItemType,nextItemCount):(bagInsert itemType remainingBag))
    where ((nextItemType,nextItemCount):remainingBag) = bag
