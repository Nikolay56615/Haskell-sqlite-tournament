-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2019-2021 Konstantin Pugachev
--                (c) 2019 Denis Miginsky
-- License     :  MIT
--
-- Maintainer  :  K.V.Pugachev@inp.nsk.su
-- Stability   :  experimental
-- Portability :  portable
--
-- The SQLHSExample module provides examples of using SQLHS/SQLHSSugar module.
--
-----------------------------------------------------------------------------

import SQLHSSugar
import DBReader

-- CATEGORY:     WARE,    CLASS
-- MANUFACTURER: BILL_ID, COMPANY
-- MATERIAL:     BILL_ID, WARE,   AMOUNT
-- PRODUCT:      BILL_ID, WARE,   AMOUNT, PRICE

main = readDB' defaultDBName >>= executeSomeQueries

executeSomeQueries :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
executeSomeQueries (categories, manufacturers, materials, products) = do
  test "lecPlan1'" lecPlan1'
  test "lecPlan2" lecPlan2
  test "lecPlan3" lecPlan3
  test "lecPlan4" lecPlan4
  test "ex1" ex1
  test "ex2" ex2
  test "ex3" ex3
  
  where
    test msg p = do
      putStrLn $ "===== execute " ++ msg ++ " ====="
      -- putStrLn . debugTable $ p & enumerate
      printResult $ p & enumerate
    
    lecPlan1' = 
      -- MANUFACTURER NL_JOIN PRODUCT ON m.BILL_ID=p.BILL_ID
      manufacturers // "m" `njoin` products // "p" `on` "m.BILL_ID" `jeq` "p.BILL_ID"
      -- -> NL_JOIN CATEGORY ON c.WARE=p.WARE
      `njoin` categories // "c" `on` "p.WARE" `jeq` "c.WARE"
      -- -> FILTER c.CLASS='Raw food'
      `wher` col "CLASS" `eq` str "Raw food"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- -> DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10
  
    lecPlan2 = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      categories // "c" `wher` col "c.CLASS" `eq` str "Raw food"
      -- -> NL_JOIN PRODUCT ON c.WARE=p.WARE
      `njoin` products // "p" `on` "c.WARE" `jeq` "p.WARE"
      -- -> NL_JOIN MANUFACTURER ON m.BILL_ID=p.BILL_ID
      `njoin` manufacturers // "m" `on` "p.BILL_ID" `jeq` "m.BILL_ID"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10
  
    lecPlan3 = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      categories // "c" `wher` col "c.CLASS" `eq` str "Raw food"  
      -- -> HASH_JOIN PRODUCT INDEX BY WARE ON c.WARE=p.WARE
      `hjoin` (products // "p" `indexby` col "WARE") `on` col "c.WARE"
      -- -> HASH_JOIN MANUFACTURER INDEX BY BILL_ID ON m.BILL_ID=p.BILL_ID
      `hjoin` (manufacturers // "m" `indexby` col "BILL_ID") `on` col "p.BILL_ID"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10

    lecPlan4 = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      (categories // "c" `indexby` col "WARE" & flatten) `wher` col "CLASS" `eq` str "Raw food"
      -- -> MERGE_JOIN PRODUCT INDEX BY WARE ON c.WARE=p.WARE
      `mjoin` (products // "p" `indexby` col "WARE" & flatten) `on` "c.WARE" `jeq` "p.WARE"
      -- -> HASH_JOIN MANUFACTURER INDEX BY BILL_ID ON m.BILL_ID=p.BILL_ID
      `hjoin` (manufacturers // "m" `indexby` col "BILL_ID") `on` col "p.BILL_ID"
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10
  
    ex1 = 
      ((categories // "c" `indexby` col "WARE" & flatten) `wher` col "CLASS" `eq` str "Mineral"
      `hjoin` (materials // "m" `indexby` col "WARE") `on` col "c.WARE"
      `hjoin` (products // "p" `indexby` col "BILL_ID") `on` col "m.BILL_ID"
      `select` ["p.WARE"]
      & distinct)
      `orderby` ["p.WARE":asc]

    ex2 = 
      (categories // "c" `indexby` col "WARE" & flatten) `wher` col "CLASS" `eq` str "Stuff"
      `hjoin` (products // "p" `indexby` col "WARE") `on` col "c.WARE"
      `hjoin` (materials // "m" `indexby` col "BILL_ID") `on` col "p.BILL_ID"
      `hjoin` (categories // "c1" `indexby` col "WARE") `on` col "m.WARE"
      `wher` col "c1.CLASS" `eq` str "Mineral"
      `select` ["m.BILL_ID", "m.WARE", "p.WARE"]
      `orderby` ["m.BILL_ID":asc]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 50
      -- MANUFACTURER NL_JOIN PRODUCT ON m.BILL_ID=p.BILL_ID
      -- materials // "m" `njoin` products // "p" `on` "m.BILL_ID" `jeq` "p.BILL_ID"
      -- -> NL_JOIN CATEGORY ON c.WARE=p.WARE
      --`njoin` categories // "c" `on` "p.WARE" `jeq` "c.WARE" `wher` "c.CLASS" `eq` str "Stuff"
      -- -> NL_JOIN CATEGORY ON c.WARE=p.WARE
      --`njoin` categories // "c1" `on` "m.WARE" `jeq` "c1.WARE"
      -- -> FILTER c.CLASS='Raw food'
      --`wher` "c1.CLASS" `eq` str "Mineral"
      -- -> MAP (p.WARE, m.COMPANY)
      --`select` ["p.BILL_ID", "m.WARE", "p.WARE"]
      -- -> DISTINCT
      -- distinct 
      -- -> TAKE 10
      -- limit 0 50

    ex3 = 
      ((manufacturers // "ma" `indexby` col "BILL_ID" & flatten)
      `hjoin` (materials // "m1" `indexby` col "BILL_ID") `on` col "ma.BILL_ID"
      `hjoin` (products // "p1" `indexby` col "BILL_ID") `on` col "ma.BILL_ID"
      `hjoin` (manufacturers // "ma1" `indexby` col "COMPANY") `on` col "ma.COMPANY"
      `hjoin` (materials // "m2" `indexby` col "BILL_ID") `on` col "ma1.BILL_ID"
      `wher` col "m2.WARE" `eq` col "p1.WARE"
      `hjoin` (products // "p2" `indexby` col "BILL_ID") `on` col "ma1.BILL_ID"
      `select` ["ma.COMPANY"]
      `orderby` ["COMPANY":asc]
      & distinct)
