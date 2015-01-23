{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Database.MongoDB.Query.Typesafe where

import           Data.Bson

import qualified Database.MongoDB.Query as DB
import           TSQuery.Query

data Op = Eq | Neq | Gt | Lt | In

type TSQuery = Query Op

eq :: (Val b, Eq b) => Entity a b -> b -> TSQuery a
eq = QBin Eq

neq :: (Val b, Eq b) => Entity a b -> b -> TSQuery a
neq = QBin Neq

gt :: (Val b, Ord b) => Entity a b -> b -> TSQuery a
gt = QBin Gt

lt :: (Val b, Ord b) => Entity a b -> b -> TSQuery a
lt = QBin Lt

cnt :: (Val b, Eq b) => Entity a b -> b -> TSQuery a
cnt = QBin In

query :: DB.Selector -> DB.Collection -> DB.Query
query sel col = DB.Query [] (DB.Select sel col) [] 0 0 [] False 0 []

tsQueryToSelector :: TSQuery a -> DB.Selector
tsQueryToSelector QAll                     = []
tsQueryToSelector (QBin Eq (Entity fld) v) = [ fld =: v ]
tsQueryToSelector (QBin op (Entity fld) v) = [ fld =: [ (opText op) =: v ] ]
  where
    opText Eq = "$eq"
    opText Neq = "$ne"
    opText Gt = "$gt"
    opText Lt = "$lt"
    opText In = "$in"
tsQueryToSelector (QOr lhs rhs)  = [ "$or" =: [ tsQueryToSelector lhs, tsQueryToSelector rhs ] ]
tsQueryToSelector (QAnd lhs rhs) = [ "$and" =: [ tsQueryToSelector lhs, tsQueryToSelector rhs ] ]
tsQueryToSelector (QNot q)       = [ "$not" =: tsQueryToSelector q ]
