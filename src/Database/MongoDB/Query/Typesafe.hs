{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Database.MongoDB.Query.Typesafe where

import           Data.Bson
import           Data.Word

import qualified Database.MongoDB.Query as DB
import           TSQuery.Query

data Op = Eq | Neq | Gt | Lt | In deriving (Show, Eq)

type QueryExp = QueryExpOp Op

data QueryTs a = QueryTs
  { options :: [DB.QueryOption]  -- ^ Default = []
  , selection :: QueryExp a
  , skip :: Word32  -- ^ Number of initial matching documents to skip. Default = 0
  , limit :: DB.Limit -- ^ Maximum number of documents to return, 0 = no limit. Default = 0
  , sort :: DB.Order  -- ^ Sort results by this order, [] = no sort. Default = []
  , snapshot :: Bool  -- ^ If true assures no duplicates are returned, or objects missed, which were present at both the start and end of the query's execution (even if the object were updated). If an object is new during the query, or deleted during the query, it may or may not be returned, even with snapshot mode. Note that short query responses (less than 1MB) are always effectively snapshotted. Default = False
  , batchSize :: DB.BatchSize  -- ^ The number of document to return in each batch response from the server. 0 means use Mongo default. Default = 0
  , hint :: DB.Order  -- ^ Force MongoDB to use this index, [] = no hint. Default = []
  }

eq :: (Val b, Eq b) => Entity a b -> b -> QueryExp a
eq = QBin Eq

neq :: (Val b, Eq b) => Entity a b -> b -> QueryExp a
neq = QBin Neq

gt :: (Val b, Ord b) => Entity a b -> b -> QueryExp a
gt = QBin Gt

lt :: (Val b, Ord b) => Entity a b -> b -> QueryExp a
lt = QBin Lt

cnt :: (Val b, Eq b) => Entity a b -> b -> QueryExp a
cnt = QBin In

query :: DB.Selector -> DB.Collection -> DB.Query
query sel col = DB.Query [] (DB.Select sel col) [] 0 0 [] False 0 []

tsQueryToSelector :: QueryExp a -> DB.Selector
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

--

-- find :: (DB.MonadIO m, DB.MonadBaseControl IO m) => TSQuery -> DB.Action m DB.Cursor
find tsq = DB.find (query (tsQueryToSelector tsq) "")
