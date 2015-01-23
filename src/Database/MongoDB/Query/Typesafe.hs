{-# LANGUAGE OverloadedStrings #-}

module Database.MongoDB.Query.Typesafe where

import           Data.Bson

import qualified Database.MongoDB.Query as DB
import           TSQuery.Query

query :: DB.Selector -> DB.Collection -> DB.Query
query sel col = DB.Query [] (DB.Select sel col) [] 0 0 [] False 0 []

tsQueryToQuery :: Query a -> DB.Query
tsQueryToQuery QAll = query [] ""
tsQueryToQuery (QEq (Entity fld) v) = query [fld := v] ""
