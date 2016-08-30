{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Schema.Index where

import Control.Lens (view, (?~), (^.))

import Data.Text (Text)
import Data.Foldable (toList, find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Function      ((&))
import Data.List          ((\\), nub)
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))

import GHC.TypeLits

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import Network.AWS.DynamoDB.Schema.Key
import Network.AWS.DynamoDB.Schema.Throughput
import Network.AWS.DynamoDB.Schema.Types
import Network.AWS.DynamoDB.Schema.Invariant

import qualified Network.AWS.DynamoDB as Dynamo

class DynamoIndexes a where
    getGlobalIndexes :: Proxy a -> [Dynamo.GlobalSecondaryIndex]
    getLocalIndexes  :: Proxy a -> [Dynamo.LocalSecondaryIndex]

    getGlobalIndexes = const []
    getLocalIndexes  = const []

instance ( DynamoIndexes (Schema a i)
         ) => DynamoIndexes (Table n a t s i) where
    getGlobalIndexes _ = getGlobalIndexes (Proxy :: Proxy (Schema a i))
    getLocalIndexes  _ = getLocalIndexes  (Proxy :: Proxy (Schema a i))

instance DynamoIndexes (Schema s '[])

instance ( DynamoIndexes (Schema s i)
         , DynamoIndexes (Schema s is)
         ) => DynamoIndexes (Schema s (i ': is)) where
    getGlobalIndexes _ =
           getGlobalIndexes (Proxy :: Proxy (Schema s i))
        <> getGlobalIndexes (Proxy :: Proxy (Schema s is))

    getLocalIndexes  _ =
           getLocalIndexes  (Proxy :: Proxy (Schema s i))
        <> getLocalIndexes  (Proxy :: Proxy (Schema s is))

instance ( UniqueAttributes    a
         , HasAttributes     s a
         , PartitionKeyOrder   a
         , DynamoKeys          a
         , DynamoThroughput  t
         , KnownSymbol       n
         , KnownSymbols        a
         ) => DynamoIndexes (Schema s (GlobalSecondaryIndex n a t)) where
    getGlobalIndexes _ =
        pure $ globalSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy a))
            (project (nonKeyAttributes (Proxy :: Proxy a)))
            (getThroughput (Proxy :: Proxy t))

instance ( UniqueAttributes    a
         , HasAttributes     s a
         , SortKeyOrder        a
         , DynamoKeys        s
         , DynamoKeys          a
         , KnownSymbols        a
         , KnownSymbol       n
         ) => DynamoIndexes (Schema s (LocalSecondaryIndex n a)) where
    getLocalIndexes _ =
        pure $ localSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy a))
            indexProjection
      where
        -- Project 's' SortKey as a non-key attribute, if it exists.
        schemaKeys = getKeys (Proxy :: Proxy s)
        sortKey    =
            view kseAttributeName
                <$> find ((== Range) . view kseKeyType) schemaKeys

        indexProjection =
            project . nub . maybe id (:) sortKey $
                nonKeyAttributes (Proxy :: Proxy a)

project :: [Text] -> Projection
project []     =
    projection & pProjectionType ?~ KeysOnly
project (x:xs) =
    projection & pProjectionType ?~ Include
        & pNonKeyAttributes ?~ x :| xs

nonKeyAttributes :: forall a. (DynamoKeys a, KnownSymbols a)
                 => Proxy a
                 -> [Text]
nonKeyAttributes _ =
    let attrs = map (^. kseAttributeName) (toList (getKeys (Proxy :: Proxy a)))
        keys  = symbolTexts (Proxy :: Proxy a)
     in attrs \\ keys
