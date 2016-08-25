{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Mapper.Indexes where

import Control.Lens (view, (?~))

import Data.Function (on)
import Data.List     (nubBy)

import Data.Function ((&))
import Data.Proxy     (Proxy (..))
import Data.Semigroup ((<>))

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper.Attributes
import Network.AWS.DynamoDB.Mapper.Keys
import Network.AWS.DynamoDB.Mapper.Types

-- | A type-level wrapper for passing the table's attribute schema
-- through type class instance heads.
data Schema s a

class DynamoIndexes a where
    getGlobalIndexes :: Proxy a -> [GlobalSecondaryIndex]
    getLocalIndexes  :: Proxy a -> [LocalSecondaryIndex]

    getGlobalIndexes = const []
    getLocalIndexes  = const []

instance ( DynamoIndexes (Schema s o)
         ) => DynamoIndexes (Table n s o) where
    getGlobalIndexes _ =
        uniqueGlobalIndexes $
            getGlobalIndexes (Proxy :: Proxy (Schema s o))

    getLocalIndexes  _ =
        uniqueLocalIndexes  $
            getLocalIndexes (Proxy :: Proxy (Schema s o))

instance ( DynamoIndexes (Schema s a)
         , DynamoIndexes (Schema s b)
         ) => DynamoIndexes (Schema s (a :# b)) where
    getGlobalIndexes _ =
           getGlobalIndexes (Proxy :: Proxy (Schema s a))
        <> getGlobalIndexes (Proxy :: Proxy (Schema s b))

    getLocalIndexes  _ =
           getLocalIndexes  (Proxy :: Proxy (Schema s a))
        <> getLocalIndexes  (Proxy :: Proxy (Schema s b))

instance ( HasAttributes s ks
         , DynamoKeys      ks
         , KnownSymbol   n
         ) => DynamoIndexes (Schema s (GlobalIndex n ks o)) where
    getGlobalIndexes _ = pure $
        globalSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy ks))
            (projection & pProjectionType ?~ All)
            undefined -- (getProvisionedThroughput (Proxy :: Proxy r) (Proxy :: Proxy w))

instance ( HasAttributes s ks
         , DynamoKeys      ks
         , KnownSymbol   n
         ) => DynamoIndexes (Schema s (LocalIndex n ks)) where
    getLocalIndexes _ = pure $
        localSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy ks))
            (projection & pProjectionType ?~ All)

instance DynamoIndexes (Key        n k)
instance DynamoIndexes (Throughput r w)
instance DynamoIndexes (Stream     v)
instance DynamoIndexes (Project    p)

-- FIXME: Migrate this to a type family.
uniqueGlobalIndexes :: [GlobalSecondaryIndex] -> [GlobalSecondaryIndex]
uniqueGlobalIndexes = nubBy (on (==) (view gsiIndexName))

-- FIXME: Migrate this to a type family.
uniqueLocalIndexes :: [LocalSecondaryIndex] -> [LocalSecondaryIndex]
uniqueLocalIndexes = nubBy (on (==) (view lsiIndexName))
