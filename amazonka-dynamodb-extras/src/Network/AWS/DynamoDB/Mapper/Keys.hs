{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Mapper.Keys where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy         (Proxy (..))
import Data.Semigroup      ((<>))

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper.Attributes
import Network.AWS.DynamoDB.Mapper.Types

class DynamoKeyType a where
    getKeyType :: Proxy a -> KeyType

instance DynamoKeyType 'Hash  where getKeyType = const Hash
instance DynamoKeyType 'Range where getKeyType = const Range

class DynamoKeys a where
    getKeys :: Proxy a -> NonEmpty KeySchemaElement

instance (HasAttributes o s, DynamoKeys o) => DynamoKeys (Table n s o) where
    getKeys _ = getKeys (Proxy :: Proxy o)

instance (DynamoKeys a, DynamoKeys b) => DynamoKeys (a :# b) where
    getKeys _ =
           getKeys (Proxy :: Proxy a)
        <> getKeys (Proxy :: Proxy b)

instance (KnownSymbol n, DynamoKeyType k) => DynamoKeys (Key n k) where
    getKeys _ = pure $
        keySchemaElement
            (symbolText (Proxy :: Proxy n))
            (getKeyType (Proxy :: Proxy k))
