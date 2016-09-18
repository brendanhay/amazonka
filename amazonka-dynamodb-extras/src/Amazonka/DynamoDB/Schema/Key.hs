{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Amazonka.DynamoDB.Schema.Key where

import Amazonka.DynamoDB.Schema.Attribute

import Control.Lens ((^.))

import Data.Functor.Identity
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Maybe
import Data.Proxy            (Proxy (..))
import Data.Semigroup        ((<>))
import Data.Text             (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import qualified Data.List.NonEmpty as NE

type DynamoKeys a = (DynamoPartitionKey a, DynamoSortKey Maybe a)

getKeys :: forall a. DynamoKeys a => Proxy a -> NonEmpty KeySchemaElement
getKeys _ =
       keySchemaElement (getPartitionKey (Proxy :: Proxy a)) Hash
    :| maybeToList (flip keySchemaElement Range <$> getSortKey (Proxy :: Proxy a))

class DynamoPartitionKey a where
    getPartitionKey :: Proxy a -> Text

instance KnownSymbol n => DynamoPartitionKey (PartitionKey n) where
    getPartitionKey _ = symbolToText (Proxy :: Proxy n)

instance KnownSymbol n => DynamoPartitionKey (PartitionKey n :# a) where
    getPartitionKey _ = symbolToText (Proxy :: Proxy n)

class DynamoSortKey f a where
    getSortKey :: Proxy a -> f Text

instance DynamoSortKey Identity a => DynamoSortKey Identity (PartitionKey n :# a) where
    getSortKey _ = getSortKey (Proxy :: Proxy a)

instance KnownSymbol n => DynamoSortKey Identity (SortKey n) where
    getSortKey _ = pure $ symbolToText (Proxy :: Proxy n)

instance KnownSymbol n => DynamoSortKey Identity (SortKey n :# a) where
    getSortKey _ = pure $ symbolToText (Proxy :: Proxy n)

instance {-# OVERLAPPABLE #-} DynamoSortKey Maybe a where
    getSortKey _ = Nothing

instance {-# OVERLAPS #-}
      DynamoSortKey Maybe a => DynamoSortKey Maybe (PartitionKey n :# a) where
    getSortKey _ = getSortKey (Proxy :: Proxy a)

instance {-# OVERLAPS #-}
      KnownSymbol n => DynamoSortKey Maybe (SortKey n) where
    getSortKey _ = pure $ symbolToText (Proxy :: Proxy n)

instance {-# OVERLAPS #-}
      KnownSymbol n => DynamoSortKey Maybe (SortKey n :# a) where
    getSortKey _ = pure $ symbolToText (Proxy :: Proxy n)
