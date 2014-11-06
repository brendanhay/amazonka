{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Module      : Network.AWS.Data.Internal.Map
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Map
    ( Map
    , _Map
    , nullMap
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.CaseInsensitive                 as CI
import           Data.Foldable                        (Foldable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Hashable                        (Hashable)
import           Data.Monoid
import           Data.Semigroup                       (Semigroup)
import           Data.Tagged
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Header
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML

newtype Map k v = Map { toHashMap :: HashMap k v }
    deriving
        ( Eq
        , Show
        , Read
        , Functor
        , Foldable
        , Traversable
        , Monoid
        , Semigroup
        )

_Map :: Iso' (Map k v) (HashMap k v)
_Map = iso toHashMap Map

nullMap :: Map k v -> Bool
nullMap = Map.null . toHashMap

instance (ToByteString k, ToByteString v) => ToHeader (Map k v) where
    toHeader p = map (bimap (CI.mk . mappend p . toBS) toBS)
        . Map.toList
        . toHashMap

instance (ToByteString k, ToQuery v) => ToQuery (Map k v) where
    toQuery = toQuery . map (toQuery . first toBS) . Map.toList . toHashMap

instance (Eq k, Hashable k, FromText k, FromJSON v) => FromJSON (Map k v) where
    parseJSON = withObject "HashMap" f
      where
        f = fmap (Map . Map.fromList) . mapM g . Map.toList
        g (k, v) = (,)
            <$> either fail return (fromText k)
            <*> parseJSON v

instance (ToText k, ToJSON v) => ToJSON (Map k v) where
    toJSON = Object
        . Map.fromList
        . map (bimap toText toJSON)
        . Map.toList
        . toHashMap

instance (Eq k, Hashable k, FromText k, FromXML v) => FromXML (Map k v) where
    fromXMLRoot = fromRoot "Map"
    fromXML o   = fmap Map . fromXML (retag o)
