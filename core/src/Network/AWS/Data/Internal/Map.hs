{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE TupleSections              #-}

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
    ( Map (..)
    , _Map
    , (~::)
    ) where

import           Control.Applicative
import           Control.Lens                         hiding (coerce)
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.CaseInsensitive                 as CI
import           Data.Coerce
import           Data.Foldable                        (Foldable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Hashable                        (Hashable)
import           Data.Monoid
import           Data.Semigroup                       (Semigroup)
import           Data.Tagged
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Header
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML
import           Network.HTTP.Types.Header            (Header)

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

type role Map nominal representational

_Map :: (Coercible a b, Coercible b a) => Iso' (Map k a) (HashMap k b)
_Map = iso (coerce . toHashMap) (Map . coerce)

(~::) :: FromText v => [Header] -> Text -> Either String (Map Text v)
(~::) hs p = Map
    . Map.filterWithKey (const . Text.isPrefixOf p)
    . Map.fromList <$> mapM f hs
  where
    f (k, v) = (Text.decodeUtf8 (CI.foldedCase k),)
        <$> fromText (Text.decodeUtf8 v)

instance (ToByteString k, ToByteString v) => ToHeader (Map k v) where
    toHeader k = map (bimap (mappend k . CI.mk . toBS) toBS)
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

-- instance (Eq k, Hashable k, FromText k, FromXML v) => FromXML (Map k v) where
--     parseXML = fmap Map . parseXML
