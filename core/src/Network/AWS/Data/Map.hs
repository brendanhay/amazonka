{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Data.Map
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Map
    ( Map (..)
    , _Map
    , parseXMLMap
    , toQueryMap
    ) where

import Data.Maybe
import Data.ByteString (ByteString)
import Data.Bifunctor
import           Data.HashMap.Strict (HashMap)
import qualified           Data.HashMap.Strict as Map
import Data.Semigroup
import           Control.Lens         (Iso', iso)
import           Control.Monad
import           Data.Aeson
import Network.AWS.Data.Query
import Network.AWS.Data.Text
import           Data.Coerce
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Text            (Text)
import           GHC.Exts
import           Network.AWS.Data.XML
import           Text.XML             (Node)
import Data.Hashable

newtype Map k v = Map { toMap :: HashMap k v }
    deriving
        ( Functor
        , Foldable
        , Traversable
        , Monoid
        , Semigroup
        , Eq
        , Read
        , Show
        )

type role Map nominal representational

_Map :: (Coercible a b, Coercible b a) => Iso' (Map k a) (HashMap k b)
_Map = iso (coerce . toMap) (Map . coerce)

instance (Hashable k, Eq k) => IsList (Map k v) where
   type Item (Map k v) = (k, v)

   fromList = Map . Map.fromList
   toList   = Map.toList . toMap

-- instance (Eq k, Hashable k, FromText k, FromJSON v) => FromJSON (Map k v) where
--     parseJSON = withObject "HashMap" $
--           fmap (Map . Map.fromList)
--         . traverse g
--         . Map.toList
--       where
--         g (k, v) = (,)
--             <$> either fail return (fromText k)
--             <*> parseJSON v

instance (Eq k, Hashable k, ToText k, ToJSON v) => ToJSON (Map k v) where
    toJSON = Object . Map.fromList . map (bimap toText toJSON) . toList

parseXMLMap :: (Eq k, Hashable k, FromText k, FromXML v)
            => Text
            -> Text
            -> Text
            -> [Node]
            -> Either String (Map k v)
parseXMLMap e k v = fmap fromList . traverse f . mapMaybe (childNodesOf e)
  where
    f ns = (,)
       <$> (ns .@ k >>= fromText)
       <*>  ns .@ v

toQueryMap :: (Hashable k, Eq k, ToQuery k, ToQuery v)
           => ByteString
           -> ByteString
           -> ByteString
           -> Map k v
           -> QueryString
toQueryMap e k v = toQueryList e . map f . toList
  where
    f (x, y) = QList [k =: toQuery x, v =: toQuery y]
