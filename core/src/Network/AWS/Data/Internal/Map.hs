{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
    ( Map  (..)
    , _Map

    , EMap (..)
    , _EMap
    ) where

import           Control.Applicative
import           Control.Lens                         hiding (coerce, element)
import           Data.Aeson
import qualified Data.ByteString.Char8                as BS
import           Data.Coerce
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Hashable                        (Hashable)
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup                       (Semigroup)
import           Data.String
import           GHC.Exts
import           GHC.TypeLits
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML
import           Text.XML

newtype Map k v = Map
    { fromMap :: HashMap k v
    } deriving (Eq, Show, Monoid, Semigroup)

type role Map nominal representational

_Map :: (Coercible a b, Coercible b a) => Iso' (Map k a) (HashMap k b)
_Map = iso (coerce . fromMap) (Map . coerce)

instance (Eq k, Hashable k) => IsList (Map k v) where
    type Item (Map k v) = (k, v)

    fromList = Map . Map.fromList
    toList   = Map.toList . fromMap

instance (Eq k, Hashable k, FromText k, FromJSON v) => FromJSON (Map k v) where
    parseJSON = withObject "HashMap" $
          fmap (Map . Map.fromList)
        . traverse g
        . Map.toList
      where
        g (k, v) = (,)
            <$> either fail return (fromText k)
            <*> parseJSON v

instance (Eq k, Hashable k, ToText k, ToJSON v) => ToJSON (Map k v) where
    toJSON = Object . Map.fromList . map (bimap toText toJSON) . toList

newtype EMap (e :: Symbol) (i :: Symbol) (j :: Symbol) k v = EMap
    { fromEMap :: HashMap k v
    } deriving (Eq, Show, Monoid, Semigroup)

type role EMap phantom phantom phantom nominal representational

_EMap :: (Coercible a b, Coercible b a) => Iso' (EMap e i j k a) (HashMap k b)
_EMap = iso (coerce . fromEMap) (EMap . coerce)

instance (Eq k, Hashable k) => IsList (EMap e i j k v) where
    type Item (EMap e i j k v) = (k, v)

    fromList = EMap . Map.fromList
    toList   = Map.toList . fromEMap

instance ( KnownSymbol e
         , KnownSymbol i
         , KnownSymbol j
         , Eq k
         , Hashable k
         , ToQuery k
         , ToQuery v
         ) => ToQuery (EMap e i j k v) where
    toQuery m = toBS e =? (mconcat . zipWith go idx $ toList m)
      where
        go n (k, v) = toBS n =? toQuery (i, k) <> toQuery (j, v)

        idx = [1..] :: [Integer]

        i = BS.pack $ symbolVal (Proxy :: Proxy i)
        j = BS.pack $ symbolVal (Proxy :: Proxy j)
        e = BS.pack $ symbolVal (Proxy :: Proxy e)

instance ( KnownSymbol e
         , KnownSymbol i
         , KnownSymbol j
         , Eq k
         , Hashable k
         , FromXML k
         , FromXML v
         ) => FromXML (EMap e i j k v) where
    parseXML = fmap fromList . traverse (withElement e go . (:[]))
      where
        go ns
            | length ns == 2 =
                (,) <$> withElement i parseXML ns
                    <*> withElement j parseXML ns
            | otherwise      =
                Left $ "Expected two elements named "
                     ++ show i ++ " and "
                     ++ show j ++ " within "
                     ++ show e

        i = fromString $ symbolVal (Proxy :: Proxy i)
        j = fromString $ symbolVal (Proxy :: Proxy j)
        e = fromString $ symbolVal (Proxy :: Proxy e)

instance ( KnownSymbol e
         , KnownSymbol i
         , KnownSymbol j
         , Eq k
         , Hashable k
         , ToXML k
         , ToXML v
         ) => ToXML (EMap e i j k v) where
    toXML = map (uncurry go) . toList
      where
        go k v =
            NodeElement $ element e
                [ NodeElement (element i (toXML k))
                , NodeElement (element j (toXML v))
                ]

        i = fromString $ symbolVal (Proxy :: Proxy i)
        j = fromString $ symbolVal (Proxy :: Proxy j)
        e = fromString $ symbolVal (Proxy :: Proxy e)
