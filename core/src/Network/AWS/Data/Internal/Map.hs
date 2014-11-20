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
    ( Map (..)
    , _Map
    ) where
    -- ( Map (..)
    -- , _Map
    -- , (~::)
    -- ) where

import           Control.Applicative
import           Control.Lens                         hiding (coerce, element)
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Char8                as BS
import qualified Data.CaseInsensitive                 as CI
import           Data.Coerce
import           Data.Foldable                        (Foldable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Hashable                        (Hashable)
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup                       (Semigroup)
import           Data.String
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           GHC.TypeLits
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Header
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML
import           Network.HTTP.Types.Header            (Header)
import           Text.XML

newtype Map (e :: Symbol) (i :: Symbol) (j :: Symbol) k v = Map
    { toHashMap :: HashMap k v
    } deriving (Eq, Show, Monoid, Semigroup)

type role Map phantom phantom phantom nominal representational

_Map :: (Coercible a b, Coercible b a) => Iso' (Map e i j k a) (HashMap k b)
_Map = iso (coerce . toHashMap) (Map . coerce)

fromList :: (Eq k, Hashable k) => [(k, v)] -> Map e i j k v
fromList = Map . Map.fromList

toList :: Map e i j k v -> [(k, v)]
toList = Map.toList . toHashMap

instance ( KnownSymbol e
         , KnownSymbol i
         , KnownSymbol j
         , Eq k
         , Hashable k
         , ToQuery k
         , ToQuery v
         ) => ToQuery (Map e i j k v) where
    toQuery m = toBS e =? (mconcat . zipWith go idx $ toList m)
      where
        go n (k, v) = toBS n =? toQuery (i, k) <> toQuery (j, v)

        idx = [1..] :: [Integer]

        i = BS.pack $ symbolVal (Proxy :: Proxy i)
        j = BS.pack $ symbolVal (Proxy :: Proxy j)
        e = BS.pack $ symbolVal (Proxy :: Proxy e)

-- instance ( Eq k
--          , Hashable k
--          , FromText k
--          , FromJSON v
--          ) => FromJSON (Map e i j k v) where
--     parseJSON = withObject "HashMap" (fmap fromList . traverse g . Map.toList)
--       where
--         g (k, v) = (,)
--             <$> either fail return (fromText k)
--             <*> parseJSON v

-- instance ( Eq k
--          , Hashable k
--          , ToText k
--          , ToJSON v
--          ) => ToJSON (Map e i j k v) where
--     toJSON = Object . Map.fromList . map (bimap toText toJSON) . toList

instance ( KnownSymbol e
         , KnownSymbol i
         , KnownSymbol j
         , Eq k
         , Hashable k
         , FromXML k
         , FromXML v
         ) => FromXML (Map e i j k v) where
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
         ) => ToXML (Map e i j k v) where
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

--    flattened: true
-- looks for many top-level element name
-- <Attribute>
--   <Name>ReceiveMessageWaitTimeSeconds</Name>
--   <Value>2</Value>
-- </Attribute>
-- <Attribute>
--   <Name>VisibilityTimeout</Name>
--   <Value>30</Value>
-- </Attribute>
-- <Attribute>

--    flattened: false
-- looks for top-level element name, then <entry>

-- <Attributes>
--   <entry>
--     <key>Owner</key>
--     <value>123456789012</value>
--   </entry>
--   <entry>
--     <key>Policy</key>
--     <value>x</value>
--   </entry>
--   <entry>
--     <key>TopicArn</key>
--     <value>arn:aws:sns:us-east-1:123456789012:My-Topic</value>
--   </entry>
-- </Attributes>
