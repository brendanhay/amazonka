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

module Network.AWS.Data.Internal.Map where
    -- ( Map (..)
    -- , _Map
    -- , (~::)
    -- ) where

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
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup                       (Semigroup)
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           GHC.Exts
import           GHC.TypeLits
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Flatten
import           Network.AWS.Data.Internal.Header
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML
import           Network.HTTP.Types.Header            (Header)
import           Text.XML

newtype Map (e :: Symbol) k v = Map { toHashMap :: HashMap k v }
    deriving (Eq, Show, Monoid, Semigroup)

type role Map phantom nominal representational

_Map :: (Coercible a b, Coercible b a) => Iso' (Map e k a) (HashMap k b)
_Map = iso (coerce . toHashMap) (Map . coerce)

-- (~::) :: FromText v => [Header] -> Text -> Either String (Map e Text v)
-- (~::) hs p = Map
--     . Map.filterWithKey (const . Text.isPrefixOf p)
--     . Map.fromList <$> mapM f hs
--   where
--     f (k, v) = (Text.decodeUtf8 (CI.foldedCase k),)
--         <$> fromText (Text.decodeUtf8 v)

instance (Eq k, Hashable k) => IsList (Map e k v) where
    type Item (Map e k v) = (k, v)

    fromList = Map . Map.fromList
    toList   = Map.toList . toHashMap

-- instance (ToByteString k, ToByteString v) => ToHeader (Map e k v) where
--     toHeader k = map (bimap (mappend k . CI.mk . toBS) toBS) . toList

-- Move the x-amz-meta shit into Map witness?

-- instance (ToByteString k, ToQuery v) => ToQuery (Map k v) where
--     toQuery = toQuery . map (toQuery . first toBS) . Map.toList . toHashMap

instance (Eq k, Hashable k, FromText k, FromJSON v) => FromJSON (Map e k v) where
    parseJSON = withObject "HashMap" (fmap fromList . traverse g . toList)
      where
        g (k, v) = (,)
            <$> either fail return (fromText k)
            <*> parseJSON v

instance (Eq k, Hashable k, ToText k, ToJSON v) => ToJSON (Map e k v) where
    toJSON = Object . fromList . map (bimap toText toJSON) . toList

instance ( KnownSymbol e
         , Eq k
         , Hashable k
         , FromXML k
         , FromXML v
         ) => FromXML (Map e k v) where
    parseXML = fmap fromList . traverse (withElement n try . (:[]))
      where
        n = Text.pack $ symbolVal (Proxy :: Proxy e)

        try ns = pair "key"  "value" ns
             <|> pair "Name" "Value" ns

        pair k v ns
            | length ns == 2 =
                (,) <$> withElement k parseXML ns
                    <*> withElement v parseXML ns
            | otherwise      =
                Left $ "Expected two elements named "
                     ++ show k ++ " and "
                     ++ show v ++ " within "
                     ++ show n

instance ( KnownSymbol e
         , Eq k
         , Hashable k
         , ToXML k
         , ToXML v
         ) => ToXML (Map e k v) where
    toXML = map (uncurry go) . toList
      where
        n = fromString $ symbolVal (Proxy :: Proxy e)
        n = fromString $ symbolVal (Proxy :: Proxy i)
        n = fromString $ symbolVal (Proxy :: Proxy j)

        go k v =

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
