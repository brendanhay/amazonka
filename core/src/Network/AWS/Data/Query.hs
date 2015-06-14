{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Query where
    -- ( ToQuery (..)
    -- , renderQuery

    -- , Query
    -- -- , valuesOf

    -- , (=?)
    -- , pair
    -- ) where

import           Control.Lens
import qualified Data.ByteString.Char8       as BS
import           Data.Data
import           Data.Data.Lens
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.List                   (sort)
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Monoid
import           Data.String
import qualified Data.Text.Encoding          as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.HTTP.Types.URI      (urlEncode)
import           Numeric.Natural

data QueryString
    = List  [QueryString]
    | Pair  ByteString QueryString
    | Value (Maybe ByteString)
      deriving (Eq, Show, Data, Typeable)

makePrisms ''QueryString

instance Monoid QueryString where
    mempty = List []

    mappend a b = case (a, b) of
        (List l, List r) -> List (l ++ r)
        (List l, r)      -> List (r : l)
        (l,      List r) -> List (l : r)
        (l,      r)      -> List [l, r]

instance Plated QueryString where
    plate = uniplate

instance IsString QueryString where
    fromString = toQuery . BS.pack

instance ToByteString QueryString where
    toBS = intercalate . sort . enc Nothing
      where
        enc k = \case
            List xs           -> concatMap (enc k) xs

            Pair (urlEncode True -> k') x
                | Just n <- k -> enc (Just $ n <> "." <> k') x
                | otherwise   -> enc (Just k')               x

            Value (Just (urlEncode True -> v))
                | Just n <- k -> [n <> vsep <> v]
                | otherwise   -> [v <> vsep]

            _   | Just n <- k -> [n <> vsep]
                | otherwise   -> []

        intercalate []     = mempty
        intercalate [x]    = x
        intercalate (x:xs) = x <> ksep <> intercalate xs

        ksep = "&"
        vsep = "="

valuesOf :: Traversal' QueryString (Maybe ByteString)
valuesOf = deep _Value

pair :: ToQuery a => ByteString -> a -> QueryString -> QueryString
pair k v = mappend (Pair k (toQuery v))

infixr 7 =:

(=:) :: ToQuery a => ByteString -> a -> QueryString
k =: v = Pair k (toQuery v)

toQueryMap :: (ToQuery k, ToQuery v)
           => ByteString
           -> ByteString
           -> ByteString
           -> HashMap k v
           -> QueryString
toQueryMap e k v = Pair e . toQuery . map f . Map.toList
  where
    f (x, y) = List [k =: toQuery x, v =: toQuery y]

class ToQuery a where
    toQuery :: a -> QueryString

    default toQuery :: ToText a => a -> QueryString
    toQuery = toQuery . toText

instance ToQuery QueryString where
    toQuery = id

instance (ToByteString k, ToQuery v) => ToQuery (k, v) where
    toQuery (k, v) = Pair (toBS k) (toQuery v)

instance ToQuery Char where
    toQuery = toQuery . BS.singleton

instance ToQuery ByteString where
    toQuery "" = Value Nothing
    toQuery bs = Value (Just bs)

instance ToQuery Text    where toQuery = toQuery . Text.encodeUtf8
instance ToQuery Int     where toQuery = toQuery . toBS
instance ToQuery Integer where toQuery = toQuery . toBS
instance ToQuery Double  where toQuery = toQuery . toBS
instance ToQuery Natural where toQuery = toQuery . toBS

instance ToQuery a => ToQuery (Maybe a) where
    toQuery (Just x) = toQuery x
    toQuery Nothing  = mempty

instance ToQuery a => ToQuery [a] where
    toQuery = List . zipWith (\n v -> toBS n =: toQuery v) idx
      where
        idx = [1..] :: [Integer]

instance ToQuery a => ToQuery (NonEmpty a) where
    toQuery = toQuery . NonEmpty.toList

instance ToQuery Bool where
    toQuery True  = toQuery ("true"  :: ByteString)
    toQuery False = toQuery ("false" :: ByteString)
