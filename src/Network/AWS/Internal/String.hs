{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Internal.String
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.String
    (
    -- * Type Class
      IsByteString (..)

    -- * ByteStrings
    , strip
    , stripLower
    , stripPrefix
    , wrap
    , addPrefix
    , addSuffix
    , lowerHead
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Text

class IsByteString a where
    toBS :: a -> ByteString

instance IsByteString ByteString where
    toBS = id

instance IsByteString Text where
    toBS = Text.encodeUtf8

instance IsByteString String where
    toBS = BS.pack

strip :: IsByteString a => Char -> a -> ByteString
strip c (toBS -> bs)
    | BS.null bs = bs
    | fst m      = c `strip` BS.tail bs
    | snd m      = c `strip` BS.init bs
    | otherwise  = bs
  where
    m = c `match` bs

stripLower :: IsByteString a => a -> ByteString
stripLower = BS.dropWhile isLower . toBS

stripPrefix :: IsByteString a => a -> a -> ByteString
stripPrefix (toBS -> x) (toBS -> y)
    | x `BS.isPrefixOf` y = BS.drop (BS.length x) y
    | otherwise           = y

wrap :: IsByteString a => Char -> a -> ByteString
wrap c (toBS -> bs) = case c `match` bs of
    (True,  True)  -> bs
    (False, True)  -> c `BS.cons` bs
    (True,  False) -> bs `BS.snoc` c
    (False, False) -> let b = BS.singleton c
                      in  BS.concat [b, bs, b]

addPrefix :: IsByteString a => a -> a -> ByteString
addPrefix (toBS -> x) (toBS -> y)
    | x `BS.isPrefixOf` y = y
    | otherwise           = x <> y

addSuffix :: IsByteString a => a -> a -> ByteString
addSuffix (toBS -> x) (toBS -> y)
    | x `BS.isSuffixOf` y = y
    | otherwise           = y <> x

lowerHead :: IsByteString a => a -> ByteString
lowerHead (toBS -> bs)
    | BS.null bs = bs
    | isUpper h  = toLower h `BS.cons` BS.tail bs
    | otherwise  = bs
  where
    h = BS.head bs

match :: Char -> ByteString -> (Bool, Bool)
match c bs
    | BS.null bs = (False, False)
    | otherwise  = (c == BS.head bs, c == BS.last bs)
