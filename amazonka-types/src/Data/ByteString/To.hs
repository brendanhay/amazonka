-- Module      : Data.ByteString.To
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.ByteString.To
    (
    -- * Class
      ToByteString (..)

    -- * Instance helpers
    , showByteString
    , fromBuilder
    ) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Builder   as LBS
import qualified Data.ByteString.Char8     as BS
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as Text
import           Network.HTTP.Types.Method

showByteString :: ToByteString a => a -> String
showByteString = BS.unpack . toByteString

fromBuilder :: LBS.Builder -> ByteString
fromBuilder = LBS.toStrict . LBS.toLazyByteString

class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString ByteString where toByteString = id
instance ToByteString Text       where toByteString = Text.encodeUtf8
instance ToByteString Int        where toByteString = fromBuilder . intDec
instance ToByteString Integer    where toByteString = fromBuilder . integerDec
instance ToByteString Float      where toByteString = fromBuilder . floatDec
instance ToByteString Double     where toByteString = fromBuilder . doubleDec
instance ToByteString StdMethod  where toByteString = renderStdMethod
