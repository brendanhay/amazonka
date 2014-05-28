{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.ByteString
    (
    -- * Classes
    -- ** ToByteString
      ToByteString (..)
    , showByteString

    -- ** ToBuilder
    , ToBuilder    (..)
    , buildByteString
    ) where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder    (Builder)
import qualified Data.ByteString.Builder    as Build
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Int
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Network.HTTP.Types.Method

showByteString :: ToByteString a => a -> String
showByteString = BS.unpack . toByteString

class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString ByteString where toByteString = id
instance ToByteString Text       where toByteString = Text.encodeUtf8
instance ToByteString Int        where toByteString = buildByteString
instance ToByteString Integer    where toByteString = buildByteString
instance ToByteString Double     where toByteString = buildByteString
instance ToByteString StdMethod  where toByteString = renderStdMethod

buildByteString :: ToBuilder a => a -> ByteString
buildByteString = LBS.toStrict . Build.toLazyByteString . build

class ToBuilder a where
    build :: a -> Builder

    default build :: ToByteString a => a -> Builder
    build = build . toByteString

instance ToBuilder Builder    where build = id
instance ToBuilder ByteString where build = Build.byteString
instance ToBuilder Char       where build = Build.charUtf8
instance ToBuilder [Char]     where build = Build.stringUtf8
instance ToBuilder Int        where build = Build.intDec
instance ToBuilder Int64      where build = Build.int64Dec
instance ToBuilder Integer    where build = Build.integerDec
instance ToBuilder Double     where build = Build.doubleDec
