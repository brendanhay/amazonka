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

module Network.AWS.Data.Internal.ByteString
    ( ToByteString (..)
    , showBS

    , ToBuilder    (..)
    , buildBS

    , stripBS
    ) where

import           Crypto.Hash
import           Data.ByteString                (ByteString)
import           Data.ByteString.Builder        (Builder)
import qualified Data.ByteString.Builder        as Build
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Char
import           Data.Int
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Data.Time                      (UTCTime)
import           Network.AWS.Data.Internal.Text
import           Network.HTTP.Types.Method
import           Numeric.Natural

showBS :: ToByteString a => a -> String
showBS = BS.unpack . toBS

class ToByteString a where
    toBS :: a -> ByteString

    default toBS :: ToText a => a -> ByteString
    toBS = toBS . toText

instance ToByteString Builder    where toBS = buildBS
instance ToByteString ByteString where toBS = id
instance ToByteString Text       where toBS = Text.encodeUtf8
instance ToByteString Int        where toBS = buildBS
instance ToByteString Integer    where toBS = buildBS
instance ToByteString Natural    where toBS = buildBS
instance ToByteString Double     where toBS = buildBS
instance ToByteString StdMethod  where toBS = renderStdMethod
instance ToByteString (Digest a) where toBS = digestToHexByteString
instance ToByteString UTCTime    where toBS = BS.pack . show

buildBS :: ToBuilder a => a -> ByteString
buildBS = LBS.toStrict . Build.toLazyByteString . build

class ToBuilder a where
    build :: a -> Builder

    default build :: ToByteString a => a -> Builder
    build = build . toBS

instance ToBuilder Builder    where build = id
instance ToBuilder ByteString where build = Build.byteString
instance ToBuilder Char       where build = Build.charUtf8
instance ToBuilder [Char]     where build = Build.stringUtf8
instance ToBuilder Int        where build = Build.intDec
instance ToBuilder Int64      where build = Build.int64Dec
instance ToBuilder Integer    where build = Build.integerDec
instance ToBuilder Natural    where build = Build.integerDec . toInteger
instance ToBuilder Double     where build = Build.doubleDec
instance ToBuilder StdMethod
instance ToBuilder (Digest a)

stripBS :: ByteString -> ByteString
stripBS = BS.dropWhile isSpace . fst . BS.spanEnd isSpace
