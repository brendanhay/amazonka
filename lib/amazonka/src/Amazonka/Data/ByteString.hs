--

-- |
-- Module      : Amazonka.Data.ByteString
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.ByteString
  ( -- * ByteString
    ByteString,
    ByteStringLazy,
    ToByteString (..),
    showBS,
    stripBS,
  )
where

import Amazonka.Data.Text
import Amazonka.Prelude
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Numeric

showBS :: ToByteString a => a -> String
showBS = BS8.unpack . toBS

stripBS :: ByteString -> ByteString
stripBS = BS8.dropWhile Char.isSpace . fst . BS8.spanEnd Char.isSpace

class ToByteString a where
  toBS :: a -> ByteString
  default toBS :: ToText a => a -> ByteString
  toBS = Text.encodeUtf8 . toText

instance ToByteString ByteString where
  toBS = id

instance ToByteString ByteStringBuilder where
  toBS = toBS . Build.toLazyByteString

instance ToByteString ByteStringLazy where
  toBS = LBS.toStrict

instance ToByteString Text where
  toBS = Text.encodeUtf8

instance ToByteString String where
  toBS = BS8.pack

instance ToByteString Int where
  toBS = toBS . Build.intDec

instance ToByteString Integer where
  toBS = toBS . Build.integerDec

instance ToByteString Natural where
  toBS = toBS . toInteger

instance ToByteString Double where
  toBS = toBS . ($ "") . Numeric.showFFloat Nothing

instance ToByteString HTTP.StdMethod where
  toBS = HTTP.renderStdMethod

instance ToByteString UTCTime where
  toBS = BS8.pack . show

instance ToByteString a => ToByteString (CI a) where
  toBS = toBS . CI.original
