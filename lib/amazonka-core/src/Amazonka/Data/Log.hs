-- |
-- Module      : Amazonka.Data.Log
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Log where

import Amazonka.Data.ByteString
import Amazonka.Data.Headers
import Amazonka.Data.Path
import Amazonka.Data.Query
import Amazonka.Data.Text
import Amazonka.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP
import qualified Numeric

class ToLog a where
  -- | Convert a value to a loggable builder.
  build :: a -> ByteStringBuilder

instance ToLog ByteStringBuilder where
  build = id

instance ToLog ByteStringLazy where
  build lbs = case LText.decodeUtf8' lbs of
    Right lt | LText.all Char.isPrint lt -> Build.lazyByteString lbs
    _ -> mconcat ["non-printable lazy ByteString (", build (LBS.length lbs), " bytes)"]

instance ToLog ByteString where
  build bs = case Text.decodeUtf8' bs of
    Right t | Text.all Char.isPrint t -> Build.byteString bs
    _ -> mconcat ["non-printable strict ByteString (", build (BS.length bs), " bytes)"]

instance ToLog Int where
  build = Build.intDec

instance ToLog Int8 where
  build = Build.int8Dec

instance ToLog Int16 where
  build = Build.int16Dec

instance ToLog Int32 where
  build = Build.int32Dec

instance ToLog Int64 where
  build = Build.int64Dec

instance ToLog Integer where
  build = Build.integerDec

instance ToLog Word where
  build = Build.wordDec

instance ToLog Word8 where
  build = Build.word8Dec

instance ToLog Word16 where
  build = Build.word16Dec

instance ToLog Word32 where
  build = Build.word32Dec

instance ToLog Word64 where
  build = Build.word64Dec

instance ToLog UTCTime where
  build = Build.stringUtf8 . show

instance ToLog Float where
  build = build . ($ "") . Numeric.showFFloat Nothing

instance ToLog Double where
  build = build . ($ "") . Numeric.showFFloat Nothing

instance ToLog Text where
  build = build . Text.encodeUtf8

instance ToLog TextLazy where
  build = build . LText.encodeUtf8

instance ToLog Char where
  build = build . Text.singleton

instance ToLog [Char] where
  build = build . LText.pack

instance ToLog HTTP.StdMethod where
  build = build . HTTP.renderStdMethod

instance ToLog QueryString where
  build = build . toBS

instance ToLog EscapedPath where
  build = build . toBS

-- | Intercalate a list of 'ByteStringBuilder's with newlines.
buildLines :: [ByteStringBuilder] -> ByteStringBuilder
buildLines = mconcat . List.intersperse "\n"

instance ToLog a => ToLog (CI a) where
  build = build . CI.foldedCase

instance ToLog a => ToLog (Maybe a) where
  build Nothing = "Nothing"
  build (Just x) = "Just " <> build x

instance ToLog Bool where
  build True = "True"
  build False = "False"

instance ToLog HTTP.Status where
  build x = build (HTTP.statusCode x) <> " " <> build (HTTP.statusMessage x)

instance ToLog [HTTP.Header] where
  build =
    mconcat
      . List.intersperse "; "
      . map (\(k, v) -> build k <> ": " <> build v)

instance ToLog HTTP.HttpVersion where
  build HTTP.HttpVersion {httpMajor, httpMinor} =
    "HTTP/"
      <> build httpMajor
      <> build '.'
      <> build httpMinor

instance ToLog Client.RequestBody where
  build = \case
    Client.RequestBodyBuilder n _ -> " <builder:" <> build n <> ">"
    Client.RequestBodyStream n _ -> " <stream:" <> build n <> ">"
    Client.RequestBodyLBS lbs
      | n <= 4096 -> build lbs
      | otherwise -> " <lazy:" <> build n <> ">"
      where
        n = LBS.length lbs
    Client.RequestBodyBS bs
      | n <= 4096 -> build bs
      | otherwise -> " <strict:" <> build n <> ">"
      where
        n = BS.length bs
    _ -> " <chunked>"

instance ToLog Client.HttpException where
  build x = "[HttpException] {\n" <> build (show x) <> "\n}"

instance ToLog Client.HttpExceptionContent where
  build x = "[HttpExceptionContent] {\n" <> build (show x) <> "\n}"

instance ToLog Client.Request where
  build x =
    buildLines
      [ "[Client Request] {",
        "  host      = " <> build (Client.host x) <> ":" <> build (Client.port x),
        "  secure    = " <> build (Client.secure x),
        "  method    = " <> build (Client.method x),
        "  target    = " <> build target,
        "  timeout   = " <> build (show (Client.responseTimeout x)),
        "  redirects = " <> build (Client.redirectCount x),
        "  path      = " <> build (Client.path x),
        "  query     = " <> build (Client.queryString x),
        "  headers   = " <> build (Client.requestHeaders x),
        "  body      = " <> build (Client.requestBody x),
        "}"
      ]
    where
      target = hAMZTarget `lookup` Client.requestHeaders x

instance ToLog (Client.Response a) where
  build x =
    buildLines
      [ "[Client Response] {",
        "  status  = " <> build (Client.responseStatus x),
        "  headers = " <> build (Client.responseHeaders x),
        "}"
      ]
