{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.ByteString
    ( LazyByteString
    , ToByteString (..)
    , ToBuilder    (..)
    , showBS
    , buildBS
    , stripBS
    ) where

import           Crypto.Hash
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.ByteString.Builder        (Builder)
import qualified Data.ByteString.Builder        as Build
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import           Data.CaseInsensitive           (CI)
import qualified Data.CaseInsensitive           as CI
import           Data.Char
import           Data.Int
import           Data.List                      (intersperse)
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Data.Time                      (UTCTime)
import           Network.AWS.Data.Internal.Text
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Numeric.Natural

type LazyByteString = LBS.ByteString

showBS :: ToByteString a => a -> String
showBS = BS8.unpack . toBS

class ToByteString a where
    toBS :: a -> ByteString

    default toBS :: ToText a => a -> ByteString
    toBS = Text.encodeUtf8 . toText

instance ToByteString ByteString     where toBS = id
instance ToByteString Builder        where toBS = toBS . Build.toLazyByteString
instance ToByteString LazyByteString where toBS = LBS.toStrict
instance ToByteString Text           where toBS = Text.encodeUtf8
instance ToByteString Int            where toBS = toBS . buildBS
instance ToByteString Integer        where toBS = toBS . buildBS
instance ToByteString Natural        where toBS = toBS . buildBS
instance ToByteString Double         where toBS = toBS . buildBS
instance ToByteString StdMethod      where toBS = renderStdMethod
instance ToByteString (Digest a)     where toBS = digestToHexByteString
instance ToByteString UTCTime        where toBS = BS8.pack . show

instance ToByteString a => ToByteString (CI a) where
    toBS = toBS . CI.original

buildBS :: ToBuilder a => a -> LazyByteString
buildBS = Build.toLazyByteString . build

class ToBuilder a where
    build :: a -> Builder

    default build :: ToByteString a => a -> Builder
    build = build . toBS

instance ToBuilder Builder        where build = id
instance ToBuilder ByteString     where build = Build.byteString
instance ToBuilder LazyByteString where build = Build.lazyByteString
instance ToBuilder Text           where build = Build.byteString . toBS
instance ToBuilder Char           where build = Build.charUtf8
instance ToBuilder [Char]         where build = Build.stringUtf8
instance ToBuilder Int            where build = Build.intDec
instance ToBuilder Int64          where build = Build.int64Dec
instance ToBuilder Integer        where build = Build.integerDec
instance ToBuilder Natural        where build = Build.integerDec . toInteger
instance ToBuilder Double         where build = Build.doubleDec
instance ToBuilder StdMethod
instance ToBuilder (Digest a)

instance ToBuilder Bool where
    build True  = "True"
    build False = "False"

instance ToBuilder UTCTime where
    build = Build.stringUtf8 . show

instance ToBuilder a => ToBuilder (Maybe a) where
    build Nothing  = "Nothing"
    build (Just x) = "Just " <> build x

instance ToBuilder a => ToBuilder (CI a) where
    build = build . CI.foldedCase

instance ToBuilder [Header] where
    build = mconcat
          . intersperse "; "
          . map (\(k, v) -> build k <> ": " <> build v)

instance ToBuilder HttpVersion where
    build HttpVersion{..} = "HTTP/"
        <> build httpMajor
        <> build '.'
        <> build httpMinor

instance ToBuilder RequestBody where
    build = \case
        RequestBodyLBS lbs
            | n <= m               -> build lbs
            | otherwise            -> "      <lazy:"    <> build n <> ">"
          where
            n = LBS.length lbs

        RequestBodyBS bs
            | n <= fromIntegral m  -> build bs
            | otherwise            -> "      <strict:"  <> build n <> ">"
          where
            n = BS.length bs

        RequestBodyBuilder n _     -> "      <builder:" <> build n <> ">"
        RequestBodyStream  n _     -> "      <stream:"  <> build n <> ">"
        RequestBodyStreamChunked _ -> "      <chunked>"
      where
        m :: Int64
        m = 4096

instance ToBuilder Request where
    build x = mconcat $ intersperse "\n"
        [ "[Client Request] {"
        , "  host              = " <> build (host            x)
        , "  port              = " <> build (port            x)
        , "  secure            = " <> build (secure          x)
        , "  headers           = " <> build (requestHeaders  x)
        , "  path              = " <> build (path            x)
        , "  query             = " <> build (queryString     x)
        , "  method            = " <> build (method          x)
        , "  redirect count    = " <> build (redirectCount   x)
        , "  response timeout  = " <> build (responseTimeout x)
        , "  request version   = " <> build (requestVersion  x)
        , "}"
        ]

instance ToBuilder (Response a) where
    build x = mconcat $ intersperse "\n"
        [ "[Client Response] {"
        , "  status code    = " <> build (statusCode      s)
        , "  status message = " <> build (statusMessage   s)
        , "  version        = " <> build (responseVersion x)
        , "  headers        = " <> build (responseHeaders x)
        , "  cookies        = " <> build (show (responseCookieJar x))
        , "}"
        ]
      where
        s = responseStatus x

stripBS :: ByteString -> ByteString
stripBS = BS8.dropWhile isSpace . fst . BS8.spanEnd isSpace
