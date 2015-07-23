{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.AWS.Data.ByteString
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

--
module Network.AWS.Data.ByteString
    (
    -- * ByteString
      ByteString
    , LazyByteString
    , ToByteString (..)
    , showBS
    , stripBS

    -- * Builder
    , ToBuilder    (..)
    , buildLines
    ) where

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (Builder)
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Builder      as Build
import           Data.CaseInsensitive              (CI)
import qualified Data.CaseInsensitive              as CI
import           Data.Char
import           Data.Double.Conversion.ByteString (toShortest)
import           Data.Int
import           Data.List                         (intersperse)
import           Data.Monoid
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.Lazy                    as LText
import qualified Data.Text.Lazy.Encoding           as LText
import           Data.Time                         (UTCTime)
import           Data.Word
import           GHC.Float
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Text
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Numeric.Natural

type LazyByteString = LBS.ByteString

showBS :: ToByteString a => a -> String
showBS = BS8.unpack . toBS

stripBS :: ByteString -> ByteString
stripBS = BS8.dropWhile isSpace . fst . BS8.spanEnd isSpace

class ToByteString a where
    toBS :: a -> ByteString

    default toBS :: ToText a => a -> ByteString
    toBS = Text.encodeUtf8 . toText

instance ToByteString ByteString     where toBS = id
instance ToByteString Builder        where toBS = toBS . Build.toLazyByteString
instance ToByteString LazyByteString where toBS = LBS.toStrict
instance ToByteString Text           where toBS = Text.encodeUtf8
instance ToByteString Int            where toBS = toBS . Build.intDec
instance ToByteString Integer        where toBS = toBS . Build.integerDec
instance ToByteString Natural        where toBS = toBS . toInteger
instance ToByteString Double         where toBS = toShortest
instance ToByteString StdMethod      where toBS = renderStdMethod
instance ToByteString (Digest a)     where toBS = digestToBase Base16
instance ToByteString UTCTime        where toBS = BS8.pack . show

instance ToByteString a => ToByteString (CI a) where
    toBS = toBS . CI.original

class ToBuilder a where
    build :: a -> Builder

instance ToBuilder Builder        where build = id
instance ToBuilder LBS.ByteString where build = Build.lazyByteString
instance ToBuilder ByteString     where build = Build.byteString
instance ToBuilder Int            where build = Build.intDec
instance ToBuilder Int8           where build = Build.int8Dec
instance ToBuilder Int16          where build = Build.int16Dec
instance ToBuilder Int32          where build = Build.int32Dec
instance ToBuilder Int64          where build = Build.int64Dec
instance ToBuilder Integer        where build = Build.integerDec
instance ToBuilder Word           where build = Build.wordDec
instance ToBuilder Word8          where build = Build.word8Dec
instance ToBuilder Word16         where build = Build.word16Dec
instance ToBuilder Word32         where build = Build.word32Dec
instance ToBuilder Word64         where build = Build.word64Dec
instance ToBuilder UTCTime        where build = Build.stringUtf8 . show
instance ToBuilder Float          where build = build . toShortest . float2Double
instance ToBuilder Double         where build = build . toShortest
instance ToBuilder Text           where build = build . Text.encodeUtf8
instance ToBuilder LText.Text     where build = build . LText.encodeUtf8
instance ToBuilder Char           where build = build . Text.singleton
instance ToBuilder [Char]         where build = build . LText.pack
instance ToBuilder StdMethod      where build = build . renderStdMethod

instance ToBuilder a => ToBuilder (CI a) where
    build = build . CI.foldedCase

instance ToBuilder a => ToBuilder (Maybe a) where
    build Nothing  = "Nothing"
    build (Just x) = "Just " <> build x

instance ToBuilder Bool where
    build True  = "True"
    build False = "False"

instance ToBuilder Status where
    build x = build (statusCode x) <> " " <> build (statusMessage x)

instance ToBuilder [Header] where
    build = mconcat . intersperse "; " . map (\(k, v) -> build k <> ": " <> build v)

instance ToBuilder HttpVersion where
    build HttpVersion{..} = "HTTP/" <> build httpMajor <> build '.' <> build httpMinor

instance ToBuilder RequestBody where
    build = \case
        RequestBodyBuilder     n _ -> " <msger:"   <> build n <> ">"
        RequestBodyStream      n _ -> " <stream:"  <> build n <> ">"
        RequestBodyStreamChunked _ -> " <chunked>"

        RequestBodyLBS lbs
            | n <= 4096            -> build lbs
            | otherwise            -> " <lazy:" <> build n <> ">"
          where
            n = LBS.length lbs

        RequestBodyBS bs
            | n <= 4096            -> build bs
            | otherwise            -> " <strict:" <> build n <> ">"
          where
            n = BS.length bs

instance ToBuilder HttpException where
    build x = "[HttpException] {\n" <> build (show x) <> "\n}"

instance ToBuilder Request where
    build x = buildLines
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
    build x = buildLines
        [ "[Client Response] {"
        , "  status  = " <> build (responseStatus  x)
        , "  version = " <> build (responseVersion x)
        , "  headers = " <> build (responseHeaders x)
        , "  cookies = " <> build (show (responseCookieJar x))
        , "}"
        ]

buildLines :: [Builder] -> Builder
buildLines = mconcat . intersperse "\n"
