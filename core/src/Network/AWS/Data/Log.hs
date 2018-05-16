{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}

{-# OPTIONS_GHC -fsimpl-tick-factor=110 #-}

-- |
-- Module      : Network.AWS.Data.Log
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Log where

import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Int
import           Data.List                    (intersperse)
import           Data.Monoid
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import           Data.Time                    (UTCTime)
import           Data.Word
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Numeric

class ToLog a where
    -- | Convert a value to a loggable builder.
    build :: a -> Builder

instance ToLog Builder        where build = id
instance ToLog LBS.ByteString where build = Build.lazyByteString
instance ToLog ByteString     where build = Build.byteString
instance ToLog Int            where build = Build.intDec
instance ToLog Int8           where build = Build.int8Dec
instance ToLog Int16          where build = Build.int16Dec
instance ToLog Int32          where build = Build.int32Dec
instance ToLog Int64          where build = Build.int64Dec
instance ToLog Integer        where build = Build.integerDec
instance ToLog Word           where build = Build.wordDec
instance ToLog Word8          where build = Build.word8Dec
instance ToLog Word16         where build = Build.word16Dec
instance ToLog Word32         where build = Build.word32Dec
instance ToLog Word64         where build = Build.word64Dec
instance ToLog UTCTime        where build = Build.stringUtf8 . show
instance ToLog Float          where build = build . ($ "") . showFFloat Nothing
instance ToLog Double         where build = build . ($ "") . showFFloat Nothing
instance ToLog Text           where build = build . Text.encodeUtf8
instance ToLog LText.Text     where build = build . LText.encodeUtf8
instance ToLog Char           where build = build . Text.singleton
instance ToLog [Char]         where build = build . LText.pack
instance ToLog StdMethod      where build = build . renderStdMethod
instance ToLog QueryString    where build = build . toBS
instance ToLog EscapedPath    where build = build . toBS

-- | Intercalate a list of 'Builder's with newlines.
buildLines :: [Builder] -> Builder
buildLines = mconcat . intersperse "\n"

instance ToLog a => ToLog (CI a) where
    build = build . CI.foldedCase

instance ToLog a => ToLog (Maybe a) where
    build Nothing  = "Nothing"
    build (Just x) = "Just " <> build x

instance ToLog Bool where
    build True  = "True"
    build False = "False"

instance ToLog Status where
    build x = build (statusCode x) <> " " <> build (statusMessage x)

instance ToLog [Header] where
    build = mconcat
        . intersperse "; "
        . map (\(k, v) -> build k <> ": " <> build v)

instance ToLog HttpVersion where
    build HttpVersion{..} =
           "HTTP/"
        <> build httpMajor
        <> build '.'
        <> build httpMinor

instance ToLog RequestBody where
    build = \case
        RequestBodyBuilder     n _ -> " <builder:" <> build n <> ">"
        RequestBodyStream      n _ -> " <stream:"  <> build n <> ">"

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

        _                          -> " <chunked>"

instance ToLog HttpException where
    build x = "[HttpException] {\n" <> build (show x) <> "\n}"

#if MIN_VERSION_http_client(0,5,0)
instance ToLog HttpExceptionContent where
    build x = "[HttpExceptionContent] {\n" <> build (show x) <> "\n}"
#endif


instance ToLog Request where
    build x = buildLines
        [  "[Client Request] {"
        ,  "  host      = " <> build (host x) <> ":" <> build (port x)
        ,  "  secure    = " <> build (secure x)
        ,  "  method    = " <> build (method x)
        ,  "  target    = " <> build target
#if MIN_VERSION_http_client(0,5,0)
        ,  "  timeout   = " <> build (show (responseTimeout x))
#else
        ,  "  timeout   = " <> build (responseTimeout x)
#endif
        ,  "  redirects = " <> build (redirectCount x)
        ,  "  path      = " <> build (path            x)
        ,  "  query     = " <> build (queryString     x)
        ,  "  headers   = " <> build (requestHeaders  x)
        ,  "  body      = " <> build (requestBody     x)
        ,  "}"
        ]
      where
        target = hAMZTarget `lookup` requestHeaders x

instance ToLog (Response a) where
    build x = buildLines
        [ "[Client Response] {"
        , "  status  = " <> build (responseStatus  x)
        , "  headers = " <> build (responseHeaders x)
        , "}"
        ]
