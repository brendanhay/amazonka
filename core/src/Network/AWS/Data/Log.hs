{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

-- |
-- Module      : Network.AWS.Data.Log
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
import           GHC.Float
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Numeric

import           Prelude

class ToLog a where
    message :: a -> Builder

instance ToLog Builder        where message = id
instance ToLog LBS.ByteString where message = Build.lazyByteString
instance ToLog ByteString     where message = Build.byteString
instance ToLog Int            where message = Build.intDec
instance ToLog Int8           where message = Build.int8Dec
instance ToLog Int16          where message = Build.int16Dec
instance ToLog Int32          where message = Build.int32Dec
instance ToLog Int64          where message = Build.int64Dec
instance ToLog Integer        where message = Build.integerDec
instance ToLog Word           where message = Build.wordDec
instance ToLog Word8          where message = Build.word8Dec
instance ToLog Word16         where message = Build.word16Dec
instance ToLog Word32         where message = Build.word32Dec
instance ToLog Word64         where message = Build.word64Dec
instance ToLog UTCTime        where message = Build.stringUtf8 . show
instance ToLog Float          where message = message . ($ "") . showFFloat Nothing
instance ToLog Double         where message = message . ($ "") . showFFloat Nothing
instance ToLog Text           where message = message . Text.encodeUtf8
instance ToLog LText.Text     where message = message . LText.encodeUtf8
instance ToLog Char           where message = message . Text.singleton
instance ToLog [Char]         where message = message . LText.pack
instance ToLog StdMethod      where message = message . renderStdMethod
instance ToLog QueryString    where message = message . toBS
instance ToLog EscapedPath    where message = message . toBS

buildLines :: [Builder] -> Builder
buildLines = mconcat . intersperse "\n"

instance ToLog a => ToLog (CI a) where
    message = message . CI.foldedCase

instance ToLog a => ToLog (Maybe a) where
    message Nothing  = "Nothing"
    message (Just x) = "Just " <> message x

instance ToLog Bool where
    message True  = "True"
    message False = "False"

instance ToLog Status where
    message x = message (statusCode x) <> " " <> message (statusMessage x)

instance ToLog [Header] where
    message = mconcat
        . intersperse "; "
        . map (\(k, v) -> message k <> ": " <> message v)

instance ToLog HttpVersion where
    message HttpVersion{..} =
           "HTTP/"
        <> message httpMajor
        <> message '.'
        <> message httpMinor

instance ToLog RequestBody where
    message = \case
        RequestBodyBuilder     n _ -> " <msger:"   <> message n <> ">"
        RequestBodyStream      n _ -> " <stream:"  <> message n <> ">"
        RequestBodyStreamChunked _ -> " <chunked>"

        RequestBodyLBS lbs
            | n <= 4096            -> message lbs
            | otherwise            -> " <lazy:" <> message n <> ">"
          where
            n = LBS.length lbs

        RequestBodyBS bs
            | n <= 4096            -> message bs
            | otherwise            -> " <strict:" <> message n <> ">"
          where
            n = BS.length bs

instance ToLog HttpException where
    message x = "[HttpException] {\n" <> message (show x) <> "\n}"

instance ToLog Request where
    message x = buildLines
        [ "[Client Request] {"
        , "  host              = " <> message (host            x)
        , "  port              = " <> message (port            x)
        , "  secure            = " <> message (secure          x)
        , "  headers           = " <> message (requestHeaders  x)
        , "  path              = " <> message (path            x)
        , "  query             = " <> message (queryString     x)
        , "  method            = " <> message (method          x)
        , "  redirect count    = " <> message (redirectCount   x)
        , "  response timeout  = " <> message (responseTimeout x)
        , "  request version   = " <> message (requestVersion  x)
        , "}"
        ]

instance ToLog (Response a) where
    message x = buildLines
        [ "[Client Response] {"
        , "  status  = " <> message (responseStatus  x)
        , "  version = " <> message (responseVersion x)
        , "  headers = " <> message (responseHeaders x)
        , "  cookies = " <> message (show (responseCookieJar x))
        , "}"
        ]
