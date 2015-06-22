{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Logging
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Types and functions for optional logging machinery used during the
-- request, response, and signing life-cycles.
module Network.AWS.Logging
    ( LogLevel  (..)
    , Logger
    , ToMessage (..)
    , newLogger
    , info
    , debug
    , trace
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Double.Conversion.Text
import           Data.Int
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (encodeUtf8)
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import           Data.Time                    (UTCTime)
import           Data.Word
import           GHC.Float
import           Network.AWS.Types
import           Network.HTTP.Client
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Types
import           System.IO

-- | This is a primitive logger which can be used to log messages to a 'Handle'.
--
-- /Note/: A more sophisticated logging library such as tinylog or FastLogger
-- should be used in production code.
newLogger :: MonadIO m => LogLevel -> Handle -> m Logger
newLogger x hd = liftIO $ do
    hSetBinaryMode hd True
    hSetBuffering  hd LineBuffering
    return $ \y b ->
        when (x >= y) $
            Build.hPutBuilder hd (b <> "\n")

info, debug, trace :: (MonadIO m, ToMessage a) => Logger -> a -> m ()
info  f = liftIO . f Info  . msg
debug f = liftIO . f Debug . msg
trace f = liftIO . f Trace . msg

class ToMessage a where
    msg :: a -> Builder

instance ToMessage Builder        where msg = id
instance ToMessage LBS.ByteString where msg = Build.lazyByteString
instance ToMessage ByteString     where msg = Build.byteString
instance ToMessage Int            where msg = Build.intDec
instance ToMessage Int8           where msg = Build.int8Dec
instance ToMessage Int16          where msg = Build.int16Dec
instance ToMessage Int32          where msg = Build.int32Dec
instance ToMessage Int64          where msg = Build.int64Dec
instance ToMessage Integer        where msg = Build.integerDec
instance ToMessage Word           where msg = Build.wordDec
instance ToMessage Word8          where msg = Build.word8Dec
instance ToMessage Word16         where msg = Build.word16Dec
instance ToMessage Word32         where msg = Build.word32Dec
instance ToMessage Word64         where msg = Build.word64Dec
instance ToMessage UTCTime        where msg = Build.stringUtf8 . show
instance ToMessage Float          where msg = msg . toShortest . float2Double
instance ToMessage Double         where msg = msg . toShortest
instance ToMessage Text           where msg = msg . encodeUtf8
instance ToMessage LText.Text     where msg = msg . LText.encodeUtf8
instance ToMessage Char           where msg = msg . Text.singleton
instance ToMessage [Char]         where msg = msg . LText.pack

instance ToMessage a => ToMessage (CI a) where
    msg = msg . CI.foldedCase

instance ToMessage a => ToMessage (Maybe a) where
    msg Nothing  = "Nothing"
    msg (Just x) = "Just " <> msg x

instance ToMessage Bool where
    msg True  = "True"
    msg False = "False"

instance ToMessage Auth where
    msg (Ref t _) = "[Amazonka Auth] { <thread:" <> msg (show t) <> "> }"
    msg (Auth  e) = msg e

instance ToMessage AuthEnv where
    msg AuthEnv{..} = mconcat $ intersperse "\n"
        [ "[Amazonka Auth] {"
        , "  access key     = ****"
        , "  secret key     = ****"
        , "  security token = " <> msg (const "****" <$> _authToken :: Maybe Builder)
        , "  expiry         = " <> msg _authExpiry
        , "}"
        ]

instance ToMessage [Header] where
    msg = mconcat . intersperse "; " . map (\(k, v) -> msg k <> ": " <> msg v)

instance ToMessage HttpVersion where
    msg HttpVersion{..} = "HTTP/" <> msg httpMajor <> msg '.' <> msg httpMinor

instance ToMessage RequestBody where
    msg = \case
        RequestBodyBuilder     n _ -> " <msger:"   <> msg n <> ">"
        RequestBodyStream      n _ -> " <stream:"  <> msg n <> ">"
        RequestBodyStreamChunked _ -> " <chunked>"

        RequestBodyLBS lbs
            | n <= 4096            -> msg lbs
            | otherwise            -> " <lazy:" <> msg n <> ">"
          where
            n = LBS.length lbs

        RequestBodyBS bs
            | n <= 4096            -> msg bs
            | otherwise            -> " <strict:" <> msg n <> ">"
          where
            n = BS.length bs

instance ToMessage Client.Request where
    msg x = mconcat $ intersperse "\n"
        [ "[Client Request] {"
        , "  host              = " <> msg (host            x)
        , "  port              = " <> msg (port            x)
        , "  secure            = " <> msg (secure          x)
        , "  headers           = " <> msg (requestHeaders  x)
        , "  path              = " <> msg (path            x)
        , "  query             = " <> msg (queryString     x)
        , "  method            = " <> msg (method          x)
        , "  redirect count    = " <> msg (redirectCount   x)
        , "  response timeout  = " <> msg (responseTimeout x)
        , "  request version   = " <> msg (requestVersion  x)
        , "}"
        ]

instance ToMessage (Client.Response a) where
    msg x = mconcat $ intersperse "\n"
        [ "[Client Response] {"
        , "  status code    = " <> msg (statusCode      s)
        , "  status message = " <> msg (statusMessage   s)
        , "  version        = " <> msg (responseVersion x)
        , "  headers        = " <> msg (responseHeaders x)
        , "  cookies        = " <> msg (show (responseCookieJar x))
        , "}"
        ]
      where
        s = responseStatus x
