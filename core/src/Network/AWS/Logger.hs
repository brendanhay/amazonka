{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Logger
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
module Network.AWS.Logger
    (
    -- * Logging
      LogLevel  (..)
    , Logger
    , newLogger
    -- ** Functions
    , logError
    , logInfo
    , logDebug
    , logTrace
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
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
import           Network.AWS.Data.ByteString
import           Network.HTTP.Client
import qualified Network.HTTP.Client          as Client
import           System.IO

data LogLevel
    = Trace -- ^ Includes potentially sensitive signing metadata, and non-streaming response bodies.
    | Debug -- ^ Useful debug information + info + error levels.
    | Info  -- ^ Info messages supplied by the user - this level is not emitted by the library.
    | Error -- ^ Error messages.
      deriving (Eq, Ord, Enum, Show)

type Logger = LogLevel -> Builder -> IO ()

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

logError, logInfo, logDebug, logTrace
 :: (MonadIO m, ToBuilder a) => Logger -> a -> m ()
logError f = liftIO . f Error . build
logInfo  f = liftIO . f Info  . build
logDebug f = liftIO . f Debug . build
logTrace f = liftIO . f Trace . build
