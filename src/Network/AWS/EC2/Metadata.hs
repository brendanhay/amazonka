{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.EC2.Metadata
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Metadata (metadata) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Maybe
import           Data.Monoid
import           Network.Http.Client
import qualified System.IO.Streams      as Streams

metadata :: MonadIO m => ByteString -> m ByteString
metadata key = request $ "http://169.254.169.254/latest/meta-data" <> key

request :: MonadIO m => ByteString -> m ByteString
request url = liftIO $
    bracket (establishConnection url) closeConnection $ \conn -> do
        rq <- buildRequest $ http GET url
        sendRequest conn rq emptyBody
        receiveResponse conn $ \_ inp -> fromMaybe "" <$> Streams.read inp
