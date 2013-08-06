{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS
    ( runAWS
    , withRegion
    , send

    , module EC2
    , module Route53
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Maybe
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal
import           Network.Http.Client
import           OpenSSL                  (withOpenSSL)
import           System.Environment
import qualified System.IO.Streams        as Streams

import           Network.AWS.EC2          as EC2
import           Network.AWS.Route53      as Route53

runAWS :: AWS a -> IO a
runAWS aws = withOpenSSL $ discover >>=
    runReaderT (unWrap aws) . Env Nothing

withRegion :: Region -> AWS a -> AWS a
withRegion reg aws = awsAuth <$> ask >>=
    liftIO . runReaderT (unWrap aws) . Env (Just reg)

-- FIXME: XHT -> Aeson
send :: AWSRequest b a => a -> AWS ByteString
send payload = do
    SignedRequest{..} <- request payload >>= sign
    liftIO . bracket (establishConnection rqUrl) closeConnection $ \conn -> do
        sendRequest conn rqRequest $ maybe emptyBody inputStreamBody rqStream
        print rqRequest
        receiveResponse conn $ \_ inp ->
            fromMaybe "" <$> Streams.read inp

--
-- Internal
--

-- FIXME: Should I try to be smart about choosing the IAM role name
-- from the metadata, or require it to be specified?
discover :: MonadIO m => m Auth
discover = liftIO $ do
    me <- fromEnv
    case me of
        Just x  -> return x
        Nothing -> fromMaybe (error msg) <$> fromMetadata
  where
    fromEnv = do
        acc <- pack "ACCESS_KEY_ID"
        sec <- pack "SECRET_ACCESS_KEY"
        return $ Auth <$> acc <*> sec

    pack = fmap (fmap BS.pack) . lookupEnv

    fromMetadata = decode
        . LBS.fromStrict <$> metadata (SecurityCredentials "s3_ro")

    msg = "Failed to get auth information from environment or EC2 metadata"
