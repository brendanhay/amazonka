{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS
    (
    -- * Credentials
      Credentials(..)
    , credentials

    -- * AWS Monadic Context
    , runAWS
    , send
    , within

    -- * Re-exported
    , module Network.AWS.Internal.Types
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal
import           Network.AWS.Internal.Types
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import           System.Environment
import qualified System.IO.Streams          as Streams

data Credentials
    = Keys ByteString ByteString
    | FromEnv ByteString ByteString
    | FromRole ByteString

credentials :: (Applicative m, MonadIO m) => Credentials -> EitherT Error m Auth
credentials cred = fmapError $ case cred of
    Keys acc sec -> right $ Auth acc sec
    FromEnv (BS.unpack -> k1) (BS.unpack -> k2) -> do
        acc <- pack k1
        sec <- pack k2
        (Auth <$> acc <*> sec) ??
            ("Failed to read " ++ k1 ++ ", " ++ k2 ++ " from ENV")
    FromRole role -> do
        m <- LBS.fromStrict <$> metadata (SecurityCredentials role)
        hoistEither $ Aeson.eitherDecode m
  where
    pack = fmap (fmap BS.pack) . scriptIO . lookupEnv

-- | Run an 'AWS' monadic operation.
runAWS :: Auth -> Bool -> AWSContext a -> EitherT Error IO a
runAWS auth debug aws = (fmap join . runAWS' auth debug) `mapEitherT` aws

runAWS' :: Auth -> Bool -> AWS a -> IO (Either Error a)
runAWS' auth debug aws = withOpenSSL . runReaderT (runEitherT $ unWrap aws) $
    Env Nothing auth debug

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Region -> AWS a -> AWS a
within reg = local (\e -> e { awsRegion = Just reg })

-- | Encode, then send an 'AWSRequest' type to its functionally dependent 'AWSService'.
send :: (AWSService s, AWSRequest s a b, AWSResponse s b) => a -> AWSContext b
send payload = do
    sig  <- lift . sign $ request payload
    whenDebug . print $ rqRequest sig
    mres <- receive sig
    res  <- mres ?? "Failed to receive any data"
    whenDebug $ BS.putStrLn res
    hoistEither $ response res
  where
    receive SignedRequest{..} =
        tryAWS . bracket (establishConnection rqUrl) closeConnection $ \c -> do
        b <- maybe (return emptyBody)
            (fmap inputStreamBody . Streams.fromByteString) rqPayload
        sendRequest c rqRequest b
        receiveResponse c $ const Streams.read
