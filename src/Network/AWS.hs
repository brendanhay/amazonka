{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , within
    , send
    , send'

    -- * Re-exported
    , module Network.AWS.Internal.Monad
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
import           Network.AWS.Internal.Monad
import           Network.AWS.Internal.Types
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams

data Credentials
    = FromKeys ByteString ByteString
    | FromRole ByteString

credentials :: (Applicative m, MonadIO m) => Credentials -> EitherT Error m Auth
credentials cred = case cred of
    FromKeys acc sec  -> right $ Auth acc sec
    FromRole role -> do
        m <- LBS.fromStrict <$> metadata (SecurityCredentials role)
        hoistError $ Aeson.eitherDecode m

-- | Run an 'AWS' monadic operation.
runAWS :: Auth -> Bool -> AWSContext a -> EitherT Error IO a
runAWS auth debug aws = (fmap join . runAWS' auth debug) `mapEitherT` aws

runAWS' :: Auth -> Bool -> AWS a -> IO (Either Error a)
runAWS' auth debug aws = withOpenSSL . runReaderT (runEitherT $ unWrap aws) $
    Env Nothing auth debug

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Region -> AWS a -> AWS a
within reg = local (\e -> e { awsRegion = Just reg })

send :: (ToError e, Rq a, Rs a ~ Either e b) => a -> EitherT Error AWS b
send rq = hoistEither . fmapL toError =<< send' rq

send' :: Rq a => a -> EitherT Error AWS (Rs a)
send' rq = do
    sig  <- lift . sign $ request rq
    whenDebug . print $ srqRequest sig
    mres <- receive sig
    res  <- mres ?? "Failed to receive any data"
    whenDebug $ BS.putStrLn res
    hoistEither $ response rq res
  where
    receive SignedRequest{..} =
        tryIO' . bracket (establishConnection srqUrl) closeConnection $ \c -> do
        b <- maybe (return emptyBody)
            (fmap inputStreamBody . Streams.fromByteString) srqPayload
        sendRequest c srqRequest b
        receiveResponse c $ const Streams.read
