{-# LANGUAGE FlexibleContexts  #-}
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
    , runAWS'
    , within
    , next
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

-- | Run an 'AWS' operation.
runAWS :: Auth -> Bool -> AWSContext a -> EitherT Error IO a
runAWS auth debug aws = (fmap join . runAWS' auth debug) `mapEitherT` aws

runAWS' :: Auth -> Bool -> AWS a -> IO (Either Error a)
runAWS' auth debug aws = withOpenSSL . runReaderT (runEitherT $ unWrap aws) $
    Env Nothing auth debug

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Region -> AWSContext a -> AWSContext a
within reg = local (\e -> e { awsRegion = Just reg })

next :: (Rq a, ToError (Er a)) => Rs a -> AWSContext (Maybe (Rs a))
next = maybe (right Nothing) (fmap Just . send) . paginate

send :: (Rq a, ToError (Er a)) => a -> AWSContext (Rs a)
send = (hoistEither . fmapL toError =<<) . send'

send' :: Rq a => a -> AWSContext (Either (Er a) (Rs a))
send' rq = do
    sig <- lift . sign $ request rq
    whenDebug . print $ srqRequest sig
    res <- fromMaybe "" <$> req sig
    whenDebug $ BS.putStrLn res
    hoistEither $ response res
  where
    req SignedRequest{..} = tryIO'
        . bracket (establishConnection srqUrl) closeConnection
        $ \conn -> do
            sendRequest conn srqRequest =<< body srqPayload
            receiveResponse conn $ const Streams.read

    body = maybe (return emptyBody)
        (fmap inputStreamBody . Streams.fromByteString)
